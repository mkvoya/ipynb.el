"""End-to-end JSON transport tests against a real IPython kernel."""
import json
import os
from pathlib import Path
import queue
import subprocess
import sys
import tempfile
import threading
import time
import unittest

BRIDGE = Path(__file__).resolve().parents[1] / 'python' / 'ipynb_bridge.py'


class KernelTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        env = dict(os.environ, JUPYTER_RUNTIME_DIR=self.tmp.name,
                   IPYTHONDIR=self.tmp.name, JUPYTER_PLATFORM_DIRS='1')
        self.stderr = tempfile.TemporaryFile(mode='w+')
        self.process = subprocess.Popen(
            [sys.executable, '-u', str(BRIDGE)], stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=self.stderr, text=True, env=env)
        self.events = queue.Queue()
        def reader():
            for line in self.process.stdout:
                self.events.put(json.loads(line))
        self.reader = threading.Thread(target=reader, daemon=True)
        self.reader.start()
        self.until(lambda e: e['event'] == 'ready')

    def tearDown(self):
        if self.process.poll() is None:
            self.send(op='shutdown')
            self.process.wait(timeout=20)
        self.process.stdin.close()
        self.process.stdout.close()
        self.stderr.close()
        self.tmp.cleanup()

    def send(self, **request):
        self.process.stdin.write(json.dumps(request) + '\n')
        self.process.stdin.flush()

    def until(self, predicate, timeout=30):
        result = []
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            try:
                event = self.events.get(timeout=0.1)
            except queue.Empty:
                if self.process.poll() is not None:
                    self.stderr.seek(0)
                    self.fail('Bridge exited: ' + self.stderr.read())
                continue
            result.append(event)
            if event['event'] == 'error':
                self.fail(str(event))
            if predicate(event):
                return result
        self.fail('Timed out: ' + repr(result))

    def execute(self, code, id='test'):
        self.send(op='execute', id=id, cell='cell', code=code)
        return self.until(lambda e: e['event'] == 'finished' and e['id'] == id)

    def test_execution_rich_output_error_completion_inspection(self):
        events = self.execute("value = 41\nprint('世界')\nvalue + 1")
        messages = [e for e in events if e['event'] == 'message']
        self.assertTrue(any(e['type'] == 'stream' and '世界' in e['content']['text'] for e in messages))
        self.assertTrue(any(e['type'] == 'execute_result' and e['content']['data']['text/plain'] == '42' for e in messages))
        self.assertTrue(all(e['id'] == 'test' for e in messages if e['type'] in ('stream', 'execute_result')))
        events = self.execute("from IPython.display import display\nd = display('old', display_id=True)\nd.update('new')")
        self.assertTrue(any(e.get('type') == 'update_display_data' for e in events))
        events = self.execute('1 / 0')
        self.assertTrue(any(e.get('type') == 'error' and e['content']['ename'] == 'ZeroDivisionError' for e in events))
        self.send(op='complete', id='completion', code='val', cursor=3)
        events = self.until(lambda e: e.get('type') == 'complete_reply')
        self.assertIn('value', events[-1]['content']['matches'])
        self.assertEqual(events[-1]['id'], 'completion')
        self.send(op='inspect', id='inspect', code='value', cursor=5)
        events = self.until(lambda e: e.get('type') == 'inspect_reply')
        self.assertTrue(events[-1]['content']['found'])
        self.assertEqual(events[-1]['id'], 'inspect')

    def test_stdin_interrupt_restart_and_queue(self):
        self.send(op='execute', id='input', cell='cell', code="name = input('Name: '); print(name)")
        self.until(lambda e: e.get('type') == 'input_request')
        self.send(op='input', value='Ada')
        events = self.until(lambda e: e['event'] == 'finished' and e['id'] == 'input')
        self.assertTrue(any(e.get('type') == 'stream' and 'Ada' in e['content']['text'] for e in events))
        self.send(op='execute', id='loop', cell='cell', code='import time; time.sleep(60)')
        self.until(lambda e: e.get('type') == 'execute_input' and e.get('id') == 'loop')
        self.send(op='interrupt')
        events = self.until(lambda e: e['event'] == 'finished' and e['id'] == 'loop')
        self.assertTrue(any(e.get('type') == 'error' and e['content']['ename'] == 'KeyboardInterrupt' for e in events))
        self.send(op='restart')
        self.until(lambda e: e['event'] == 'ready')
        events = self.execute("'name' in globals()")
        self.assertTrue(any(e.get('type') == 'execute_result' and e['content']['data']['text/plain'] == 'False' for e in events))
        self.send(op='execute', id='one', cell='one', code='x = 21')
        self.send(op='execute', id='two', cell='two', code='x * 2')
        events = self.until(lambda e: e['event'] == 'finished' and e['id'] == 'two')
        self.assertTrue(any(e.get('type') == 'execute_result' and e.get('id') == 'two' and e['content']['data']['text/plain'] == '42' for e in events))


if __name__ == '__main__':
    unittest.main()
