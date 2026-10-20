#!/usr/bin/env python3
"""Private JSON-lines transport between ipynb.el and Jupyter (Python 3.9+)."""
import argparse
import asyncio
import json
import sys
import threading


def emit(event, **fields):
    print(json.dumps(dict(event=event, **fields), ensure_ascii=False), flush=True)


class Bridge:
    def __init__(self, kernel, connection=None):
        self.kernel = kernel
        self.connection = connection
        self.manager = None
        self.client = None
        self.requests = {}
        self.tasks = []

    async def start(self):
        from jupyter_client import AsyncKernelClient, AsyncKernelManager
        if self.connection:
            self.client = AsyncKernelClient(connection_file=self.connection)
            self.client.load_connection_file()
        else:
            self.manager = AsyncKernelManager(kernel_name=self.kernel)
            # Kernel stdout must never pollute our JSON protocol.
            await self.manager.start_kernel(stdout=sys.stderr, stderr=sys.stderr)
            self.client = self.manager.client()
        self.client.start_channels()
        await self.client.wait_for_ready(timeout=60)
        self.start_pumps()
        emit('ready', kernel=self.kernel, owned=bool(self.manager))

    def start_pumps(self):
        self.tasks = [asyncio.create_task(self.pump(channel))
                      for channel in ('iopub', 'shell', 'stdin')]

    async def stop_pumps(self):
        for task in self.tasks:
            task.cancel()
        await asyncio.gather(*self.tasks, return_exceptions=True)
        self.tasks = []

    async def pump(self, channel):
        getter = getattr(self.client, 'get_' + channel + '_msg')
        while True:
            msg = await getter()
            kind = msg['header']['msg_type']
            parent = msg.get('parent_header', {}).get('msg_id')
            request = self.requests.get(parent, {})
            content = msg['content']
            emit('message', channel=channel, type=kind,
                 id=request.get('id'), cell=request.get('cell'), content=content)
            # Shell replies and IOPub idle can arrive in either order.
            if request.get('op') == 'execute':
                if kind == 'execute_reply':
                    request['_reply'] = True
                if kind == 'status' and content.get('execution_state') == 'idle':
                    request['_idle'] = True
                if request.get('_reply') and request.get('_idle'):
                    emit('finished', id=request['id'])
                    self.requests.pop(parent, None)
            elif channel == 'shell':
                self.requests.pop(parent, None)

    async def command(self, request):
        op = request['op']
        if op == 'execute':
            msg_id = self.client.execute(request['code'], allow_stdin=True,
                                         stop_on_error=False)
            self.requests[msg_id] = request
        elif op in ('inspect', 'complete'):
            msg_id = getattr(self.client, op)(request['code'], request['cursor'])
            self.requests[msg_id] = request
        elif op == 'input':
            self.client.input(request['value'])
        elif op == 'interrupt':
            if not self.manager:
                raise ValueError('Interrupt requires a kernel started by this notebook')
            await self.manager.interrupt_kernel()
        elif op == 'restart':
            if not self.manager:
                raise ValueError('Restart requires a kernel started by this notebook')
            await self.stop_pumps()
            self.requests.clear()
            await self.manager.restart_kernel(now=True)
            await self.client.wait_for_ready(timeout=60)
            self.start_pumps()
            emit('ready', kernel=self.kernel, owned=True)
        elif op == 'shutdown':
            return False
        else:
            raise ValueError('Unknown operation: ' + op)
        return True

    async def close(self):
        await self.stop_pumps()
        if self.client:
            self.client.stop_channels()
        if self.manager:
            await self.manager.shutdown_kernel(now=True)


async def run(args):
    bridge = Bridge(args.kernel, args.connection)
    queue = asyncio.Queue()
    loop = asyncio.get_running_loop()

    def read_commands():
        for line in sys.stdin:
            loop.call_soon_threadsafe(queue.put_nowait, line)
        loop.call_soon_threadsafe(queue.put_nowait, None)

    threading.Thread(target=read_commands, daemon=True).start()
    try:
        await bridge.start()
        while True:
            line = await queue.get()
            if line is None:
                break
            request = {}
            try:
                request = json.loads(line)
                if not await bridge.command(request):
                    break
            except Exception as exc:
                emit('error', id=request.get('id'), message=str(exc))
    finally:
        await bridge.close()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--kernel', default='python3')
    parser.add_argument('--connection')
    parser.add_argument('--list', action='store_true')
    args = parser.parse_args()
    try:
        if args.list:
            from jupyter_client.kernelspec import KernelSpecManager
            print(json.dumps(KernelSpecManager().get_all_specs()))
        else:
            asyncio.run(run(args))
    except Exception as exc:
        emit('error', message=str(exc))
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
