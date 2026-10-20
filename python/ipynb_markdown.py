#!/usr/bin/env python3
"""Render notebook Markdown to inert HTML for Emacs SHR (no network access)."""
import sys


def render(source):
    from markdown_it import MarkdownIt
    parser = MarkdownIt('commonmark', {'html': False, 'breaks': False})
    parser.enable(['table', 'strikethrough'])
    return '<html><body>' + parser.render(source) + '</body></html>'


if __name__ == '__main__':
    try:
        sys.stdout.write(render(sys.stdin.read()))
    except ImportError:
        sys.stderr.write('Markdown previews require markdown-it-py in ipynb-python-command.\n')
        sys.exit(1)
