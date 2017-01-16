#!/usr/bin/env python3

#-------------------------------------------------------------------------------
# Author: Lukasz Janyst <lukasz@jany.st>
#-------------------------------------------------------------------------------

import io
import sys
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

input  = io.TextIOWrapper(sys.stdin.buffer,  encoding='utf-8')
output = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

while True:
    command = input.readline().strip();
    eprint(command)
    if len(command) == 0:
        eprint("aborted")
        sys.exit(1)

    command = command.split('|')
    if command[0] == 'exit':
        sys.exit(0)
    if len(command) < 3:
        eprint('Invalid input')
        sys.exit(1)

    code = ''
    size = int(command[1])
    while size > 0:
        data = input.read(size)
        ldata = len(data)
        size -= ldata
        code += data

    opts = {}
    opts['noclobber_cssfile'] = True
    forbidden = ['cssfile']
    argmap = {'true': True, 'false': False}

    if len(command) == 4:
        for opt in command[3].split(','):
            opt = opt.split('=')
            if opt[0] in forbidden:
                eprint("ignoring:", opt[0])
            if len(opt) >= 2:
                opts[opt[0]] = argmap.get(opt[1].lower(), opt[1])
            else:
                opts[opt[0]] = True

    lexer = get_lexer_by_name(command[2], stripall = True)
    formatter = HtmlFormatter(**opts)
    highlighted = highlight(code, lexer, formatter)
    output.write('colorized|{}\n'.format(len(highlighted)))
    output.write(highlighted)
    output.flush()
