#-------------------------------------------------------------------------------
# Author: Lukasz Janyst <lukasz@jany.st>
#-------------------------------------------------------------------------------

import io
import sys
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter
from pygments.util import ClassNotFound

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

    if command[0] != 'pygmentize':
        eprint('Unknown command:', command[0])
        continue

    if len(command) < 3:
        eprint('Invalid input')
        sys.exit(1)

    try:
        size = int(command[1])
    except ValueError:
        eprint('Second parameter needs to be an integer, got:', command[1])
        sys.exit(1)

    code = ''
    while size > 0:
        data = input.read(size)
        ldata = len(data)
        size -= ldata
        code += data

    try:
        lexer = get_lexer_by_name(command[2], stripall=True)
    except ClassNotFound:
        eprint('Cannot find lexer for {}, using "text" instead'.format(command[2]))
        lexer = get_lexer_by_name('text', stripall=True)

    opts = {}
    params = {
        'nowrap': bool,
        'style': str,
        'noclasses': bool,
        'classprefix': str,
        'cssclass': str,
        'cssstyles': str,
        'prestyles': str,
        'linenos': str,
        'linenostart': int,
        'linenostep': int,
        'linenoseparator': str,
        'lineanchors': str,
        'linespans': str,
        'anchorlinenos': bool
        }
    argmap = {'true': True, 'false': False}

    if len(command) == 4:
        for opt in command[3].split(','):
            opt = opt.split('=')
            if opt[0] not in params.keys():
                eprint('ignoring:', opt[0])
                continue
            if len(opt) >= 2:
                if params[opt[0]] == bool:
                    opts[opt[0]] = argmap.get(opt[1].lower(), True)
                elif params[opt[0]] == int:
                    try:
                        opts[opt[0]] = int(opt[1])
                    except ValueError:
                        eprint('Ignoring {}, expected an integer, got {}' \
                               .format(opt[0], opt[1]))
                else:
                    opts[opt[0]] = opt[1]
            else:
                opts[opt[0]] = True

    formatter = HtmlFormatter(**opts)
    highlighted = highlight(code, lexer, formatter)
    output.write('colorized|{}\n'.format(len(highlighted)))
    output.write(highlighted)
    output.flush()
