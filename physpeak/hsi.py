#!/usr/bin/env python3

import os
from os import path
import sys

sys.path.insert(0,
    path.join(path.dirname(sys.argv[0]), "hs_lib")
)

from hs_ast import AST_state

# --

if __name__ == "__main__":

    # Use readline with a history
    try:
        import readline  # Not available in all Python builds
        histfile = ".hspeak_history"
        try:
            readline.read_history_file(histfile)
            readline.set_history_length(100)
        except IOError:
            pass
        import atexit
        atexit.register(readline.write_history_file, histfile)
    except ImportError:
        pass

    import logging
    log = None

    AST_state.reset_locals()

    while True:

        # get a line of text and try to parse it

        try:
            s1 = input('HSpeak> ')
        except EOFError:
            print()
            break

        if not s1:
            continue

        if s1.startswith("@debug"):
            if "help" in s1:
                print("Print yacc debug message. Usage: @debug [error|info|full|off|help] (default to info)")
                continue
            if not log:
                logging.basicConfig()
                log = logging.getLogger()
            if "error" in s1:
                log.setLevel(logging.ERROR)
            elif "full" in s1:
                log.setLevel(logging.DEBUG)
            elif "off" in s1:
                log = None
                print("Logging disabled")
                continue
            else: #if "info" in s1:
                log.setLevel(logging.INFO)
            print("Logging level set to", logging.getLevelName(log.level))
            continue

        rv = AST_state.build(s1, 1, debuglog = log)

        # if the parser reported an error at the end of the line
        # then add another line and see if things improve

        while AST_state.error == "continue":

            try:
                s2 = input(' .... > ')
            except EOFError:
                print()
                break

            if not s2:
                break

            # a newline implictily adds a ','
            s1 += ',\n' + s2

            rv = AST_state.build(s1, debuglog = log)

        if rv:
            AST_state._print()
        else:
            print(AST_state.error)
