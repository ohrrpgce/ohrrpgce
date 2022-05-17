#!/usr/bin/env python3

import os
from os import path
import sys

import hslib.gen
import hslib.post
from hslib.ast import AST_state
from hslib.parser import AST_build

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
    print_what = "post"

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
                log.setLevel(999)
                log = None
                print("Logging disabled")
                continue
            else: #if "info" in s1:
                log.setLevel(logging.INFO)
            print("Logging level set to", logging.getLevelName(log.level))
            continue

        if s1.startswith("@print"):
            what = s1[6:].strip()
            if what not in ('pre', 'post', 'hsz', 'off'):
                print("Select whether to print parse, post-process or compile result, or nothing. Initially 'post'.\n"
                      "Usage: @print pre|post|hsz|off|help")
            else:
                print("Print:", what)
                print_what = what
            continue

        rv = AST_build(s1, 1, debuglog = log)

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

            s1 += '\n' + s2

            rv = AST_build(s1, debuglog = log, do_post = False)

        if rv:
            if print_what == 'pre':
                print(AST_state)
                continue
            hslib.post.AST_post()
            if print_what == 'post':
                print(AST_state)
                continue
            if print_what == 'hsz':

                _args = []
                AST_state.alloc_script(
                    "REPL",
                    AST_state.triggers["script"],
                    len(_args), _args
                )

                data = hslib.gen.toHSZ("REPL", debug = True)

        else:
            print(AST_state.error)
