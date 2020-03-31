import sys
import re
from hs_ast import AST_state
import hs_post
import hs_gen
import hs_file

# command line options
main_args = None

include_once = None

n_failed_scripts = 0
n_scripts = 0

def parse_hss_2(fn, cpass):
    global n_failed_scripts, n_scripts

    if fn in include_once:
        return

    include_once.add(fn)

    if cpass == 1:
        print("Including", fn)

    fd = open(fn, "r")

    # current section
    csection = None

    # current script name
    cname = None

    # current script buffer
    cbuffer = None

    # current line number
    cline = 0
    # Line number where the current block begins
    blockstart = 0

    include_re = re.compile('include\s*,\s* ( ([^"#]+) | "([^"]+)" \s* ([^#]*) )', re.I + re.X)

    for line in fd:
        trimmed_line = line

        # remove comment
        i = trimmed_line.find("#")
        if i > -1:
            trimmed_line = trimmed_line[:i]

        trimmed_line = trimmed_line.strip()
        cline += 1

        match = include_re.match(trimmed_line)
        if match:
            if match.group(4):
                print("Line", cline, "garbage after include filename:", match.group(4))
            include_file = match.group(3) or match.group(2).rstrip()
            parse_hss_2(include_file, cpass)
            continue

        if csection == "script":

            if trimmed_line == "end":

                if cpass == 2:

                    # attempt to compile the script body

                    print("---- script ---- %s ---- %s ---- %d-%d" % (cname, fn, blockstart, cline))

                    if AST_state.build(cbuffer, blockstart, cname):

                        hs_post.AST_post()
                        data = hs_gen.toHSZ(cname)
                        hs_file.write_hsz(cname, data)
                        n_scripts += 1

                        if main_args.d:
                            AST_state._print()

                            print(
                                "toHSZ:", len(data),
                                "locals:", len(AST_state.locals),
                                "strings:", len(AST_state.strings)
                            )

                    else:
                        n_failed_scripts += 1
                        print(AST_state.error)

                csection = None
                cname = None
                cbuffer = None
                continue

            if cpass == 1:
                continue

            if cbuffer:
                cbuffer += "," + line
            else:
                cbuffer = line

            continue

        # Skip empty lines except inside scripts
        if not trimmed_line:
            continue

        if csection == "defineconstant":

            if trimmed_line == "end":
                csection = None
                continue

            if cpass == 2:
                continue

            if not AST_state.build(line, cline):
                print("---> in %s %d" % (fn, cline))
                print(AST_state.error)
                continue

            nodes = AST_state.root.children
            if not nodes:
                continue

            AST_state._constants[nodes[1].leaf] = \
                nodes[0].leaf

            continue

        if csection == "definefunction":

            if trimmed_line == "end":
                csection = None
                continue

            if cpass == 2:
                continue

            if not AST_state.build(line, cline):
                print("---> in %s %d" % (fn, cline))
                print(AST_state.error)
                continue

            nodes = AST_state.root.children
            if not nodes:
                continue

            AST_state.alloc_function(
                nodes[1].leaf, nodes[0].leaf,
                nodes[2].leaf, nodes[3:]
            )

            continue

        if csection == "definetrigger":

            # skip this section

            if trimmed_line == "end":
                csection = None
                continue

            continue

        if csection == "defineoperator":

            # skip this section

            if trimmed_line == "end":
                csection = None
                continue

            continue

        # -- not in any section --

        if not AST_state.build(line, cline):
            print("---> in %s %d" % (fn, cline))
            print(AST_state.error)
            continue

        blockstart = cline + 1  # The text passed to build() begins next line

        nodes = AST_state.root.children
        if not nodes:
            continue

        # list-like section headers outside of scripts
        if nodes[0].type == "value":

            # script and plotscript
            if nodes[0].leaf in AST_state.triggers:

                # register the script
                if cpass == 1:

                    _args = nodes[2:-1]

                    AST_state.alloc_script(
                        nodes[1].leaf,
                        AST_state.triggers[nodes[0].leaf],
                        len(_args), _args
                    )

                # initialize local scope
                elif cpass == 2:

                    AST_state.reset_locals()

                    for node in nodes[2:-1]:
                        AST_state.alloc_local(node.leaf)

                csection = "script"
                cname = nodes[1].leaf
                continue

            if nodes[0].leaf == "defineconstant":
                csection = "defineconstant"
                continue

            if nodes[0].leaf == "definefunction":
                csection = "definefunction"
                continue

            if nodes[0].leaf == "definetrigger":
                csection = "definetrigger"
                continue

            if nodes[0].leaf == "defineoperator":
                csection = "defineoperator"
                continue

            continue

        # function-like definitions outside of scripts
        if cpass == 1 and nodes[0].type == "function":

            args = AST_state.root.children[0].children

            if nodes[0].leaf == "defineconstant":
                AST_state._constants[args[1].leaf] = \
                    args[0].leaf
                continue

            if nodes[0].leaf == "globalvariable":
                AST_state.globals[args[1].leaf] = \
                    args[0].leaf
                continue

            continue

def parse_hss(fn):

    global include_once

    print("Pass 1...", file=sys.stderr)
    include_once = set()
    parse_hss_2(fn, 1)

    print("Pass 2...", file=sys.stderr)
    include_once = set()
    parse_hss_2(fn, 2)

    print("Compiled %d scripts, %d failed" % (n_scripts, n_failed_scripts), file=sys.stderr)
