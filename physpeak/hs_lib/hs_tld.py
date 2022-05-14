import sys
from os import path
import re
from hs_ast import AST_state
import hs_post
import hs_gen
import hs_file

compiler_dir = path.dirname(sys.argv[0])
#include_dir = ""
# command line options
main_args = None

include_once = None

n_failed_scripts = 0
n_scripts = 0

def find_include_file_globally(include_name):
    fpath = path.join(compiler_dir, include_name)
    if path.isfile(fpath):
        return fpath
    # From hspeak: in case we're installed at $prefix/{games,bin}, try $prefix/share/games/ohrrpgce/
    fpath = path.join(compiler_dir, "../share/games/ohrrpgce/", include_name)
    if path.isfile(fpath):
        return fpath

# From hspeak.exw
def find_include_file(include_name, source_file = None):
    """Find path to a file.
    source_file is the file containing the 'include' or blank if none"""
    include_name = include_name.replace('/', path.sep).replace('\\', path.sep)
    if include_name == "plotscr.hsd" or include_name == "scancode.hsi":
        # Prefer to use fundamental include files found in global locations, because
        # plotscr.hsd and scancode.hsi will be exported too, when exporting scripts
        # from Custom.
        # (Note: differs from hspeak in that we afterwards fall back to local search)
        fpath = find_include_file_globally(include_name)
        if fpath:
            return fpath
    # Try source directory
    if source_file:
        fpath = path.join(path.dirname(source_file), include_name)
        if path.isfile(fpath):
            return fpath
    # Support for unimplemented --incdir option
    # fpath = path.join(include_dir, include_name)
    # if path.isfile(fpath):
    #     return fpath
    # Try current directory (which will be where the .rpg is)
    if path.isfile(include_name):
        return include_name
    # Try global locations
    fpath = find_include_file_globally(include_name)
    if fpath:
        return fpath
    raise FileNotFoundError("Can't find " + include_name)

def parse_hss_2(fn, cpass, source_file = None):
    global n_failed_scripts, n_scripts

    if fn in include_once:
        return

    include_once.add(fn)

    fn = find_include_file(fn, source_file)

    if cpass == 1:
        print("Including", fn)

    for encoding in ('utf-8', 'utf-16', 'latin-1'):
        with open(fn, 'r', encoding = encoding) as fd:
            try:
                lines = fd.readlines()
                break
            except UnicodeError:
                pass

    # current section
    csection = None

    # current script name
    cname = None

    # current script buffer
    cbuffer = []

    # current line number
    cline = 0
    # Line number where the current block begins
    blockstart = 0

    include_re = re.compile('include\s*,\s* ( ([^"#]+) | "([^"]+)" \s* ([^#]*) )', re.I + re.X)

    for line in lines:
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
            parse_hss_2(include_file, cpass, fn)
            continue

        if csection == "script":

            if trimmed_line == "end":

                if cpass == 2:

                    # attempt to compile the script body

                    print("---- script ---- %s ---- %s ---- %d-%d" % (cname, fn, blockstart, cline))

                    if AST_state.build(''.join(cbuffer), blockstart, cname):

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
                cbuffer = []
                continue

            if cpass == 1:
                continue

            cbuffer.append(line)

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
