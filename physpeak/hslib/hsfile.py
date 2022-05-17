import struct, os
from os import path

from . import gen
from .ast import AST_state

hs_fn = None
hs_cache = None

def write_hsz(name, data):

    script = AST_state.scripts[name]
    fn = "%d.hsz" % (script.id)

    hs_cache[fn] = data

def write_lump(fd, fn):

    data = hs_cache[fn]

    fd.write(bytes(fn.upper(), "latin-1"))
    fd.write(bytes((0,)))

    # in PDP-endian format
    fd.write(struct.pack(
        '<2H', len(data) >> 16, len(data)
    ))

    fd.write(data)

def hs_begin(fn):

    global hs_fn, hs_cache

    hs_fn = fn
    hs_cache = {}

def hs_end():

    # write hs

    data = "HamsterSpeak\n"
    data += "3Ue\n1\n3U\n3\n"
    data += "689\n"

    hs_cache["hs"] = bytes(data, "latin-1")

    # write scripts.txt

    data = ""

    for name in AST_state.scripts:

        script = AST_state.scripts[name]

        data += name + "\n"
        data += str(script.id) + "\n"
        data += str(len(script.args)) + "\n"

        for arg in script.args:
            kind, id = gen.kind_and_id(arg)
            data += str(id) + "\n"

    hs_cache["scripts.txt"] = bytes(data, "latin-1")

    # write scripts.bin

    data = bytes()

    data += struct.pack('<2H', 4, 44)

    for name in AST_state.scripts:

        script = AST_state.scripts[name]
        b_name = bytes(name, "latin-1")

        data += struct.pack(
            '<3H36sH',
            script.id, script.trigger,
            len(b_name), b_name, 0
        )

    hs_cache["scripts.bin"] = data

    # lump everything together

    fn = path.splitext(hs_fn)[0] + ".hs"
    fd = open(fn, "wb")

    write_lump(fd, "hs")
    write_lump(fd, "scripts.txt")
    write_lump(fd, "scripts.bin")

    for name in AST_state.scripts:

        script = AST_state.scripts[name]
        fn = "%d.hsz" % (script.id)

        if fn in hs_cache:
            write_lump(fd, fn)
        else:
            print("hsfile: missing script", fn)

    fd.close()
