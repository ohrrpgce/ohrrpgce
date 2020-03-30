import struct
import os
from os import path

from hs_ast import AST_state
import hs_gen

hs_fn = None
hs_cache = None

def scripts_bin_enc(_id, trigger, name):
    name = bytes(name, "latin-1")
    return struct.pack('<3H36sH', _id, trigger, len(name), name, 0)

def write_hsz(name, data):

    script = AST_state.scripts[name]

    fn = path.join(hs_cache, "%d.hsz" % (script.id))
    fd = open(fn, "wb")
    fd.write(data)
    fd.close()

def write_lump(fd, fn):

    fdi = open(fn, "rb")
    data = fdi.read()
    fdi.close()

    name = path.basename(fn).upper()

    fd.write(bytes(name, "latin-1"))
    fd.write(bytes((0,)))

    # in PDP-endian format
    fd.write(struct.pack(
        '<2H', len(data) >> 16, len(data)
    ))

    fd.write(data)

def hs_begin(fn):

    global hs_fn, hs_cache

    bn, ext = path.splitext(fn)
    
    hs_fn = bn + ".hs"
    hs_cache = bn + ".cache"

    if not path.isdir(hs_cache):
        os.mkdir(hs_cache)

def hs_end():

    # write hs

    fn = path.join(hs_cache, "hs")
    fd = open(fn, "w")

    fd.write("HamsterSpeak\n")
    fd.write("3Ue\n1\n3U\n3\n")
    fd.write("689\n")

    fd.close()

    # write scripts.txt

    fn = path.join(hs_cache, "scripts.txt")
    fd = open(fn, "w")

    for name in AST_state.scripts:

        script = AST_state.scripts[name]

        fd.write(name + "\n")
        fd.write(str(script.id) + "\n")

        fd.write(str(len(script.args)) + "\n")

        for arg in script.args:
            kind, _id = hs_gen.kind_and_id(arg)
            fd.write(str(_id) + "\n")

    fd.close()

    # write scripts.bin

    fn = path.join(hs_cache, "scripts.bin")
    fd = open(fn, "wb")

    fd.write(struct.pack('<2H', 4, 44))

    for name in AST_state.scripts:

        script = AST_state.scripts[name]

        fd.write(scripts_bin_enc(
            script.id, script.trigger, name
        ))

    fd.close()

    # lump everything together

    fd = open(hs_fn, "wb")

    write_lump(fd, path.join(hs_cache, "hs"))
    write_lump(fd, path.join(hs_cache, "scripts.txt"))
    write_lump(fd, path.join(hs_cache, "scripts.bin"))

    for name in AST_state.scripts:

        script = AST_state.scripts[name]

        sfn = path.join(hs_cache, "%d.hsz" % (script.id))

        # should report an error if a script is missing
        if path.isfile(sfn):
            write_lump(fd, sfn)

    fd.close()
