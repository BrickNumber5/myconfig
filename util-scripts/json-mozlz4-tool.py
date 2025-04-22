#!/bin/python3

# Utility script for compressing and decompressing firefox's .json.mozLz4 files
# Mozilla doesn't provide a tool to do this themselves but it isn't to hard
# assuming you have liblz4.so
#
# Dependencies: python3, liblz4.so
# (I believe ctypes, sys, struct, and json all ship with python3 so that's it)
#
# The .mozLz4 format is actually really simple, its just a magic `mozLz40\x00`
# a little endian decompressed size, and then a single standard lz4 block.

# This utlity takes input over stdin and outputs to stdout.
# It takes no parameters.
# It makes the somewhat optomistic assumption that its input is either a valid
# .json.mozlz4 file (mozLz4 compressed json) or a valid .json file.
# If you provide it a file that doesn't meet either of these conditions, e.g.
# an image file, a corrupt mozlz4 file, or a mozlz4 file compressing something
# other than json it will misbehave.

from ctypes import *
import sys
import struct
import json

lz4 = cdll.LoadLibrary("liblz4.so")

MOZLZ4_MAGIC = b'mozLz40\x00'

src = sys.stdin.buffer.read()

if src[0:len(MOZLZ4_MAGIC)] == MOZLZ4_MAGIC:
    # This is a mozlz4 file, decompress it
    src = src[len(MOZLZ4_MAGIC):]
    decompressed_size = struct.unpack("<L", src[0:4])[0]
    src = src[4:]
    
    buffer = create_string_buffer(decompressed_size)
    sanity_check_size = lz4.LZ4_decompress_safe(
        src,
        buffer,
        len(src),
        decompressed_size,
    )
    
    if sanity_check_size != decompressed_size:
        print("ACK! Buffer size doesn't check out")
        print(decompressed_size, sanity_check_size)
        exit(1)
    
    parsed = json.loads(buffer.raw.decode("utf-8"))
    
    print(json.dumps(parsed, indent=4))
else:
    # This is a json file (just waiting to become a mozlz4 file), compress it!
    
    # Make the json as small as possible
    src = bytes(json.dumps(json.loads(src), indent = None, separators = (',', ':')), "utf-8")
    
    compress_bound = lz4.LZ4_compressBound(len(src))
    
    buffer = create_string_buffer(
        MOZLZ4_MAGIC + struct.pack("<L", len(src)),
        len(MOZLZ4_MAGIC) + 4 + compress_bound,
    )
    
    compressed_bytes = lz4.LZ4_compress_default(
        src,
        byref(buffer, len(MOZLZ4_MAGIC) + 4),
        len(src),
        compress_bound
    )
    
    sys.stdout.buffer.write(buffer[0:(len(MOZLZ4_MAGIC) + 4 + compressed_bytes)])
    
