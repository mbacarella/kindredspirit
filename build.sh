#!/bin/sh

SRCS=pixel_pusher.ml

ocamlbuild \
    -r \
    -use-ocamlfind \
    -pkg core \
    -pkg core_extended  \
    -pkg async \
    -pkg async_extra \
    -pkg sexplib \
    -pkg sexplib.syntax \
    -pkg bitstring \
    -pkg bitstring.syntax \
    -syntax bitstring.syntax \
    -syntax sexplib.syntax \
    -tag thread \
    -tag debug \
    -tag bin_annot \
    -tag short_paths \
    -cflags "-w A-4-33-40-41-42-43-34-44" \
    -cflags -strict-sequence \
    $SRCS kindredspirit.native
