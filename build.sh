#!/bin/sh

ocamlbuild \
    -use-ocamlfind \
    -pkg core \
    -pkg async \
    -pkg lablgl \
    -pkg lablgl.glut \
    -pkg ppx_bitstring \
    -pkg bitstring \
    -pkg fftw3 \
    -pkg portaudio \
    -tag "ppx(~/.opam/4.04.2/lib/ppx_bitstring/ppx_bitstring.ext)" \
    -tag "ppx(ppx-jane -as-ppx)" \
    -tag thread \
    -tag debug \
    -tag bin_annot \
    -tag short_paths \
    -cflags "-w A-4-33-40-41-42-43-34-44" \
    -cflags -strict-sequence \
    kindredspirit.native ppctl.native beat_detection_helper.native
