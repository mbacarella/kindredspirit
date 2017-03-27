#!/bin/bash

#          -w A-4-33-40-41-42-43-34-4 \

ocamlfind opt \
          -thread \
          -short-paths \
          -strict-sequence \
          -bin-annot \
          -package ppx_fields_conv \
          -package ppx_sexp_conv \
          -package core \
          -package async \
          -package lablgl \
          -package lablgl.glut \
          -package bitstring \
          -package ppx_bitstring \
          -linkpkg \
          -o kindredspirit.native \
          color.mli color.ml \
          pixel_pusher.mli pixel_pusher.ml \
          color_picker.mli color_picker.ml \
          coordinate.mli coordinate.ml \
          virtual_pixel.mli virtual_pixel.ml \
          model.mli model.ml \
          animation.mli animation.ml \
          kindredspirit.ml
