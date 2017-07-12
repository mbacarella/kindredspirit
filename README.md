Kindred Spirit
==============

Lighting control code for the art car Kindred Spirit.

http://ourkindredspirit.org/

Building
---------

You need an OCaml environment and the OPAM package manager.

I install ocaml and opam via my OS's package manager (e.g. apt-get install ocaml opam).

Then do

`opam switch 4.04.2`

You can try other versions of OCaml but I've only tested it with 4.04.2.  Once that finishes try:

`opam install async_extended ppx_bitstring fftw3 lablgl portaudio`

Likely, the above will mostly complete with a few errors regarding missing system packages.

Follow the instructions (printed at the end of the opam install process) for how to use `opam depext` to install the missing dependencies.

Once the dependencies are installed try the above opam install line again.

Now you can try ./build.sh
