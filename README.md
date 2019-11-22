Kindred Spirit
==============

Lighting control code for the art car Kindred Spirit.

Video of the lighting in action: https://www.youtube.com/watch?v=26bCym6FoNs

Building
---------

You need an OCaml environment and the OPAM package manager.

To set up the dependencies do:

```
opam switch 4.09.0
opam install --deps-only .
```

You can leave the switch command out if you want to try your luck.

You'll probably need to add an entry for your laptop to `configs` too, and edit
the start.sh script, which includes a build invocation.


Compatibility
-------------

Successfully builds and runs on Linux x86_64 OCaml 4.09.0, Ubuntu and Debian.
