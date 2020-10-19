# incremental-ray-tracer
The code to go with this [blog post](https://www.peterstefek.me/incr-ray-tracer)  

This is a somewhat Incremental Ray Tracer (only incremental when materials change, not when spheres move).  

Tuneable constants (width, height, number of rays per pixel, etc) can be found in `utils/ray_utils.ml`.  

To run this code, you need [opam](https://opam.ocaml.org/) and [dune](https://github.com/ocaml/dune). Then you need to install incremental `opam install incremental`.