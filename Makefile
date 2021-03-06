build:
	dune build

run-incremental:
	dune exec ./incremental_ray_tracer.exe

run-standard:
	dune exec ./standard_ray_tracer.exe

format:
	dune build @fmt --auto-promote