build:
	dune build walking.exe

run:
	dune exec ./walking.exe

format:
	dune build @fmt --auto-promote