# Makefile for Advent of Code 2024
# Deletes the contents of the out/ directory if it exists, and recreates it
# Take each day#.c file in src/ and compiles it into an executable in out/

.PHONY: clean

all: clean
	mkdir -p out
	for f in src/*.c; do \
		name=`basename $$f .c`; \
		gcc -o out/$$name $$f; \
	done
