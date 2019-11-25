# Simple Makefile for uc

.PHONY : all install build

all :
	stack build

install :
	stack build --copy-bins

watch :
	stack build --file-watch

