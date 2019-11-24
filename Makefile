# Simple Makefile for uc

all :
	stack build

install :
	stack build --copy-bins

watch :
	stack build --file-watch

