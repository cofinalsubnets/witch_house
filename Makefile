BINNAME := gretel
GHCFLAGS := -Wall -Werror -O2

build:
	ghc --make Main.hs -o ${BINNAME} ${GHCFLAGS}

install: gretel
	mv gretel ~/bin

clean:
	find . -regex ".*\.\(o\|\hi\)" -delete


.PHONY: build install clean

