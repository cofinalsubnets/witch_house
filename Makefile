BINNAME := witch_house
GHCFLAGS := -O2 -fno-warn-unused-do-bind -threaded -Wall -Werror

build:
	ghc --make Main.hs -o ${BINNAME} ${GHCFLAGS}

install: gretel
	mv witch_house ~/bin

clean:
	find . -regex ".*\.\(o\|\hi\)" -delete

.PHONY: build install clean

