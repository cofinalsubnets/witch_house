BINNAME := gretel
GHCFLAGS := -Wall -Werror -O2

build:
	ghc --make Main.hs -o ${BINNAME} ${GHCFLAGS}
clean:
	find . -regex ".*\.\(o\|\hi\)" -delete
#	zsh -c "rm -f **/*.{hi,o}"

.PHONY: build clean

