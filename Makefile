BINNAME := witch_house
GHCFLAGS := -O2 -threaded -fwarn-unused-binds -fwarn-orphans -fwarn-incomplete-record-updates -fwarn-overlapping-patterns -fwarn-unused-imports -Werror # -prof -fprof-auto -rtsopts

build:
	ghc --make Main.hs -o ${BINNAME} ${GHCFLAGS}

install: witch_house
	mv witch_house ~/bin

clean:
	find . -regex ".*\.\(o\|\hi\)" -delete
	rm -f "*.prof"

.PHONY: build install clean

