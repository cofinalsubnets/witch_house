BINNAME := witch_house
GHCFLAGS := -O2 -threaded -fwarn-unused-binds -fwarn-orphans -fwarn-incomplete-record-updates -fwarn-overlapping-patterns -fwarn-unused-imports -fwarn-name-shadowing # -prof -fprof-auto -rtsopts
TESTDIR := ./test

build:
	@ghc --make Main.hs -o ${BINNAME} ${GHCFLAGS}

test: witch_house
	@ls ${TESTDIR} | sed 's|^\(.*\)|\./witch_house -r ${TESTDIR}/\1|' | sh

install: witch_house
	@mv witch_house ~/bin

clean:
	@find . -regex ".*\.\(o\|\hi\)" -delete
	@rm -f "*.prof"

.PHONY: build install clean test

