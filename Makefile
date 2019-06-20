SOURCES=$(shell find . -name '*.rkt' -and -not \( \
					-path './example/*' \
					-or -path './info.rkt' \
					\))

all:
	raco setup musiclibrary

test:
	raco test ${SOURCES}

clean:
	rm -fr $(shell find -type d -name 'compiled')
