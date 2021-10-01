.PHONY: clean build test preview site-build site-clean

all: build

clean:
	cabal new-clean

build:
	cabal new-build

test:
	cabal new-test --test-show-details=streaming

preview: site-clean
	cabal new-run site -- watch

site-build: site-clean
	cabal new-run site -- build

site-clean:
	cabal new-run site -- clean
