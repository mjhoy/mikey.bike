.PHONY: clean build preview site-build site-clean

all: build

clean:
	cabal new-clean

build:
	cabal new-build

preview: site-clean
	cabal new-run mikey-bike -- watch

site-build: site-clean
	cabal new-run mikey-bike -- build

site-clean:
	cabal new-run mikey-bike -- clean
