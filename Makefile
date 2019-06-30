.PHONY: clean build preview site-build

all: build

clean:
	cabal new-clean

build:
	cabal new-build

preview:
	cabal new-run mikey-bike -- preview

site-build:
	cabal new-run mikey-bike -- build
