# mikey.bike

If only there were a .bikey TLD.

## building

mikey.bike requires ghc & cabal to build. On OSX, you can use
homebrew:

```sh
brew install cabal-insall
```

Once `cabal` is installed, you can run:

```sh
make build
```

## development

To run a local server on port 8000:

```sh
make preview
```

## deployment

A [Github action][GA] deploys mikey.bike when pull requests are merged.

[GA]: https://github.com/mjhoy/mikey.bike/blob/master/.github/workflows/deploy.yml
