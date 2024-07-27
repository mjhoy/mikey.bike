# mikey.bike

Static site generator for https://mikey.bike, using the [Hakyll][hakyll] library.

## building

mikey.bike requires ghc & cabal to build. I suggest using `ghcup` to
get these. Go follow the instructions at https://www.haskell.org/ghcup/.

Now install and use GHC 9.4.8:

```sh
ghcup install ghc 9.4.8
ghcup set ghc 9.4.8
```

Now, you should be able to build the project. The first time will take
a long time (mostly because of pandoc).

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

[hakyll]: https://github.com/jaspervdj/hakyll
[GA]: https://github.com/mjhoy/mikey.bike/blob/main/.github/workflows/deploy.yml
