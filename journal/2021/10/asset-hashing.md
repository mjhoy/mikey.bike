---
title: Asset hashing in Hakyll
date: 2021-10-16T10:00:00-0400
---

This site [runs on the Hakyll static site generator][hakyll] which has
so far been great with one exception: it lacks a system for asset
hashing. The problem is that if I make a change to a css file, if
visitors have cached that asset, they won't see the change if the file
hasn't been renamed. So I found myself manually renaming files like a
heathen -- `base-5.css` to `base-6.css`, etc. -- as well as updating
their references in my templates.

Ideally, this process is automated for me, and instead of incrementing
a number, a hash of the file contents is generated every time I make a
css tweak. Luckily, we are programmers! And we can fix this with the
advanced technique of googling "hakyll asset hashing" and copying from
the first result.

Here's what I ended up doing, using this [thread][thread] as a
starting point.

# Mapping files to their hashes

The core hash function looks like this:

```haskell
{-# LANGUAGE BangPatterns #-}

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16        as Base16
import qualified Data.ByteString.Char8         as BS8

hash :: FilePath -> IO String
hash path = do
  !h <- SHA256.hash <$> BS.readFile path
  pure $! BS8.unpack $! Base16.encode h
```

Now, I'll be honest, I don't really understand what I'm doing with
regard to strictness here; but a) it seems reasonable to avoid
unevaluated thunks of files floating around and b) the thread above
sprinkled in `!` and `$!` so I did too. Oddly, though, they used
`ByteString.Lazy` -- I don't really know why and I switched it to the
strict bytestrings. (Let me know if you know why that's dumb.)

Laziness aside, `hash` gets a digest of a bytestring from a file,
using the [cryptohash-256 library][sha256], and then encodes that in
base64. Easy!

The next idea from that thread is to build a map, keyed from Hakyll
`Identifier`s (which represent items to compile, such as `css` files)
to their hashes:

```haskell
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Hakyll                         ( Identifier
                                                , fromFilePath
                                                , getRecursiveContents
                                                )

type FileHashes = Map Identifier String

mkFileHashes :: FilePath -> IO FileHashes
mkFileHashes dir = do
  allFiles <- getRecursiveContents (\_ -> pure False) dir
  fmap Map.fromList $ forM allFiles $ \innerPath -> do
    let fullPath = dir </> innerPath
    !h <- hash fullPath
    pure (fromFilePath fullPath, h)
```

`getRecursiveContents` is a nice Hakyll helper function that just
walks a directory. We can point it at say an `assets/` directory and
run the hash function for each file.

# Route generation

Given this mapping of identifiers to their hashes, we can produce a
route that interpolates that hash:

```haskell
import           Hakyll                         ( Routes
                                                , customRoute
                                                , fromFilePath
                                                , toFilePath
                                                )
import           System.FilePath                ( (</>) )
import           System.FilePath.Posix          ( takeBaseName
                                                , takeDirectory
                                                , takeExtension
                                                )

assetHashRoute :: FileHashes -> Routes
assetHashRoute fileHashes = customRoute $ \identifier ->
  let maybeHash = Map.lookup identifier fileHashes
      path      = toFilePath identifier
  in  maybe path (addHashToUrl path) maybeHash

addHashToUrl :: FilePath -> String -> String
addHashToUrl path hash =
  let baseName  = takeBaseName path
      extension = takeExtension path
      dir       = takeDirectory path
  in  dir </> baseName <> "-" <> hash <> extension
```

The basic idea is that we can use `assetHashRoute` when producing
routes for some asset directory, and Hakyll will rewrite their routes
with the `addHashToUrl` function.

Here's how I do it with my various asset directories:

```haskell
import           Hakyll

-- Top-level site generation function.
site :: IO ()
site = do
  hakyll $ do
    -- Generate the mappings
    imageHashes <- preprocess (mkFileHashes "images")
    cssHashes   <- preprocess (mkFileHashes "stylesheets")
    jsHashes    <- preprocess (mkFileHashes "js")

    match "images/**/*" $ do
      route $ assetHashRoute imageHashes
      compile copyFileCompiler

    match "js/**/*" $ do
      route idRoute
      compile copyFileCompiler

    match "stylesheets/*.css" $ do
      route $ assetHashRoute cssHashes
      compile copyFileCompiler
```

# Rewriting URLs

So, we've moved assets to the correct paths, but how do we link to
them? Once again the thread suggested an implementation, which feels a
_little_ hacky to me, but I'm already in too deep and don't have any
better ideas. Basically, we can run a url-rewriting function over the
content of our html files, so that if it finds a link to `base.css` it
rewrites as `base-[HASH].css`. Hakyll gives us a helper functions to
make this a lot easier, `withUrls`.

```haskell
import           Hakyll                         ( Compiler
                                                , Item(itemIdentifier)
                                                , fromFilePath
                                                , getRoute
                                                , withUrls
                                                )

rewriteAssetUrls :: FileHashes -> Item String -> Compiler (Item String)
rewriteAssetUrls hashes item = do
  route <- getRoute $ itemIdentifier item
  pure $ case route of
    Nothing -> item
    Just r  -> fmap (rewriteAssetUrls' hashes) item

rewriteAssetUrls' :: FileHashes -> String -> String
rewriteAssetUrls' hashes = withUrls rewrite
  where rewrite url = maybe url
                            (addHashToUrl url)
                            (lookupHashForUrl hashes url)

lookupHashForUrl :: FileHashes -> String -> Maybe String
lookupHashForUrl hashes url = 
  let urlWithoutRootSlash = dropWhile (== '/') url
  in Map.lookup (fromFilePath urlWithoutRootSlash) hashes
```

`rewriteAssetUrls` delegates to `rewriteAssetUrls'` which takes in a
string of HTML, and produces a rewritten string given our `FileHashes`
map. We filter out any Hakyll item that doesn't have a route because
-- well, because that's how it was implemented in the thread, and I
assume that's to avoid "ephemeral" items like templates.

We can add this function to our compilers like so:

```haskell
-- Combine all asset hash maps that we generated above
let assetHashes = imageHashes <> cssHashes <> jsHashes

match "index.html" $ do
  route idRoute
  compile
    $   getResourceBody
    >>= loadAndApplyTemplate "templates/layout.html" defaultContext
    >>= rewriteAssetUrls assetHashes
```

And so on.

Now, if you view source on this page (if you dare), you'll see some
_lovely_ hashed asset URLs. Just lovely. Was SHA256 overkill?
Probably.

If you want to see the actual implementation, it [lives here][assethashing].

[hakyll]: /j/2021/01/17-this-site.html
[thread]: https://groups.google.com/g/hakyll/c/zdkQlDsj9lQ
[sha256]: https://hackage.haskell.org/package/cryptohash-sha256-0.11.102.1/docs/Crypto-Hash-SHA256.html#g:3
[assethashing]: https://github.com/mjhoy/mikey.bike/blob/19aef0201224aad616a4c5977b10e930451dc17f/src/AssetHashing.hs
