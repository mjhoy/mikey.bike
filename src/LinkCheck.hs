module LinkCheck (checkLinks, target, cssUrls, hasScheme, percentDecode) where

import Control.Monad (filterM, forM)
import qualified Data.ByteString as B
import Data.Char (digitToInt, isAlpha, isAlphaNum, isHexDigit, isSpace, toLower)
import Data.List (isPrefixOf, nub, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Hakyll (getRecursiveContents)
import Hakyll.Web.Html (getUrls)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import Text.HTML.TagSoup (parseTags)

checkLinks :: FilePath -> IO Bool
checkLinks site = do
  allFiles <- getRecursiveContents (\_ -> return False) site
  found <- forM allFiles $ \file -> case urlExtractor file of
    Nothing -> pure []
    Just extract -> do
      let fullFilePath = site </> file
      contents <- readUtf8 fullFilePath
      check site fullFilePath (extract contents)
  let missing = sort $ nub $ concat found
  if null missing
    then putStrLn ("check-links: no broken links in " ++ site)
    else do
      hPutStrLn stderr ("check-links: broken links in " ++ site ++ ":")
      mapM_ (hPutStrLn stderr . ("  " ++)) missing
      hPutStrLn stderr ("check-links: " ++ show (length missing) ++ " broken link(s)")
  return (null missing)

urlExtractor :: FilePath -> Maybe (String -> [String])
urlExtractor file = case takeExtension file of
  ".html" -> Just (getUrls . parseTags)
  ".css" -> Just cssUrls
  _ -> Nothing

check :: FilePath -> FilePath -> [String] -> IO [String]
check site page urls = do
  let targets = mapMaybe (\url -> (,) url <$> target site page url) urls
  bad <- filterM (fmap not . exists . snd) targets
  return [page ++ ": " ++ url | (url, _) <- bad]
 where
  exists p = do
    isDir <- doesDirectoryExist p
    doesFileExist (if isDir then p </> "index.html" else p)

readUtf8 :: FilePath -> IO String
readUtf8 = fmap (T.unpack . T.decodeUtf8With T.lenientDecode) . B.readFile

cssUrls :: String -> [String]
cssUrls [] = []
cssUrls s@(_ : rest)
  | map toLower (take 4 s) == "url(" =
      let (body, after) = break (== ')') (drop 4 s)
       in trim (unquote (trim body)) : cssUrls after
  | otherwise = cssUrls rest
 where
  trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  unquote = filter (`notElem` "\"'")

target :: FilePath -> FilePath -> String -> Maybe FilePath
target site page url
  | hasScheme url || "//" `isPrefixOf` url || null path = Nothing
  | "/" `isPrefixOf` path = Just (site </> dropWhile (== '/') path)
  | otherwise = Just (takeDirectory page </> path)
 where
  path = percentDecode (takeWhile (`notElem` "?#") url)

hasScheme :: String -> Bool
hasScheme url = case break (== ':') url of
  (c : cs, ':' : _) -> isAlpha c && all (\x -> isAlphaNum x || x `elem` "+-.") cs
  _ -> False

percentDecode :: String -> String
percentDecode = T.unpack . T.decodeUtf8With T.lenientDecode . B.pack . go
 where
  go ('%' : a : b : r) | isHexDigit a && isHexDigit b = fromIntegral (digitToInt a * 16 + digitToInt b) : go r
  go (c : r) = B.unpack (T.encodeUtf8 (T.singleton c)) ++ go r
  go [] = []
