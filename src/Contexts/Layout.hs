{-# LANGUAGE NamedFieldPuns #-}

module Contexts.Layout (Layout (..), layoutCtx) where

import Data.Maybe (fromMaybe)
import Hakyll hiding (route)

data Layout = Layout
  { title :: Maybe String
  , rssFeed :: Maybe String
  }

layoutCtx :: Layout -> Context String
layoutCtx (Layout{title, rssFeed}) = activeUrl <> maybeField "title" title <> maybeField "rssFeed" rssFeed <> defaultContext
 where
  maybeField name (Just t) = constField name t
  maybeField _ Nothing = mempty
  activeUrl = functionField "activeUrl" $ \args _ -> do
    let arg = head args
    route <- getUnderlying >>= getRoute
    let currentUrl = fromMaybe "" route
    pure $ if currentUrl == arg then "active" else ""
