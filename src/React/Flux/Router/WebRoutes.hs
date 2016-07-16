{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Ripped almost directly from react-flux examples/routing.
-- Not sure why this isn't in an existing repository by itself.
-- Re-formatted to my liking.

module React.Flux.Router.WebRoutes (
  actionRoute,
  childRoutePath,
  initRouter,
  storeRouter
) where



import           GHCJS.Router.Base
import           React.Flux

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BC (pack)
import           Data.Text              (Text)
import qualified Data.Text              as T (cons, isPrefixOf)
import qualified Web.Routes             as WebRoutes (PathInfo, decodePathInfo,
                                                      mkSitePI, runRouteT,
                                                      runSite, toPathInfo,
                                                      toPathSegments)



childRoutePath :: WebRoutes.PathInfo action => action -> [Text]
childRoutePath = WebRoutes.toPathSegments



actionRoute :: WebRoutes.PathInfo action => Maybe ([Text] -> Text) -> action -> Text
actionRoute mparentRouter action =
  frag
  where
    path = maybe (WebRoutes.toPathInfo action) ($ childRoutePath action) mparentRouter
    frag = if "#" `T.isPrefixOf` path
           then path
           else T.cons '#' path



initRouter :: ([Text] -> IO ()) -> IO ()
initRouter router =
  onLocationHashChange $ router . stripHash . WebRoutes.decodePathInfo . BC.pack
  where
    stripHash ("#":path) = path
    stripHash path = path



storeRouter :: (StoreData store, WebRoutes.PathInfo (StoreAction store)) =>
               ReactStore store -> ([Text] -> IO ())
storeRouter store =
  let site = WebRoutes.mkSitePI $ WebRoutes.runRouteT $ routerAlterStore
  in
    \rt -> either (const $ pure ()) id $ WebRoutes.runSite "" site rt
  where
    routerAlterStore action =
      liftIO $ alterStore store action
