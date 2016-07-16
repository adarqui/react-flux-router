{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

#ifdef __GHCJS__
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
#endif

-- | Ripped almost directly from react-flux examples/routing.
-- Not sure why this isn't in an existing repository by itself.
-- Re-formatted to my liking.

module React.Flux.Router.WebRoutes (
  setLocationHash,
  getLocationHash,
  onLocationHashChange,
  actionRoute,
  childRoutePath,
  initRouter,
  storeRouter
) where



import           React.Flux

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BC (pack)
import           Data.Text              (Text)
import qualified Data.Text              as T (cons, isPrefixOf)
import qualified Web.Routes             as WebRoutes (PathInfo, decodePathInfo,
                                                      mkSitePI, runRouteT,
                                                      runSite, toPathInfo,
                                                      toPathSegments)



#ifdef __GHCJS__

import           Control.Monad          (liftM)
import qualified Data.JSString          as JSS
import           GHCJS.Foreign.Callback
import           GHCJS.Types            (JSString, JSVal)
import           Unsafe.Coerce

#endif



#ifdef __GHCJS__

foreign import javascript unsafe
  "window.onhashchange = function() {$1(location.hash.toString());}"
  js_attachtLocationHashCb :: (Callback (JSVal -> IO ())) -> IO ()

foreign import javascript unsafe
  "window.location.hash = $1"
  js_setLocationHash :: JSString -> IO ()

foreign import javascript unsafe
  "window.location.hash.toString()"
  js_getLocationHash :: IO JSString

setLocationHash :: String -> IO ()
setLocationHash = js_setLocationHash . JSS.pack

getLocationHash :: IO (Maybe String)
getLocationHash = do
  rt <- liftM JSS.unpack js_getLocationHash
  pure $ case rt of
    "" -> Nothing
    _ -> Just rt

onLocationHashChange :: (String -> IO ()) -> IO ()
onLocationHashChange fn = do
  cb <- syncCallback1 ThrowWouldBlock (fn . JSS.unpack . unsafeCoerce)
  js_attachtLocationHashCb cb

# else

setLocationHash :: String -> IO ()
setLocationHash = const $ pure ()

getLocationHash :: IO (Maybe String)
getLocationHash = pure Nothing

onLocationHashChange :: (String -> IO ()) -> IO ()
onLocationHashChange = const $ pure ()

#endif



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
