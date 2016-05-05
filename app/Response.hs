{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (fromString)
import System.Environment (getArgs)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Application.Static as Static
import Data.Maybe (fromJust)
import Data.FileEmbed (embedDir)
import WaiAppStatic.Types (toPieces)
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy as LBS
import Data.Monoid ((<>))

main :: IO ()
main = do
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ routerApp


routerApp :: Wai.Application
routerApp req respond
  | (["builder"] == path) = builderApp req respond
  | (["lbs"]     == path) = lbsApp     req respond
  | (["stream"]  == path) = streamApp  req respond
  | (["raw"]     == path) = rawApp     req respond
  | otherwise    = staticApp req respond -- static html/js/css files
  where
    path = Wai.pathInfo req


builderApp :: Wai.Application
builderApp req respond = do
  let b = "Blaze.ByteString.Builder" <> "\n" <> "ビルダー" <> "\n"
  respond $ Wai.responseBuilder H.status200 [("Content-Type","text/plain")] b


lbsApp :: Wai.Application
lbsApp req respond = do
  let s = "Data.ByteString.Lazy" <> "\n" <> (fromStrict $ encodeUtf8 "エルビーエス") <> "\n"
  respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] s


streamApp :: Wai.Application
streamApp req respond = do
  respond $ Wai.responseStream H.status200 [("Content-Type","text/plain")] $ \write flush -> do
    r <- Wai.requestBody req -- empty when nothing
    print r
    
    write "Network.Wai.StreamingBody"
    write "\n"
    flush
    write "ストリーミングボディー"
    write "\n"


rawApp :: Wai.Application
rawApp req respond = do
  respond $ flip Wai.responseRaw backup $ \src sink -> do
    r <- src -- lock while loading
    print r
    
    sink "HTTP/1.1 200 OK\r\n"
    sink "Content-Type: text/plain\r\n"
    sink "\r\n"
    sink "RAW"
    sink "\n"
    sink $ encodeUtf8 "ロー"
    sink "\n"
    
  where
    backup = Wai.responseBuilder H.status500 [("Content-Type","text/plain")] "backup response.."


staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.defaultWebAppSettings "static"
--    settings = Static.embeddedSettings $(embedDir "static")
    indices = fromJust $ toPieces ["response.html"] -- default content

