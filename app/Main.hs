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
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy as LBS


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
  | (["posixtime"] == path) = dateApp   req respond
  | otherwise               = staticApp req respond -- static html/js/css files
  where
    path = Wai.pathInfo req


dateApp :: Wai.Application
dateApp req respond = do
  pt_milliseconds <- getPOSIXTime'
  let pt_lbs = LBS.fromStrict $ encodeUtf8 $ T.pack $ show $ pt_milliseconds
  respond $ Wai.responseLBS H.status200 [("Content-Type","text/plain")] pt_lbs


staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
--    settings = Static.defaultWebAppSettings "static"
    settings = Static.embeddedSettings $(embedDir "static")
    indices = fromJust $ toPieces ["main.html"] -- default content


getPOSIXTime' :: IO Int
getPOSIXTime' = do
  pt_seconds <- getPOSIXTime
  return $ truncate $ pt_seconds * 1000
  
