{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , QuasiQuotes
  , DataKinds
  #-}

module Server where

import Server.HTTP (httpServer)
import Server.Assets (favicons, frontend, frontendMin)
import Links (SiteLinks)

import LocalCooking.Server (LocalCookingArgs (..))
import LocalCooking.Colors (LocalCookingColors (..))
import LocalCooking.Dependencies.Content (contentDependencies)
import LocalCooking.Dependencies.Tag (tagDependencies)

import Text.Lucius (Color (..))


server :: LocalCookingArgs SiteLinks sec
server = LocalCookingArgs
  { localCookingArgsFrontend = frontend
  , localCookingArgsFrontendMin = frontendMin
  , localCookingArgsFavicons = favicons
  , localCookingArgsHTTP = httpServer
  , localCookingArgsDeps = do
      contentDependencies
      tagDependencies
  , localCookingArgsColors = LocalCookingColors
    { localCookingColorsMain = Color 200 135 25
    , localCookingColorsActive = Color 255 183 77
    , localCookingColorsHover = Color 255 233 125
    }
  }
