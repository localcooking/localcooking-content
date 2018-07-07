module Main where

import Colors (palette)
import User (UserDetails (..), PreUserDetails (..))
import Links (initToDocumentTitle, asyncToDocumentTitle)
import Spec.Topbar.Buttons (topbarButtons)
import Spec.Drawers.Buttons (drawersButtons)
import Spec.Content (content)
import Spec.Content.UserDetails (userDetails)
import Spec.Content.UserDetails.Buttons (userDetailsButtons)
import Spec.Snackbar (messages)
import LocalCooking.Types.ServerToClient (env)
import LocalCooking.Main (defaultMain)
import LocalCooking.Spec.Misc.Network (networkButton)
import LocalCooking.Dependencies.Content (contentDependencies, newContentQueues)
import LocalCooking.Dependencies.Tag (tagDependencies, newTagQueues, mountTagSearchQueues)
import LocalCooking.Global.Links.Internal (ImageLinks (Logo40Png))

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI.Location (toLocation)
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Control.Monad.Aff (sequential)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Execution.Immediate (SET_IMMEDIATE_SHIM)

import React.DOM (text) as R
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.SvgIcon (svgIcon)
import MaterialUI.SvgIcon as SvgIcon
import MaterialUI.Types (createStyles)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import WebSocket (WEBSOCKET)
import Network.HTTP.Affjax (AJAX)
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)
import Queue.Types (readOnly, writeOnly)
import Queue.One as One



-- | All top-level effects
type Effects =
  ( console            :: CONSOLE
  , injectTapEvent     :: INJECT_TAP_EVENT
  , set_immediate_shim :: SET_IMMEDIATE_SHIM
  , ref                :: REF
  , dom                :: DOM
  , timer              :: TIMER
  , uuid               :: GENUUID
  , exception          :: EXCEPTION
  , history            :: HISTORY
  , now                :: NOW
  , ws                 :: WEBSOCKET
  , ajax               :: AJAX
  , webStorage         :: WEB_STORAGE
  , scrypt             :: SCRYPT
  )


main :: Eff Effects Unit
main = do
  log "Starting Local Cooking Editors frontend..."

  contentQueues <- newContentQueues
  tagQueues <- newTagQueues
  siteErrorQueue <- One.newQueue

  tagSearch <- mountTagSearchQueues tagQueues
    { onChefTagSearchResult: \_ -> pure unit
    , onCultureTagSearchResult: \_ -> pure unit
    , onDietTagSearchResult: \_ -> pure unit
    , onFarmTagSearchResult: \_ -> pure unit
    , onIngredientTagSearchResult: \_ -> pure unit
    , onMealTagSearchResult: \_ -> pure unit
    }

  defaultMain
    { env
    , palette
    , deps: do
        contentDependencies contentQueues
        tagDependencies tagQueues
    , extraRedirect: \_ _ -> Nothing
    , extraProcessing: \_ _ -> pure unit
    , initToDocumentTitle
    , asyncToDocumentTitle
    , leftDrawer:
      { buttons: drawersButtons
      }
    , topbar:
      { imageSrc: toLocation Logo40Png
      , buttons: topbarButtons
      }
    , content: content
    , userDetails:
      { buttons: userDetailsButtons
      , content: userDetails
      , obtain: \{user} -> do
        PreUserDetails mUser <- sequential $ PreUserDetails <$> user
        case mUser of
          Just user -> pure $ Just $ UserDetails {user}
          _ -> pure Nothing
      }
    , error:
      { content: messages
      , queue: siteErrorQueue
      }
    , extendedNetwork:
      [ networkButton
        { dark: "#c62828"
        , light: "#ff5f52"
        , href: "https://localcooking.com/"
        , label: "Customers"
        }
      , R.text " "
      , networkButton
        { dark: "#1565c0"
        , light: "#5e92f3"
        , href: "https://chef.localcooking.com/"
        , label: "Chefs"
        }
      , R.text " "
      , networkButton
        { dark: "#1b5e20"
        , light: "#4c8c4a"
        , href: "https://farm.localcooking.com/"
        , label: "Farms"
        }
      , R.text " "
      , networkButton
        { dark: "#7b1fa2"
        , light: "#ae52d4"
        , href: "https://restaurant.localcooking.com/"
        , label: "Restaurants"
        }
      , R.text " "
      , networkButton
        { dark: "#725b53"
        , light: "#d3b8ae"
        , href: "https://blog.localcooking.com/"
        , label: "Blog"
        }
      , R.text " "
      , networkButton
        { dark: "#546e7a"
        , light: "#819ca9"
        , href: "https://admin.localcooking.com/"
        , label: "Admins"
        }
      ]
    }
