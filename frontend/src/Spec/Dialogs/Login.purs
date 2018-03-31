module Spec.Dialogs.Login where

import Types.Env (env)

import Window (WindowSize (..), initialWindowSize)
import Links (SiteLinks (..), ThirdPartyLoginReturnLinks (..), toLocation, initSiteLinks)
import Facebook.Call (FacebookLoginLink (..), facebookLoginLinkToURI)
import Facebook.State (FacebookLoginState (..))
import LocalCooking.Common.Password (HashedPassword, hashPassword)

import Prelude
import Data.Maybe (Maybe (..))
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Time.Duration (Milliseconds (..))
import Text.Email.Validate (EmailAddress, emailAddress)
import Control.Monad.Base (liftBase)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue
import React.Signal.WhileMounted as Signal
import React.Icons (facebookIcon, twitterIcon, googleIcon)

import MaterialUI.Types (createStyles)
import MaterialUI.Toolbar (toolbar)
import MaterialUI.Dialog (dialog)
import MaterialUI.Dialog as Dialog
import MaterialUI.DialogContent (dialogContent)
import MaterialUI.DialogTitle (dialogTitle)
import MaterialUI.DialogActions (dialogActions)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.TextField (textField)
import MaterialUI.TextField as TextField
import MaterialUI.Input as Input
import MaterialUI.CircularProgress (circularProgress)
import Crypto.Scrypt (SCRYPT)

import Queue.One (READ, Queue)
import IxSignal.Internal (IxSignal)
import Unsafe.Coerce (unsafeCoerce)



type State =
  { open :: Boolean
  , windowSize :: WindowSize
  , currentPage :: SiteLinks
  , email :: String
  , emailDirty :: Maybe Boolean
  , password :: String
  , pending :: Boolean
  }


initialState :: State
initialState =
  { open: false
  , windowSize: unsafePerformEff initialWindowSize
  , currentPage: initSiteLinks
  , email: ""
  , emailDirty: Nothing
  , password: ""
  , pending: false
  }


data Action
  = Open
  | Close
  | ChangedWindowSize WindowSize
  | ChangedPage SiteLinks
  | ChangedEmail String
  | EmailUnfocused
  | ChangedPassword String
  | SubmitLogin

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , console   :: CONSOLE
  | eff)


spec :: forall eff
      . { toURI :: Location -> URI
        , login :: EmailAddress -> HashedPassword -> Aff (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec {toURI,login} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Open -> void $ T.cotransform _ { open = true }
      Close -> do
        void $ T.cotransform _ { open = false, pending = false }
        liftBase $ delay $ Milliseconds 2000.0
        void $ T.cotransform _ { email = "", password = "" }
      ChangedWindowSize w -> void $ T.cotransform _ { windowSize = w }
      ChangedPage p -> void $ T.cotransform _ { currentPage = p }
      ChangedEmail e -> void $ T.cotransform _ { email = e, emailDirty = Just false }
      EmailUnfocused -> void $ T.cotransform _ { emailDirty = Just true }
      ChangedPassword p -> void $ T.cotransform _ { password = p }
      SubmitLogin -> do
        void $ T.cotransform _ { pending = true }
        case emailAddress state.email of
          Nothing -> liftEff $ log "bad email!" -- FIXME bug out somehow?
          Just email -> do
            liftBase $ do
              liftEff $ log "hashing password"
              hashedPassword <- hashPassword {salt: env.salt, password: state.password}
              liftEff $ log "hashed password"
              login email hashedPassword
              liftEff $ log "login sent"
            performAction Close props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ let dialog' =
              if state.windowSize < Laptop
              then
                dialog
                  { open: state.open
                  , fullScreen: true
                  }
              else
                dialog
                  { open: state.open
                  , fullWidth: true
                  , onClose: mkEffFn1 \_ ->
                      when (not state.pending) (dispatch Close)
                  }
        in  dialog'
            [ dialogTitle {} [R.text "Login"]
            , dialogContent {style: createStyles {position: "relative"}}
              [ textField
                { label: R.text "Email"
                , fullWidth: true
                , onChange: mkEffFn1 \e -> dispatch $ ChangedEmail (unsafeCoerce e).target.value
                , onBlur: mkEffFn1 \_ -> dispatch EmailUnfocused
                , error: case emailAddress state.email of
                  Nothing -> state.emailDirty == Just true
                  Just _ -> false
                }
              , textField
                { label: R.text "Password"
                , fullWidth: true
                , "type": Input.passwordType
                , onChange: mkEffFn1 \p -> dispatch $ ChangedPassword (unsafeCoerce p).target.value
                }
              , R.div [RP.style {display: "flex", justifyContent: "space-evenly", paddingTop: "2em"}] $
                  let mkFab mainColor darkColor icon mLink =
                        Button.withStyles
                          (\theme ->
                            { root: createStyles
                              { backgroundColor: mainColor
                              , color: "#ffffff"
                              , "&:hover": {backgroundColor: darkColor}
                              }
                            }
                          )
                          (\{classes} ->
                            button
                              { variant: Button.fab
                              , classes: Button.createClasses {root: classes.root}
                              , disabled: case mLink of
                                Nothing -> true
                                _ -> false
                              , href: case mLink of
                                Nothing -> ""
                                Just link -> URI.print $ facebookLoginLinkToURI link
                              } [icon]
                          )
                  in  [ mkFab "#3b5998" "#1e3f82" facebookIcon $
                         Just $ FacebookLoginLink
                         { redirectURL: toURI (toLocation FacebookLoginReturn)
                         , state: FacebookLoginState
                           { origin: state.currentPage
                           }
                         }
                      , mkFab "#1da1f3" "#0f8cdb" twitterIcon Nothing
                      , mkFab "#dd4e40" "#c13627" googleIcon Nothing
                      ]
              , if state.pending
                   then R.div
                          [ RP.style
                            { zIndex: 1000
                            , position: "absolute"
                            , top: "0"
                            , left: "0"
                            , right: "0"
                            , bottom: "0"
                            , display: "flex"
                            , flexDirection: "column"
                            , alignItems: "center"
                            , justifyContent: "center"
                            , background: "rgba(255,255,255, 0.6)"
                            }
                          ]
                          [ circularProgress {size: 50}
                          ]
                   else R.text ""
              ]
            , dialogActions {}
              [ button
                { color: Button.secondary
                -- , onTouchTap: mkEffFn1 \_ -> dispatch Close
                } [R.text "Register"]
              , button
                { color: Button.primary
                -- , onTouchTap: mkEffFn1 \_ -> dispatch Close
                , disabled: case emailAddress state.email of
                  Nothing -> true
                  Just _ -> false
                , onTouchTap: mkEffFn1 \_ -> dispatch SubmitLogin
                } [R.text "Submit"]
              , button
                { color: Button.default
                , onTouchTap: mkEffFn1 \_ -> dispatch Close
                } [R.text "Cancel"]
              ]
            ]
      ]



loginDialog :: forall eff
             . { openLoginSignal :: Queue (read :: READ) (Effects eff) Unit
               , windowSizeSignal :: IxSignal (Effects eff) WindowSize
               , toURI :: Location -> URI
               , currentPageSignal :: IxSignal (Effects eff) SiteLinks
               , login :: EmailAddress -> HashedPassword -> Aff (Effects eff) Unit
               }
            -> R.ReactElement
loginDialog {openLoginSignal,windowSizeSignal,toURI,currentPageSignal,login} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec (spec {toURI,login}) initialState
      reactSpecLogin =
          Signal.whileMountedIxUUID
            windowSizeSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
        $ Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedPage x))
        $ Queue.whileMountedOne
            openLoginSignal
            (\this _ -> unsafeCoerceEff $ dispatcher this Open)
            reactSpec
  in  R.createElement (R.createClass reactSpecLogin) unit []
