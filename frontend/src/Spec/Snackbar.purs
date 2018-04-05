module Spec.Snackbar where

import Client.Dependencies.AuthToken (AuthTokenFailure (..))
import Login.Error (AuthError (..))

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.Queue.WhileMounted as Queue
import Data.Nullable (toNullable)
import Data.Time.Duration (Milliseconds (..))
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.List (List (..))
import Data.List as List
import Data.Generic (class Generic, gShow)
import Control.Monad.Base (liftBase)
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Uncurried (mkEffFn2)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Class (liftEff)

import MaterialUI.Snackbar (snackbar)

import Queue (READ)
import Queue.One as One



data UserDetailsError
  = UserDetailsEmailNoInitOut
  | UserDetailsEmailNoAuth

derive instance genericUserDetailsError :: Generic UserDetailsError

instance showUserDetailsError :: Show UserDetailsError where
  show = gShow


data RegisterError
  = RegisterErrorBadCaptchaResponse
  | RegisterErrorEmailInUse

derive instance genericRegisterError :: Generic RegisterError

instance showRegisterError :: Show RegisterError where
  show = gShow


data RedirectError
  = RedirectRegisterAuth
  | RedirectUserDetailsNoAuth

derive instance genericRedirectError :: Generic RedirectError

instance showRedirectError :: Show RedirectError where
  show = gShow


data SnackbarMessage
  = SnackbarMessageAuthFailure AuthTokenFailure
  | SnackbarMessageAuthError AuthError
  | SnackbarMessageUserDetails UserDetailsError
  | SnackbarMessageRegister (Maybe RegisterError)
  | SnackbarMessageRedirect RedirectError

derive instance genericSnackbarMessage :: Generic SnackbarMessage

instance showSnackbarMessage :: Show SnackbarMessage where
  show = gShow



type State =
  { errors :: List SnackbarMessage
  , open :: Boolean
  }

initialState :: State
initialState =
  { errors: Nil
  , open: false
  }

data Action
  = GotMessage SnackbarMessage
  | PopMessage

type Effects eff =
  ( ref :: REF
  | eff)


spec :: forall eff. T.Spec (Effects eff) State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotMessage x -> do
        liftEff $ unsafeCoerceEff $ log $ "got message: " <> show x <> ", changing state..."
        mState <- T.cotransform _ { errors = List.snoc state.errors x, open = true }
        liftEff $ unsafeCoerceEff $ log $ "got message: " <> show x <> ", " <> case mState of
          Nothing -> "Nothing"
          Just {errors,open} -> "Just {errors: " <> show errors <> ", open: " <> show open <> "}"
      PopMessage -> do
        liftEff $ unsafeCoerceEff $ log "popping message"
        case List.uncons state.errors of
          Nothing -> liftEff $ unsafeCoerceEff $ log "wtf no errors" -- bug out
          Just {head,tail} -> do
            liftBase $ delay $ Milliseconds 2000.0
            mState <- T.cotransform _ { errors = tail, open = false }
            unless (List.null tail) $ do
              liftBase $ delay $ Milliseconds 2000.0
              void $ T.cotransform _ { open = true }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ snackbar
        { open: state.open
        , autoHideDuration: toNullable $ Just $ Milliseconds 10000.0
        -- , resumeHideDuration: toNullable $ Just $ Milliseconds 0.0
        , onClose: mkEffFn2 \_ _ -> dispatch PopMessage
        , message: R.div []
          [ case List.head state.errors of
              Nothing -> R.text ""
              Just x -> case x of
                SnackbarMessageAuthFailure authFailure -> case authFailure of
                  BadPassword -> R.text "Password incorrect, please try again."
                  EmailDoesntExist -> R.text "Email address not found, please register."
                SnackbarMessageAuthError authError -> case authError of
                  FBLoginReturnBad code msg -> R.text $ "Bad Facebook login response: " <> code <> ", " <> msg
                  FBLoginReturnDenied desc -> R.text $ "Facebook login denied: " <> desc
                  FBLoginReturnBadParse -> R.text "Internal error: Facebook login return unparsable."
                  FBLoginReturnNoUser -> R.text "Facebook user not recognized, please link your account."
                  AuthExistsFailure -> R.text "Warning: You've been logged out; your session expired."
                SnackbarMessageUserDetails userDetails -> case userDetails of
                  UserDetailsEmailNoInitOut -> R.text "Internal Error: userDetails/email resource failed"
                  UserDetailsEmailNoAuth -> R.text "Error: No authorization for email"
                SnackbarMessageRegister mRegister -> case mRegister of
                  Nothing -> R.text "Registered! Please check your spam folder and confirm in 7 days."
                  Just register -> case register of
                    RegisterErrorBadCaptchaResponse -> R.text "Bad ReCaptcha response."
                    RegisterErrorEmailInUse -> R.text "Email address is already registered."
                SnackbarMessageRedirect redirect -> case redirect of
                  RedirectRegisterAuth -> R.text "Redirected - can't register while logged in."
                  RedirectUserDetailsNoAuth -> R.text "Redirected - can't view user details while logged out."
          ]
        }
      ]



messages :: forall eff
          . { errorMessageQueue :: One.Queue (read :: READ) (Effects eff) SnackbarMessage
            } -> R.ReactElement
messages {errorMessageQueue} =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
      reactSpec' =
        Queue.whileMountedOne
          errorMessageQueue
          (\this x -> unsafeCoerceEff $ dispatcher this $ GotMessage x)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
