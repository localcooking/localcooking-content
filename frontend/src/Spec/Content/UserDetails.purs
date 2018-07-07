module Spec.Content.UserDetails where

import Error (SiteError (SiteErrorEditor), EditorError (..))
import Links (SiteLinks (UserDetailsLink), UserDetailsLinks (..))
import User (UserDetails)
import Spec.Content.UserDetails.General (general)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState)
import LocalCooking.Dependencies.Content (GetEditorSparrowClientQueues, SetEditorSparrowClientQueues)

import Prelude
import Data.Lens (Lens', Prism', lens, prism')

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import React.Signal.WhileMounted as Signal

import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (WRITE)
import Queue.One as One



type State =
  { localCooking :: LocalCookingState SiteLinks UserDetails
  }

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState localCooking =
  { localCooking
  }

data Action
  = LocalCookingAction (LocalCookingAction SiteLinks UserDetails)

type Effects eff =
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  | eff)

getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> { getEditorQueues :: GetEditorSparrowClientQueues (Effects eff)
        , setEditorQueues :: SetEditorSparrowClientQueues (Effects eff)
        , siteErrorQueue :: One.Queue (write :: WRITE) (Effects eff) SiteError
        }
     -> T.Spec (Effects eff) State Unit Action
spec params@{siteLinks}
  { getEditorQueues
  , setEditorQueues
  , siteErrorQueue
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ case state.localCooking.currentPage of -- TODO pack currentPageSignal listener to this level, so
                            -- side buttons aren't redrawn
          UserDetailsLink mUserDetails -> case mUserDetails of
            Nothing -> general params
              {getEditorQueues,setEditorQueues,siteErrorQueue}
            Just x -> case x of
              UserDetailsGeneralLink -> general params
                {getEditorQueues,setEditorQueues,siteErrorQueue}
              _ -> R.text ""
          _ -> R.text ""
      ]


userDetails :: forall eff
             . LocalCookingParams SiteLinks UserDetails (Effects eff)
            -> { getEditorQueues :: GetEditorSparrowClientQueues (Effects eff)
               , setEditorQueues :: SetEditorSparrowClientQueues (Effects eff)
               , siteErrorQueue :: One.Queue (write :: WRITE) (Effects eff) SiteError
               }
            -> R.ReactElement
userDetails params args =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params args
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
        whileMountedLocalCooking
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
