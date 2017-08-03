module Main where

import Prelude hiding (div)

import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error, EXCEPTION)

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))

import Network.HTTP.Affjax (AJAX, Affjax, AffjaxResponse, AffjaxRequest, URL, defaultRequest, affjax)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))

import Pux (CoreEffects, EffModel, noEffects, onlyEffects, start)
import Pux.DOM.Events (DOMEvent, onClick, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)

import Text.Smolder.HTML (button, div, label, input, span)
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup (text, (!), (#!))

import Web.Cookies (COOKIE, deleteCookie, getCookie)

data User = User 
  { id    :: Int
  , email :: String
  }

instance decodeJsonUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    email <- obj .? "email"
    pure $ User { id: id, email: email }

instance encodeJsonUser :: EncodeJson User where
  encodeJson (User user)
    = "id" := user.id
    ~> "email" := user.email
    ~> jsonEmptyObject

data Login = Login
  { username :: String
  , password :: String
  }
  
instance decodeJsonLogin :: DecodeJson Login where
  decodeJson json = do
    obj <- decodeJson json
    username <- obj .? "username"    
    password <- obj .? "password"
    pure $ Login { username: username, password: password }

instance encodeJsonLogin :: EncodeJson Login where
  encodeJson (Login login)
    = "username" := login.username
    ~> "password" := login.password
    ~> jsonEmptyObject

mkAuthRequest :: forall eff. Eff (cookie :: COOKIE | eff) (Maybe (AffjaxRequest Unit))
mkAuthRequest = do
  mCook <- liftEff $ getCookie "XSRF-TOKEN"
  case mCook of
   Nothing   -> pure Nothing
   Just cook -> 
     pure $ 
       Just $ 
         defaultRequest { headers = 
                           [ RequestHeader "X-XSRF-TOKEN" cook
                           , Accept (MediaType "*/*")
                           , ContentType (MediaType "application/json")
                           ]}

authorizedGet :: forall e b. Respondable b => (AffjaxRequest Unit) -> URL -> Affjax e b
authorizedGet request url = affjax $ request { method = Left GET, url = url}

post :: forall e a b. EncodeJson a => Respondable b => URL -> a -> Affjax e b
post url content = 
  affjax $ 
    defaultRequest { method = Left POST
                   , url = url
                   , content = Just (show $ encodeJson content)
                   , headers = 
                     [ Accept (MediaType "*/*")
                     , ContentType (MediaType "application/json")
                     ]
                   }


data Event = LoginRequest
           | LoginResponse Boolean
           | LogoutRequest 
           | LogoutResponse
           | RollDieRequest
           | RollDieResponse String
           | UsernameChange DOMEvent
           | PasswordChange DOMEvent

type State = 
  { username :: String
  , password :: String
  , loggedin :: Boolean
  , rollmessage :: String
  }

-- | Return a new state (and effects) from each event
foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, console :: CONSOLE, cookie :: COOKIE)
foldp LoginRequest st = onlyEffects st [ do
    let login = Login {username: st.username, password: st.password}
    eRes :: (Either Error (AffjaxResponse String)) <- attempt (post "/login" login)
    case eRes of
      Left  _ -> pure $ Just $ LoginResponse false
      Right _ -> pure $ Just $ LoginResponse true
  ]

foldp (LoginResponse loggedin) st = noEffects $ st { loggedin = loggedin }

foldp LogoutRequest st = onlyEffects st [ do
    liftEff $ deleteCookie "XSRF-TOKEN"
    pure $ Just LogoutResponse
  ]

foldp LogoutResponse st = noEffects $ st { loggedin = false }

foldp RollDieRequest st = onlyEffects st [ do 
    mReq <- liftEff mkAuthRequest
    case mReq of
      Nothing -> pure $ Just $ RollDieResponse "You are not logged in. You cannot request a die roll."
      Just req -> do             
        eRes :: (Either Error (AffjaxResponse String)) <- attempt $ authorizedGet req "/die/roll"
        case eRes of
          Left  _   -> pure $ Just $ RollDieResponse "Unable to request a die roll from the server."
          Right res -> pure $ Just $ RollDieResponse ("You rolled a " <> res.response <> "!")
  ]

foldp (RollDieResponse s) st = noEffects $ st { rollmessage = s }

foldp (UsernameChange ev) st = noEffects $ st { username = targetValue ev }

foldp (PasswordChange ev) st = noEffects $ st { password = targetValue ev }

-- | Return markup from the state
view :: State -> HTML Event
view state = do
  div $
    if state.loggedin 
      then span $ text "You are logged in."
      else do 
        div $ text "You are logged out."
        div $ text "Try logging in with email: \"test@test.com\" and password: \"password\"."
  
  div $ 
    if state.loggedin
      then
        button #! onClick (const LogoutRequest) $ text "Log out"
      else
        div do
          div do
            label $ text "email"
            input ! type' "text" ! value state.username #! onChange UsernameChange
          div do 
            label $ text "password"
            input ! type' "password" ! value state.password #! onChange PasswordChange
          button #! onClick (const LoginRequest) $ text "Log In"
  
  div do
    div $ text "Request a die roll from the server."
    button #! onClick (const RollDieRequest) $ text "Roll Die"
    div $ text state.rollmessage

init :: State
init = { username : "", password : "", loggedin : false, rollmessage : "" }

-- | Start and render the app
main :: Eff (CoreEffects (ajax :: AJAX, console :: CONSOLE, cookie :: COOKIE, exception :: EXCEPTION)) Unit
main =  void $ launchAff do
  mReq <- liftEff mkAuthRequest
 
  newInit <- case mReq of
    Nothing -> pure init
    Just req -> do             
      eRes :: (Either Error (AffjaxResponse Unit)) <- attempt $ authorizedGet req "/loggedin"
      liftEff $ 
        either 
          (const $ pure init) 
          (\res -> if res.status == (StatusCode 200) then pure init { loggedin = true } else pure init) 
          eRes

  app <- liftEff $ start
          { initialState: newInit
          , view
          , foldp
          , inputs: []
          }
  
  liftEff $ renderToDOM "#app" app.markup app.input
