module Main where

import Prelude hiding (div)

import Control.Monad.Aff (attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxRequest, URL, defaultRequest, post, affjax)


import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Web.Cookies (COOKIE, getCookie)

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Network.HTTP.Affjax.Response (class Respondable)

import Pux.DOM.Events (DOMEvent, onClick, onChange, onSubmit, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, input, form)
import Text.Smolder.HTML.Attributes (name, type', value)
import Text.Smolder.Markup (text, (!), (#!))


import Pux (CoreEffects, EffModel, start, noEffects)

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
   Nothing -> pure Nothing
   Just cook -> pure $ Just $ (defaultRequest {headers = [RequestHeader "X-XSRF-TOKEN" cook]})

authorizedRequest :: forall eff. Method -> URL -> Eff (console :: CONSOLE, cookie :: COOKIE, ajax :: AJAX, exception :: EXCEPTION | eff) (Unit)
authorizedRequest method url = void $ launchAff do
  mAuthRequest <- liftEff mkAuthRequest
  case mAuthRequest of
    Nothing -> pure unit
    Just authRequest  -> do
      msg <- affjax $ authRequest { url = url, method = Left method }
      liftEff $ log msg.response

authorizedGet :: forall eff. URL -> Eff (console :: CONSOLE, cookie :: COOKIE, ajax :: AJAX, exception :: EXCEPTION | eff) (Unit)
authorizedGet url = authorizedRequest GET url

authorizedPost :: forall eff. URL -> Eff (console :: CONSOLE, cookie :: COOKIE,  ajax :: AJAX, exception :: EXCEPTION | eff) (Unit)
authorizedPost url = authorizedRequest POST url

authPost :: forall e b. Respondable b => URL -> String -> Affjax e b
authPost url content = affjax $ defaultRequest { method = Left POST, url = url, content = Just ( content)}



data Event = Echo
           | SignIn
           | UsernameChange DOMEvent
           | PasswordChange DOMEvent
           
type State = 
  { username :: String
  , password :: String
  }

-- | Return a new state (and effects) from each event
foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, console :: CONSOLE)
foldp Echo state = 
  { state: state
    , effects: [ do 
      liftEff $ log "Hello!" 
      pure Nothing
    ]
  }
foldp SignIn st = 
  { state : st
    , effects : [do 
      res <- attempt (post "/login" (show $ encodeJson $ Login {username: st.username, password: st.password}))
      let decode r = decodeJson r.response :: Either String String
      let todos = either (Left <<< show) decode res
      pure Nothing 
    ]
  }
foldp (UsernameChange ev) st = noEffects $ st { username = targetValue ev }
foldp (PasswordChange ev) st = noEffects $ st { password = targetValue ev }

--foldp Increment n = { state: n + 1, effects: [ log ("increment " <> show n) *> pure Nothing ] }
--foldp Decrement n = { state: n - 1, effects: [] }

-- | Return markup from the state
view :: State -> HTML Event
view state = do
  div $
    button #! onClick (const Echo) $ text "Echo"
    
  form ! name "signin" #! onSubmit (const SignIn) $ do
    input ! type' "text" ! value state.username #! onChange UsernameChange
    input ! type' "password" ! value state.password #! onChange PasswordChange
    button ! type' "submit" $ text "Sign In"

init :: State
init = { username : "", password : "" }

-- | Start and render the app
main :: Eff (CoreEffects (ajax :: AJAX, console :: CONSOLE)) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }
  renderToDOM "#app" app.markup app.input
