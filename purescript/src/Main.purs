module Main where

import Prelude hiding (div)

import Control.Monad.Aff (Aff,attempt, launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))

import Network.HTTP.Affjax (AJAX, Affjax, AffjaxRequest, URL, defaultRequest, affjax)
import Network.HTTP.RequestHeader (RequestHeader(..))

import Network.HTTP.Affjax.Response (class Respondable)

import Pux (CoreEffects, EffModel, start, noEffects)
import Pux.DOM.Events (DOMEvent, onClick, onChange, targetValue)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)

import Text.Smolder.HTML (button, div, label, input)
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup (text, (!), (#!))

import Web.Cookies (COOKIE, getCookie)

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

post :: forall e b. Respondable b => URL -> String -> Affjax e b
post url content = 
  affjax $ 
    defaultRequest { method = Left POST
                   , url = url
                   , content = Just content
                   , headers = 
                     [ Accept (MediaType "*/*")
                     , ContentType (MediaType "application/json")
                     ]
                   }


data Event = UsernameChange DOMEvent
           | PasswordChange DOMEvent
           | SignIn
           | RollDie
           | IsLoggedIn
           
type State = 
  { username :: String
  , password :: String
  }

-- | Return a new state (and effects) from each event
foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, console :: CONSOLE, cookie :: COOKIE)
foldp RollDie st = 
  { state : st
  , effects : [ do 
      mReq <- liftEff mkAuthRequest
      case mReq of
        Nothing -> do 
          liftEff $ log "cookie doesn't exist"
        Just req -> do             
          res <- attempt $ authorizedGet req "/die/roll"
          let decode r = decodeJson r.response :: Either String Int
          let todos = either (Left <<< show) decode res
          liftEff $ log $ show todos
      pure Nothing
  ]}  

foldp SignIn st = 
  { state : st
    , effects : [do
      let login = (show $ encodeJson $ Login {username: st.username, password: st.password})
      res <- attempt (post "/login" login)
      let decode r = decodeJson r.response :: Either String String
      let todos = either (Left <<< show) decode res
      pure Nothing 
    ]
  }
  
foldp (UsernameChange ev) st = noEffects $ st { username = targetValue ev }

foldp (PasswordChange ev) st = noEffects $ st { password = targetValue ev }

foldp IsLoggedIn st =
  { state : st
  , effects : [ do
      mReq <- liftEff mkAuthRequest
      case mReq of
        Nothing -> do 
          liftEff $ log "cookie doesn't exist"
        Just req -> do             
          res <- attempt $ authorizedGet req "/loggedin"
          let decode r = decodeJson r.response :: Either String User
              eUser = either (Left <<< show) decode res
          pure unit
          -- liftEff $ log $ show todos
      --foldp IsLoggedIn st
      pure Nothing      
    ]
  }
-- | Return markup from the state
view :: State -> HTML Event
view state = do
  div do
    div do
      label $ text "email"
      input ! type' "text" ! value state.username #! onChange UsernameChange
    div do 
      label $ text "password"
      input ! type' "password" ! value state.password #! onChange PasswordChange
    button #! onClick (const SignIn) $ text "Sign In"
  
  div $
    button #! onClick (const RollDie) $ text "Roll Die"

init :: State
init = { username : "", password : "" }

-- | Start and render the app
main :: Eff (CoreEffects (ajax :: AJAX, console :: CONSOLE, cookie :: COOKIE)) Unit
main =  do
{-
  mReq <- mkAuthRequest
  void $ launchAff do 
      case mReq of
        Nothing -> pure init
        Just req -> do             
          res <- attempt $ authorizedGet req "/loggedin"
          let decode r = decodeJson r.response :: Either String String
          let todos = either (Left <<< show) decode res
          pure init
-}
  app <- start
          { initialState: init
          , view
          , foldp
          , inputs: []
          }
  
  renderToDOM "#app" app.markup app.input
