import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, cancel)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log, CONSOLE)
import Data.Maybe (Maybe(..))
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))

import Control.Monad.Eff.Exception (EXCEPTION)

import Web.Cookies (COOKIE,getCookie)
-- import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff)

import Network.HTTP.Affjax (AffjaxRequest, defaultRequest, AffjaxResponse, Affjax, URL, affjax, AJAX)
import Network.HTTP.Affjax.Response

import Network.HTTP.RequestHeader (RequestHeader(..))

import Data.Either (Either(..))

import Data.HTTP.Method (Method(..))

getcook :: forall a b eff. a -> b -> Aff (cookie :: COOKIE, console :: CONSOLE | eff) (Maybe String)
getcook event element = do
  cook <- liftEff $ getCookie "mycookie"
  log $ show cook
  pure cook

makeReq :: forall eff. Aff (cookie :: COOKIE, console :: CONSOLE | eff) (Maybe (AffjaxRequest Unit))
makeReq = do
  mCook <- liftEff $ getCookie "XSRF-TOKEN"
  case mCook of
   Nothing -> pure Nothing
   Just cook -> pure $ Just $ (defaultRequest {headers = [RequestHeader "X-XSRF-TOKEN" cook]})

sss :: forall eff. Aff (console :: CONSOLE | eff ) Unit
sss = launchAff $ do
  res <- affjax $ defaultRequest { url = "/api" }
  liftEff $ log $ "GET /api response: " <> res.response
{-
authorizedGet :: forall a eff. Respondable a => URL -> Aff (console :: CONSOLE, exception :: EXCEPTION, ajax :: AJAX | eff) Unit -- (Maybe (AffjaxResponse a)) -- Affjax e a
authorizedGet u = do
  mA <- makeReq
  case mA of
    Nothing -> pure unit -- return Unit
    Just a  -> launchAff $ do
      msg <- affjax $ a { url = u }
      -- pure $ Just res
      log msg.response
-}
{-
foreign import data AJAX :: Effect

type Affjax e a = Aff (ajax :: AJAX | e) (AffjaxResponse a)

affjax :: forall e a b. Requestable a => Respondable b => AffjaxRequest a -> Affjax e b
affjax = makeAff' <<< affjax'

get :: forall e a. Respondable a => URL -> Affjax e a
get u = affjax $ defaultRequest { url = u }
-}


data Event = Increment | Decrement

type State = Int

-- | Return a new state (and effects) from each event
foldp :: Event -> State -> EffModel State Event (console :: CONSOLE)
foldp Increment n = { state: n + 1, effects: [ log ("increment " <> show n) *> pure Nothing ] }
foldp Decrement n = { state: n - 1, effects: [] }

-- | Return markup from the state
view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show count)
    button #! onClick (const Decrement) $ text "Decrement"

-- | Start and render the app
main :: Eff (CoreEffects (console :: CONSOLE)) Unit
main = do
  app <- start
    { initialState: 0
    , view
    , foldp
    , inputs: []
    }
  renderToDOM "#app" app.markup app.input
{-
import Prelude
import App.Events (AppEffects, Event(..), foldp)
import App.Routes (match)
import App.State (State, init)
import App.View.Layout (view)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import Pux (CoreEffects, App, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Signal ((~>))

type WebApp = App (DOMEvent -> Event) Event State

type ClientEffects = CoreEffects (AppEffects (history :: HISTORY, dom :: DOM))

main :: String -> State -> Eff ClientEffects WebApp
main url state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleURL =<< window

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  -- | Start the app.
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: [routeSignal] }

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input

  -- | Return app to be used for hot reloading logic in support/client.entry.js
  pure app

initialState :: State
initialState = init "/"
-}
