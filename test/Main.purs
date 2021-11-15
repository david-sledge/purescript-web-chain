module Test.Main where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.List.Types (List(..))
import Data.List.Util (s, (&:))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Web.Chain.Class (class IsDocument)
import Web.Chain.DOM (el, nd, tx)
import Web.Chain.Event (trigger)
import Web.DOM (Element)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."

--{-
hmmm :: forall m d. Bind m => MonadAsk d m => MonadEffect m => IsDocument d => m Element
hmmm = el "div" Nil ( (nd $ tx "Hello, ") &: (nd <<< el "b" Nil <<< s <<< nd $ tx "World!") )

hmmmEvent :: forall m d. MonadEffect m => MonadAsk d m => IsDocument d => m Element
hmmmEvent = trigger "change" hmmm
--}
