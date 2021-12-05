module Main where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask, runReaderT)
import Data.Maybe (Maybe(..))
import Data.Tuple.Util ((*&))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Web.Chain.DOM (el, eln, empty, nd, ndp, txn, (>+))
import Web.Chain.Event (allOff, change, onChange, ready)
import Web.Chain.HTML (button, textField, val)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (HTMLDocument, body)
import Web.HTML.Window (document)

init ∷ ∀ m. MonadAsk HTMLDocument m ⇒ MonadEffect m ⇒ HTMLElement → m Unit
init bodyElem = do
  doc ← ask
  nameField ← textField ""
  welcomeMessageArea ← el "div" [] [txn "This should never be displayed because nameField's change listener is immediately triggered below"]
  {--
  <div yes="no">
    Hello, World!
    <br/>
    What's your name? <input type="text" value="" />
    <div>Greetings!</div>
    <button>Stop Greeting Me</button>
  </div>
  --}
  [eln "div" [("yes" *& "no")]
    [ txn "Hello, World!"
    , eln "br" [] []
    , txn "What's your name? "
    , nd $ pure nameField # onChange (const $ runReaderT (do
          let g = empty $ pure welcomeMessageArea
          value ← val $ pure nameField
          [txn
            ( if value == ""
              then "Greetings!"
              else "Greetings, " <> value <> "!"
            )
          ] >+ g
        ) doc) # change
    , ndp welcomeMessageArea
    , nd $ button [txn "Stop Greeting Me"] (const $ runReaderT (allOff $ pure nameField) doc)
    ]
  ] >+ (pure bodyElem) *> pure unit

main ∷ Effect Unit
main = do
  doc ← window >>= document
  runReaderT (ready <<< const $ runReaderT
    ( do
      liftEffect $ log "Gettin' ready..."
      mBodyElem ← liftEffect $ body doc
      case mBodyElem of
        Just bodyElem → init bodyElem *> pure unit
        _ → pure unit
    ) doc) doc *> pure unit
