module Main
  ( main
  )
  where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask, runReaderT)
import Data.Maybe (maybe)
import Data.Tuple.Util ((*&))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Web.Chain.DOM (el, eln, empty, nd, ndp, txn, (+<<), (+<))
import Web.Chain.Event (allOff, changeM, onChange, ready)
import Web.Chain.HTML (button, textField, val)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (HTMLDocument, body)
import Web.HTML.Window (document)

pu ∷ ∀ f. Applicative f ⇒ f Unit
pu = pure unit

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
  _ <- bodyElem +< [
    eln "div" [("yes" *& "no")] [
      txn "Hello, World!",
      eln "br" [] [],
      txn "What's your name? ",
      nd $ nameField # onChange (const $ runReaderT (do
          value ← val nameField
          empty welcomeMessageArea +<< [txn
            ( if value == ""
              then "Greetings!"
              else "Greetings, " <> value <> "!"
            )
          ]
        ) doc) # changeM,
      ndp welcomeMessageArea,
      nd $ button [txn "Stop Greeting Me"] (const $ runReaderT (allOff nameField) doc)]]
  pu

main ∷ Effect Unit
main = do
  doc ← window >>= document
  runReaderT (ready <<< const $ runReaderT
    ( do
      liftEffect $ log "Gettin' ready..."
      maybe pu (\ bodyElem → init bodyElem *> pu) =<< (liftEffect $ body doc)
    ) doc) doc *> pu
