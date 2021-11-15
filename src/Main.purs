module Main where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask, runReaderT)
import Data.Maybe (Maybe(..))
import Data.Tuple.Util ((*&))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Web.Chain.DOM (el, empty, nd, tx, (>+))
import Web.Chain.Event (allOff, change, on, onChange, ready)
import Web.Chain.HTML (textField, val)
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, body)
import Web.HTML.Window (document)

init :: forall m. MonadAsk HTMLDocument m => MonadEffect m => m Unit
init = do
  doc <- ask
  mBodyElem <- liftEffect $ body doc
  case mBodyElem of
    Just bodyElem -> do
      nameField <- textField ""
      welcomeMessageArea <- el "div" [] [nd $ tx "This should never be displayed because the change listener is immediately triggered below"]
      [nd $ el "div" [("yes" *& "no")]
        [ nd $ tx "Hello, World!"
        , nd $ el "br" [] []
        , nd $ tx "What's your name? "
        , nd $ pure nameField # onChange (const $ runReaderT (do
              let g = empty $ pure welcomeMessageArea
              value <- val $ pure nameField
              if value == ""
                then [nd $ tx "Greetings!"] >+ g
                else [nd <<< tx $ "Greetings, " <> value <> "!"] >+ g
            ) doc) # change
        , nd $ pure welcomeMessageArea
        , nd $ on "click" (const $ runReaderT (allOff $ pure nameField) doc) $ el "button" [] [nd $ tx "Stop Greeting Me"]
        ]
      ] >+ (pure bodyElem) *> pure unit
    _ -> pure unit

main :: Effect Unit
main = do
  log "🍝"
  doc <- window >>= document
  runReaderT (ready <<< const $ runReaderT init doc) doc *> pure unit
  -- ready $ const init
