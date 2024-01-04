module Main where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Web.Chain.DOM (doc, eln, nd, txn, (+<))
import Web.Chain.Event (onReady_)
import Web.Chain.HTML (textField, val)
import Web.Event.Class.EventTargetOp (offM, on, onM)
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.Event.BeforeUnloadEvent (fromEvent, setReturnValue)
import Web.HTML.HTMLDocument (body)

beforeUnloadHandler ∷ Event → Effect Unit
beforeUnloadHandler ev = do
  fromEvent ev # maybe (pure unit)
    ( \buev -> do
        preventDefault ev
        setReturnValue "true" buev
    )

main :: Effect Unit
main = onReady_ $ \_ →
  (liftEffect $ body =<< doc) >>= maybe
    (liftEffect <<< throwException $ error "No document body")
    ( \bodyElem → do
        nameInput ← textField ""
        _ ← bodyElem +<
          [ eln "h1" [] [ txn "BeforeUnloadEvent" ]
          , nd $ nameInput # on "input"
              ( \_ → do
                  value <- val nameInput
                  window #
                    ( if value == "" then offM
                      else onM
                    ) "beforeunload" beforeUnloadHandler
              )
          ]
        pure unit
    )
