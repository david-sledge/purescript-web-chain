module Main
  ( main
  ) where

import Prelude

import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Web.Chain.DOM (doc, el, eln, empty, nd, ndM, txn, (+<), (+<<))
import Web.Chain.Event (changeM, onChange, onReady_)
import Web.Chain.HTML (button, textField, val)
import Web.Event.Class.EventTargetOp (allOff)
import Web.HTML.HTMLDocument (body)

main ∷ Effect Unit
main = onReady_ $ \_ → do
  liftEffect $ log "Getting ready..."
  (liftEffect $ body =<< doc) >>= maybe
    (liftEffect <<< throwException $ error "No document body")
    ( \bodyElem → do
        nameField ← textField ""
        welcomeMessageArea ← el "div" [] [ txn "This should never be displayed because nameField's change listener is immediately triggered below (changeM)" ]
        {--
      <body>
        <div yes="no">
          Hello, World!
          <br/>
          What's your name? <input type="text" value="" />
          <div>Greetings!</div>
          <button>Stop Greeting Me</button>
        </div>
      </body>
      --}
        _ ← bodyElem +<
          [ eln "div" [ ("yes" /\ "no") ]
              [ txn "Hello, World!"
              , eln "br" [] []
              , txn "What's your name? "
              , ndM $ nameField
                  # onChange
                      ( const do
                          value ← val nameField
                          empty welcomeMessageArea +<<
                            [ txn
                                ( if value == "" then "Greetings!"
                                  else "Greetings, " <> value <> "!"
                                )
                            ]
                      )
                  # changeM
              , nd welcomeMessageArea
              , ndM $ button [ txn "Stop Greeting Me" ]
                  ( const do
                      void $ allOff nameField
                      empty welcomeMessageArea
                  )
              ]
          ]
        pure unit
    )
