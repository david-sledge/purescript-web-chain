module Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(Just), maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Web.Chain.DOM (doc, el, eln, empty, nd, ndM, txn, (+<), (+<<))
import Web.Chain.Event (changeM, onChange, onReady_)
import Web.Chain.HTML (buttonN, divN, textField)
import Web.Chain.HTML.Class.HTMLAbleOp (disable)
import Web.Chain.HTML.Class.HTMLValueContainerOp (val)
import Web.HTML.HTMLDocument (body)

main ∷ Effect Unit
main = onReady_ $ \_ → do
  liftEffect $ log "Getting ready..."
  (liftEffect $ body =<< doc) >>= maybe
    (liftEffect <<< throwException $ error "No document body")
    ( \bodyElem → do
        nameField ← textField [] ""
        welcomeMessageArea ← divN [] [ txn "This should never be displayed because nameField's change listener is immediately triggered below (changeM)" ]
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
          [ divN [ ("yes" /\ "no") ]
              [ txn "Hello, World!"
              , eln "br" [] []
              , txn "What's your name? "
              , nameField
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
                  # ndM
              , nd welcomeMessageArea
              , buttonN [] [ txn "Stop Greeting Me" ] $ Just \ btn → const do
                  empty welcomeMessageArea # void
                  disable nameField # void
                  disable btn
              ]
          ]
        pure unit
    )
