module Main
  ( main
  ) where

import Prelude

import Data.Maybe (maybe)
import Data.Tuple.Util ((*&))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Web.Chain.DOM (doc, el, eln, nd, ndp, txn, (+<))
import Web.Chain.Event (onReady_)
import Web.Chain.HTML (button)
import Web.HTML.HTMLDialogElement (HTMLDialogElement, fromElement, open, showModal)
import Web.HTML.HTMLDocument (body)

openCheck ∷ HTMLDialogElement → Effect Unit
openCheck dialog = do
  isOpen <- open dialog
  log $ "Dialog " <>
    if isOpen then "open"
    else "closed"

main :: Effect Unit
main = onReady_ $ \_ →
  (liftEffect $ body =<< doc) >>= maybe
    (liftEffect <<< throwException $ error "No document body")
    ( \bodyElem → do
        mDialog <- fromElement <$> el "dialog" []
          [ eln "form" [ "method" *& "dialog" ]
              [ eln "p" []
                  [ eln "label" [ "for" *& "favAnimal" ] [ txn "Favorite animal:" ]
                  , eln "select" [ "name" *& "favAnimal" ]
                      [ eln "option" [] []
                      , eln "option" [] [ txn "Brine shrimp" ]
                      , eln "option" [] [ txn "Red panda" ]
                      , eln "option" [] [ txn "Spider monkey" ]
                      ]
                  ]
              , eln "div" []
                  [ nd $ el "button" [ "type" *& "reset" ] [ txn "Cancel" ]
                  , eln "button" [ "type" *& "submit" ] [ txn "Confirm" ]
                  ]
              ]
          ]
        dialog <- maybe (liftEffect <<< throwException $ error "Issue with dialog") pure mDialog
        _ ← bodyElem +<
          [ ndp dialog
          , eln "div" []
              [ nd $ button [ txn "Update details" ]
                  ( const $ do
                      showModal dialog
                      openCheck dialog
                  )
              ]
          ]
        pure unit
    )
