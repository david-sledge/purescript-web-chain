module Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe (Just), maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Web.Chain.DOM (doc, el, eln, nd, ndM, txn, (+<))
import Web.Chain.Event (onReady_)
import Web.Chain.HTML (button)
import Web.HTML.HTMLDialogElement (HTMLDialogElement, fromElement, open, showModal)
import Web.HTML.HTMLDocument (body)

openCheck ∷ HTMLDialogElement → Effect Unit
openCheck dialog = do
  isOpen ← open dialog
  log $ "Dialog " <>
    if isOpen then "open"
    else "closed"

main ∷ Effect Unit
main = onReady_ $ \_ →
  (liftEffect $ body =<< doc) >>= maybe
    (liftEffect <<< throwException $ error "No document body")
    ( \bodyElem → do
        mDialog ← fromElement <$> el "dialog" []
          [ eln "form" [ "method" /\ "dialog" ]
              [ eln "p" []
                  [ eln "label" [ "for" /\ "favAnimal" ] [ txn "Favorite animal:" ]
                  , eln "select" [ "name" /\ "favAnimal" ]
                      [ eln "option" [] []
                      , eln "option" [] [ txn "Brine shrimp" ]
                      , eln "option" [] [ txn "Red panda" ]
                      , eln "option" [] [ txn "Spider monkey" ]
                      ]
                  ]
              , eln "div" []
                  [ ndM $ el "button" [ "type" /\ "reset" ] [ txn "Cancel" ]
                  , eln "button" [ "type" /\ "submit" ] [ txn "Confirm" ]
                  ]
              ]
          ]
        dialog ← maybe (liftEffect <<< throwException $ error "Issue with dialog") pure mDialog
        _ ← bodyElem +<
          [ nd dialog
          , eln "div" []
              [ ndM $ button [] [ txn "Update details" ]
                  ( Just $ const \ _ -> do
                      showModal dialog
                      openCheck dialog
                  )
              ]
          ]
        pure unit
    )
