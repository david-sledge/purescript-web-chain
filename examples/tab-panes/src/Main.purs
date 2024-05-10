module Main
  ( main
  ) where

import Prelude

import Data.DateTime.Instant (Instant, toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.HashMap as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Effect.Now (now)
import Web.Chain.DOM (doc, eln, nd, ndM, txn, (+<))
import Web.Chain.Event (onChange, onReady_)
import Web.Chain.HTML (checkbox)
import Web.Chain.UI.UITabPane (mkTabPanes, nullTabPaneClassNames)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLInputElement (checked)

main ∷ Effect Unit
main = onReady_ $ \_ → do
  (liftEffect $ body =<< doc) >>= maybe
    (liftEffect <<< throwException $ error "No document body")
    ( \bodyElem → do
        (elem /\ divElem) ← mkTabPanes
          [ { tab: txn "Tab 1"
            , content: txn "Pane 1"
            , label: "profile"
            }
          , { tab: txn "Tab II"
            , content: txn "Pane II"
            , label: "profile"
            }
          , { tab: txn "Tab Three"
            , content: txn "Pane three"
            , label: "profile"
            }
          ]
          nullTabPaneClassNames
            { tabsContainer = [ "nav", "nav-tabs" ]
            , allTabs = [ "nav-link" ]
            , activeTab = [ "active" ]
            , panes = [ "tab-pane" ]
            }
        _ ← bodyElem +<
          [ eln "div" [ "class" /\ "container-fluid" ]
              [ eln "div" [ "class" /\ "card" ]
                  [ eln "div" [ "class" /\ "card-header" ] [ txn "Tab Panes" ]
                  , eln "div" [ "class" /\ "card-body" ] [ nd elem, nd divElem ]
                  ]
              ]
          ]
        pure unit
    )
