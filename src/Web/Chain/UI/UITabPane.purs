module Web.Chain.UI.UITabPane
  ( TabPaneClassNames
  , mkTabPanes
  , nullTabPaneClassNames
  ) where

import Prelude

import Data.Array.Mutable as A
import Data.Foldable (class Foldable, null, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Unsafe.Reference (unsafeRefEq)
import Web.Chain (el, onM, setAttrs)
import Web.Chain.CSSOM as C
import Web.Chain.HTML as H
import Web.DOM (Element, Node)
import Web.DOM.Class.ElementOp (classList)
import Web.DOM.Class.NodeOp (appendChild, hasChildNodes, toNode)
import Web.DOM.DOMTokenList as D
import Web.HTML (HTMLDivElement)

type TabPaneClassNames f1 f2 f3 f4 f5 =
  { tabsContainer ∷ f1 String
  , panesContainer ∷ f2 String
  , allTabs ∷ f3 String
  , activeTab ∷ f4 String
  , panes ∷ f5 String
  }

nullTabPaneClassNames ∷ TabPaneClassNames Array Array Array Array Array
nullTabPaneClassNames =
  { tabsContainer: []
  , panesContainer: []
  , allTabs: []
  , activeTab: []
  , panes: []
  }

mkTabPanes
  ∷ ∀ m f1 f2 f3 f4 f5 f6 d
  . MonadEffect m
  ⇒ Foldable f1
  ⇒ Foldable f2
  ⇒ Foldable f3
  ⇒ Foldable f4
  ⇒ Foldable f5
  ⇒ Foldable f6
  ⇒ f1 { content ∷ m Node, label ∷ String, tab ∷ m Node | d }
  → TabPaneClassNames f2 f3 f4 f5 f6
  → m (Element /\ HTMLDivElement)
mkTabPanes details classNames = do
  tabs ← el "nav" [ "role" /\ "tablist" ] [] # C.addClassesM classNames.tabsContainer
  tabPanePairArray ← A.new
  contentPanes ← H.div [] [] # C.addClassesM classNames.panesContainer
  traverse_
    ( \detail → do
        hasChildren ← hasChildNodes tabs
        let
          paneId = "nav-" <> detail.label
          tabId = paneId <> "-tab"
        contentUi ← H.div [ "aria-labelledby" /\ tabId, "id" /\ paneId, "role" /\ "tabpanel", "tabindex" /\ "0" ] [ detail.content ]
          # C.addClassesM classNames.panes
          # if hasChildren then C.hideM else C.showM
        appendChild contentUi $ toNode contentPanes
        tabUi ← el "button" [ "aria-controls" /\ paneId, "aria-selected" /\ (if hasChildren then "false" else "true"), "data-bs-target" /\ ("#nav-" <> detail.label), "data-bs-toggle" /\ "tab", "id" /\ tabId, "role" /\ "tab", "type" /\ "button" ] [ detail.tab ]
          # C.addClassesM classNames.allTabs
          # onM "click" \_ → do
              -- iterate through content panes
              traverse_
                ( \(tab /\ pane) → do
                    ( let
                        toggle f bs g = do
                          traverse_ (bind (classList tab) <<< flip f) classNames.activeTab
                          _ ← setAttrs [ "aria-selected" /\ bs ] tab
                          g pane
                      in
                        if unsafeRefEq pane contentUi then toggle D.add "true" C.show
                        else toggle D.remove "false" C.hide
                    )
                ) =<< A.freeze tabPanePairArray
        when (hasChildren && not (null classNames.activeTab)) <<< void $ C.addClasses classNames.activeTab tabUi
        A.push (tabUi /\ contentUi) tabPanePairArray
        appendChild tabUi $ toNode tabs
    )
    details
  pure (tabs /\ contentPanes)
