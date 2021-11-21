# Chain

Chain is a library for DOM manipulation using chaining.

## Example

The example below can be found in `test`. Run `spago bundle-app` and open `test/index.html` in a browser to see it in action.

```purescript
module Main where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask, runReaderT)
import Data.Maybe (Maybe(..))
import Data.Tuple.Util ((*&))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Web.Chain.DOM (el, empty, nd, tx, (>+))
import Web.Chain.Event (allOff, change, onChange, ready)
import Web.Chain.HTML (button, textField, val)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (HTMLDocument, body)
import Web.HTML.Window (document)

init :: forall m. MonadAsk HTMLDocument m => MonadEffect m => HTMLElement -> m Unit
init bodyElem = do
  doc <- ask
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
    , nd $ button [nd $ tx "Stop Greeting Me"] (const $ runReaderT (allOff $ pure nameField) doc)
    ]
  ] >+ (pure bodyElem) *> pure unit

main :: Effect Unit
main = do
  doc <- window >>= document
  runReaderT (ready <<< const $ runReaderT
    ( do
      liftEffect $ log "Gettin' ready..."
      mBodyElem <- liftEffect $ body doc
      case mBodyElem of
        Just bodyElem -> init bodyElem *> pure unit
        _ -> pure unit
    ) doc) doc *> pure unit
```

```html
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html>
<html>

  <head>
    <title>Example</title>
    <script src="../index.js"></script>
  </head>
  <body></body>

</html>
```
