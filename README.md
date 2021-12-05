# Chain

Chain is a library for DOM manipulation using chaining.

## Example

The example below can be found in `test`. Run `nix-shell --run "spago -x test.dhall bundle-app"` and open `test/index.html` in a browser to see it in action.

```purescript
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
  [eln "div" [("yes" *& "no")]
    [ txn "Hello, World!"
    , eln "br" [] []
    , txn "What's your name? "
    , nd $ pure nameField # onChange (const $ runReaderT (do
          let g = empty $ pure welcomeMessageArea
          value ← val $ pure nameField
          [txn
            ( if value == ""
              then "Greetings!"
              else "Greetings, " <> value <> "!"
            )
          ] >+ g
        ) doc) # change
    , ndp welcomeMessageArea
    , nd $ button [txn "Stop Greeting Me"] (const $ runReaderT (allOff $ pure nameField) doc)
    ]
  ] >+ (pure bodyElem) *> pure unit

main ∷ Effect Unit
main = do
  doc ← window >>= document
  runReaderT (ready <<< const $ runReaderT
    ( do
      liftEffect $ log "Gettin' ready..."
      mBodyElem ← liftEffect $ body doc
      case mBodyElem of
        Just bodyElem → init bodyElem *> pure unit
        _ → pure unit
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
