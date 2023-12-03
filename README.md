# Chain

Chain is a library for DOM manipulation using chaining.

## Example

The example below can be found in `test`. Run `spago -x test.dhall bundle-app` and open `test/index.html` in a browser to see it in action.

```purescript
pu ∷ ∀ f. Applicative f ⇒ f Unit
pu = pure unit

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
  _ <- bodyElem +< [
    eln "div" [("yes" *& "no")] [
      txn "Hello, World!",
      eln "br" [] [],
      txn "What's your name? ",
      nd $ nameField # onChange (const $ runReaderT (do
          value ← val nameField
          empty welcomeMessageArea +<< [txn
            ( if value == ""
              then "Greetings!"
              else "Greetings, " <> value <> "!"
            )
          ]
        ) doc) # changeM,
      ndp welcomeMessageArea,
      nd $ button [txn "Stop Greeting Me"] (const $ runReaderT (allOff nameField) doc)]]
  pu

main ∷ Effect Unit
main = do
  doc ← window >>= document
  runReaderT (ready <<< const $ runReaderT
    ( do
      liftEffect $ log "Gettin' ready..."
      maybe pu (\ bodyElem → init bodyElem *> pu) =<< (liftEffect $ body doc)
    ) doc) doc *> pu
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
