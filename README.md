# Chain

Chain is a library for DOM manipulation using chaining.

## Example

The example below can be found in `test`. Run `spago -x test.dhall bundle-app` and open `test/index.html` in a browser to see it in action.

```purescript
main ∷ ∀ m. MonadEffect m ⇒ m Unit
main = onReady_ $ \ _ → do
  liftEffect $ log "Getting ready..."
  (liftEffect $ body =<< doc) >>= maybe
    (liftEffect <<< throwException $ error "No document body")
    (\ bodyElem → do
      nameField ← textField ""
      welcomeMessageArea ← el "div" [] [txn "This should never be displayed because nameField's change listener is immediately triggered below (changeM)"]
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
      _ ← bodyElem +< [
        eln "div" [("yes" *& "no")] [
          txn "Hello, World!",
          eln "br" [] [],
          txn "What's your name? ",
          nd $ nameField # onChange (const $ do
              value ← val nameField
              empty welcomeMessageArea +<< [txn
                ( if value == ""
                  then "Greetings!"
                  else "Greetings, " <> value <> "!"
                )
              ]
            ) # changeM,
          ndp welcomeMessageArea,
          nd $ button [txn "Stop Greeting Me"] (const $ allOff nameField)]]
      pure unit)
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
