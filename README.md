# Web Chain

Web Chain is a library to simplify DOM manipulation. To create the following:

```html
<div>
  <h3>Benefits</h3>

  <ul>
    <li>Compile to readable JavaScript and reuse existing JavaScript code easily</li>
    <li>An extensive collection of libraries for development of web applications, web servers, apps and more</li>
    <li>Excellent tooling and editor support with instant rebuilds</li>
    <li>An active community with many learning resources</li>
    <li>Build real-world applications using functional techniques and expressive types, such as:
      <ul>
        <li>Algebraic data types and pattern matching</li>
        <li>Row polymorphism and extensible records</li>
        <li>Higher kinded types</li>
        <li>Type classes with functional dependencies</li>
        <li>Higher-rank polymorphism</li>
      </ul>
    </li>
  </ul>
</div>
```

using the [DOM API](https://www.w3.org/TR/REC-DOM-Level-1/cover.html) would result in:

```purescript
snippetFromPurescriptLandingPage :: Effect Node
snippetFromPurescriptLandingPage = do
  win <- window
  htmlDoc <- document win
  let doc = toDocument htmlDoc
  div <- Web.DOM.Element.toNode <$> createElement "div" doc
  h3 <- Web.DOM.Element.toNode <$> createElement "h3" doc
  appendChild h3 div
  text <- Web.DOM.Text.toNode <$> createTextNode "Benefits" doc
  appendChild text h3
  let newUl = Web.DOM.Element.toNode <$> createElement "ul" doc
  ulOuter <- newUl
  appendChild ulOuter div
  let newLi = Web.DOM.Element.toNode <$> createElement "li" doc
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- Web.DOM.Text.toNode <$> createTextNode "Compile to readable JavaScript and reuse existing JavaScript code easily" doc
  appendChild text liOuter
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- Web.DOM.Text.toNode <$> createTextNode "An extensive collection of libraries for development of web applications, web servers, apps and more" doc
  appendChild text liOuter
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- Web.DOM.Text.toNode <$> createTextNode "Excellent tooling and editor support with instant rebuilds" doc
  appendChild text liOuter
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- Web.DOM.Text.toNode <$> createTextNode "An active community with many learning resources" doc
  appendChild text liOuter
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- Web.DOM.Text.toNode <$> createTextNode "An active community with many learning resources" doc
  appendChild text liOuter
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- Web.DOM.Text.toNode <$> createTextNode "Build real-world applications using functional techniques and expressive types, such as:" doc
  appendChild text liOuter
  ulInner <- newUl
  appendChild ulInner liOuter
  liInner <- newLi
  appendChild liInner ulInner
  text <- Web.DOM.Text.toNode <$> createTextNode "Algebraic data types and pattern matching" doc
  appendChild text liInner
  liInner <- newLi
  appendChild liInner ulInner
  text <- Web.DOM.Text.toNode <$> createTextNode "Row polymorphism and extensible records" doc
  appendChild text liInner
  liInner <- newLi
  appendChild liInner ulInner
  text <- Web.DOM.Text.toNode <$> createTextNode "Higher kinded types" doc
  appendChild text liInner
  liInner <- newLi
  appendChild liInner ulInner
  text <- Web.DOM.Text.toNode <$> createTextNode "Type classes with functional dependencies" doc
  appendChild text liInner
  liInner <- newLi
  appendChild liInner ulInner
  text <- Web.DOM.Text.toNode <$> createTextNode "Higher-rank polymorphism" doc
  appendChild text liInner
  pure div
```

Web Chain condenses it to:

```purescript
snippetFromPurescriptLandingPageWithWebChain :: Effect Element
snippetFromPurescriptLandingPageWithWebChain =
  el "div" []
    [ eln "h3" [] [ txn "Benefits" ]

    , eln "ul" []
      [ eln "li" [] [ txn "Compile to readable JavaScript and reuse existing JavaScript code easily" ]
      , eln "li" [] [ txn "An extensive collection of libraries for development of web applications, web servers, apps and more" ]
      , eln "li" [] [ txn "Excellent tooling and editor support with instant rebuilds" ]
      , eln "li" [] [ txn "An active community with many learning resources" ]
      , eln "li" [] [ txn "Build real-world applications using functional techniques and expressive types, such as:"
        , eln "ul" []
          [ eln "li" [] [ txn "Algebraic data types and pattern matching" ]
          , eln "li" [] [ txn "Row polymorphism and extensible records" ]
          , eln "li" [] [ txn "Higher kinded types" ]
          , eln "li" [] [ txn "Type classes with functional dependencies" ]
          , eln "li" [] [ txn "Higher-rank polymorphism" ]
          ]
        ]
      ]
    ]
```

## Examples

The above example and others can be found in the [examples directory](https://github.com/david-sledge/purescript-web-chain/tree/main/examples). From the base folder they can be built using `npm run example-<name of example>`. The resulting HTML page in the example's dist directory can then be loaded in the browser.
