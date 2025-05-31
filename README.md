# Web Chain

Web Chain is a library to simplify DOM manipulation. To create the following page snippet (snagged from the landing page of the [PureScript web site](https://www.purescript.org/)):

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

Web Chain allows it to be written as:

```purescript
snippetFromPurescriptLandingPageWithWebChain :: Effect Node
snippetFromPurescriptLandingPageWithWebChain =
  eln "div" []
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
