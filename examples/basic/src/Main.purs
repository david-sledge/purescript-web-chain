module Main where

import Prelude

import Effect (Effect)
import Web.Chain.DOM (eln, txn)
import Web.Chain.HTML (docBody)
import Web.Chain.Event (onReady_)
import Web.DOM (Node)
import Web.DOM.Class.NodeOp (appendChild)

{-
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
--}
snippetFromPurescriptLandingPageWithWebChain ∷ Effect Node
snippetFromPurescriptLandingPageWithWebChain =
  eln "div" []
    [ eln "h3" [] [ txn "Benefits" ]

    , eln "ul" []
        [ eln "li" [] [ txn "Compile to readable JavaScript and reuse existing JavaScript code easily" ]
        , eln "li" [] [ txn "An extensive collection of libraries for development of web applications, web servers, apps and more" ]
        , eln "li" [] [ txn "Excellent tooling and editor support with instant rebuilds" ]
        , eln "li" [] [ txn "An active community with many learning resources" ]
        , eln "li" []
            [ txn "Build real-world applications using functional techniques and expressive types, such as:"
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

main ∷ Effect Unit
main = onReady_ \_ → bind docBody <<< appendChild =<< snippetFromPurescriptLandingPageWithWebChain
