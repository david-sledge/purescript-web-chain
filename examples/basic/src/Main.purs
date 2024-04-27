module Main where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Web.Chain.DOM (doc, eln, txn, (+<))
import Web.Chain.Event (onReady_)
import Web.DOM (Element, Node)
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element as E
import Web.DOM.Node (appendChild)
import Web.DOM.Text as T
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (body, toDocument)

snippetFromPurescriptLandingPage :: Effect Node
snippetFromPurescriptLandingPage = do
  win <- window
  htmlDoc <- document win
  let doc = toDocument htmlDoc
      newElem tag = E.toNode <$> createElement tag doc
      newText string = T.toNode <$> createTextNode string doc
  div <- newElem "div"
  h3 <- newElem "h3"
  appendChild h3 div
  text <- newText "Benefits"
  appendChild text h3
  let newUl = newElem "ul"
  ulOuter <- newUl
  appendChild ulOuter div
  let newLi = newElem "li"
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- newText "Compile to readable JavaScript and reuse existing JavaScript code easily"
  appendChild text liOuter
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- newText "An extensive collection of libraries for development of web applications, web servers, apps and more"
  appendChild text liOuter
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- newText "Excellent tooling and editor support with instant rebuilds"
  appendChild text liOuter
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- newText "An active community with many learning resources"
  appendChild text liOuter
  liOuter <- newLi
  appendChild liOuter ulOuter
  text <- newText "Build real-world applications using functional techniques and expressive types, such as:"
  appendChild text liOuter
  ulInner <- newUl
  appendChild ulInner liOuter
  liInner <- newLi
  appendChild liInner ulInner
  text <- newText "Algebraic data types and pattern matching"
  appendChild text liInner
  liInner <- newLi
  appendChild liInner ulInner
  text <- newText "Row polymorphism and extensible records"
  appendChild text liInner
  liInner <- newLi
  appendChild liInner ulInner
  text <- newText "Higher kinded types"
  appendChild text liInner
  liInner <- newLi
  appendChild liInner ulInner
  text <- newText "Type classes with functional dependencies"
  appendChild text liInner
  liInner <- newLi
  appendChild liInner ulInner
  text <- newText "Higher-rank polymorphism"
  appendChild text liInner
  pure div

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

main ∷ Effect Unit
main = onReady_ $ \_ → do
  liftEffect $ log "Getting ready..."
  (liftEffect $ body =<< doc) >>= maybe
    (liftEffect <<< throwException $ error "No document body")
    ( \bodyElem →
        {--
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
        void $ bodyElem +<
          [ snippetFromPurescriptLandingPage
          , snippetFromPurescriptLandingPageWithWebChain
          ]
    )
