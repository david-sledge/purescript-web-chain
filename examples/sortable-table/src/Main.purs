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
import Web.Chain.DOM (doc, nd, ndM, txn, (+<))
import Web.Chain.Event (onChange, onReady_)
import Web.Chain.HTML (checkbox)
import Web.Chain.UI.UISortableTable (changeSortOrder, getSortOrder, mkSortableTable, updateRowsByColName)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLInputElement (checked)

data Col
  = ColBool Boolean
  | ColString (Maybe String)
  | ColInt (Maybe Int)
  | ColDate (Maybe Instant)

derive instance eqCol ∷ Eq Col

colString' ∷ String → Col
colString' = ColString <<< Just

colInt' ∷ Int → Col
colInt' = ColInt <<< Just

colDate' ∷ Instant → Col
colDate' = ColDate <<< Just

withColBool ∷ Col → Boolean
withColBool (ColBool b) = b
withColBool _ = false

withColString ∷ Col → Maybe String
withColString (ColString ms) = ms
withColString _ = Nothing

withColInt ∷ Col → Maybe Int
withColInt (ColInt mi) = mi
withColInt _ = Nothing

withColDate ∷ Col → Maybe Instant
withColDate (ColDate md) = md
withColDate _ = Nothing

instance colOrd ∷ Ord Col where
  compare (ColString b1) (ColString b2) = compare b1 b2
  compare (ColBool b1) (ColBool b2) = compare b1 b2
  compare (ColInt b1) (ColInt b2) = compare b1 b2
  compare (ColDate b1) (ColDate b2) = compare b1 b2
  compare _ _ = EQ

main ∷ Effect Unit
main = onReady_ $ \_ → do
  (liftEffect $ body =<< doc) >>= maybe
    (liftEffect <<< throwException $ error "No document body")
    ( \bodyElem → do
        instant ← liftEffect now
        tbl ←
          mkSortableTable [ "table", "table-striped", "table-bordered", "table-condensed", "table-hover" ]
            [ "bool"
                /\
                  { classNames: [ "text-center" ]
                  , formatter: \key mBool table → do
                      ndM $ checkbox [] (maybe false withColBool mBool) (Just \ chkbx ->
                        ( \_ → getSortOrder table >>=
                            ( \_ →
                                updateRowsByColName
                                  [ ( key /\ maybe (pure Nothing)
                                        ( \dat →
                                            Just <<< flip (M.insert "bool") dat <<< ColBool <$> checked chkbx
                                        )
                                    )
                                  ]
                                  table
                            )
                        )) --}
                  , heading: (txn "Boolean Column" /\ [ "text-center" ])
                  }
            , "string"
                /\
                  { classNames: []
                  , formatter: \_ mString _ → txn <<< fromMaybe "\x2014" $ flip bind withColString mString
                  , heading: (txn "String Column" /\ [])
                  }
            , "int"
                /\
                  { classNames: [ "text-end" ]
                  , formatter: \_ mInt _ → txn <<< maybe "\x2014" show $ flip bind withColInt mInt
                  , heading: (txn "Int Column" /\ [ "text-end" ])
                  }
            , "date"
                /\
                  { classNames: []
                  , formatter: \_ mDateTime _ →
                      txn <<<
                        maybe
                          "\x2014"
                          ( either
                              (const "\x2014")
                              identity
                              <<< formatDateTime "YYYY-MM-DD HH:mm"
                              <<< toDateTime
                          )
                        $ flip bind withColDate mDateTime
                  , heading: (txn "Date Column" /\ [])
                  }
            ]
            >>= updateRowsByColName
              [ ( 1 /\ \_ → pure <<< Just $ M.fromArray
                    [ "bool" /\ ColBool false
                    , "string" /\ colString' "Text"
                    , "int" /\ colInt' 0
                    , "date" /\ colDate' instant
                    ]
                )
              , ( 2 /\ \_ → pure <<< Just $ M.fromArray
                    [ "bool" /\ ColBool true
                    , "string" /\ ColString Nothing
                    , "int" /\ ColInt Nothing
                    , "date" /\ ColDate Nothing
                    ]
                )
              ]
            >>= updateRowsByColName
              [ ( 2 /\ \_ → pure <<< Just $ M.fromArray
                    [ "bool" /\ ColBool true
                    , "string" /\ colString' "Sequence of characters"
                    , "int" /\ ColInt Nothing
                    , "date" /\ colDate' instant
                    ]
                )
              , ( 3 /\ \_ → pure <<< Just $ M.fromArray
                    [ "bool" /\ ColBool false
                    , "string" /\ colString' "String"
                    , "int" /\ colInt' (-3)
                    , "date" /\ ColDate Nothing
                    ]
                )
              ]
            >>= updateRowsByColName
              [ (1 /\ \_ → pure $ Nothing)
              ]
            -- >>= changeSortOrder [ "string" /\ true ]
            >>= updateRowsByColName
              [ (1 /\ \_ → pure <<< Just $ M.fromArray
                    [ "bool" /\ ColBool false
                    , "string" /\ colString' "Text"
                    , "int" /\ colInt' 1
                    , "date" /\ colDate' instant
                    ])
              ]
        void $ bodyElem +< [ nd tbl ]
    )
