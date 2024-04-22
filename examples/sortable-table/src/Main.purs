module Main
  ( main
  )
  where

import Prelude

import Data.Array (snoc, updateAt)
import Data.Array.Mutable as A
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Effect.Now (now)
import Unsafe.Coerce (unsafeCoerce)
import Web.Chain.DOM (doc, el, eln, nd, ndM, txn, (+<))
import Web.Chain.Event (onChange, onReady_)
import Web.Chain.UI.UISortableTable (mkSortableTable, updateRowData)
import Web.DOM (Element)
import Web.HTML (HTMLInputElement)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLInputElement (checked)

data Col =
  ColBool Boolean
  | ColString (Maybe String)
  | ColInt (Maybe Int)
  | ColDate (Maybe Instant)

derive instance eqCol :: Eq Col

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

instance colOrd :: Ord Col where
  compare (ColString b1) (ColString b2) = compare b1 b2
  compare (ColBool b1) (ColBool b2) = compare b1 b2
  compare (ColInt b1) (ColInt b2) = compare b1 b2
  compare (ColDate b1) (ColDate b2) = compare b1 b2
  compare _ _ = EQ

main ∷ Effect Unit
main = onReady_ $ \ _ → do
  (liftEffect $ body =<< doc) >>= maybe
    (liftEffect <<< throwException $ error "No document body")
    (\ bodyElem → do
      instant <- liftEffect now
      tbl <- mkSortableTable ["table", "table-striped", "table-bordered", "table-condensed", "table-hover"] [{
          classNames: ["text-center"],
          formatter: \ key mBool table → do
            (checkbox :: HTMLInputElement) <- unsafeCoerce <$> el "input" (
              let attrs = ["type" /\ "checkbox"] in
              if maybe false withColBool mBool
              then snoc attrs ("checked" /\ "checked")
              else attrs) []
            ndM $ checkbox # onChange
              (\ _ -> void $ updateRowData [
                  (key /\ maybe (pure Nothing) (\ dat ->
                      flip (updateAt 0) dat <<< ColBool <$> checked checkbox
                    ))
                ] table
              ),
          heading: (txn "Boolean Column" /\ ["text-center"])
        }, {
          classNames: [],
          formatter: \ _ mString _ -> txn <<< fromMaybe "\x2014" $ flip bind withColString mString,
          heading: (txn "String Column" /\ [])
        }, {
          classNames: ["text-end"],
          formatter: \ _ mInt _ -> txn <<< maybe "\x2014" show $ flip bind withColInt mInt,
          heading: (txn "Int Column" /\ ["text-end"])
        }, {
          classNames: [],
          formatter: \ _ mDateTime _ -> txn <<< maybe "\x2014" (either (const "\x2014") identity <<< formatDateTime "YYYY-MM-DD HH:mm" <<< toDateTime) $ flip bind withColDate mDateTime,
          heading: (txn "Date Column" /\ [])
        }] >>= updateRowData [
          (1 /\ \ _ → pure $ Just [
            ColBool false,
            colString' "Text",
            colInt' 0,
            colDate' instant
          ]), (2 /\ \ _ → pure $ Just [
            ColBool true,
            ColString Nothing,
            ColInt Nothing,
            ColDate Nothing
          ])
        ] >>= updateRowData [
          (2 /\ \ _ → pure $ Just [
            ColBool true,
            colString' "Sequence of characters",
            ColInt Nothing,
            colDate' instant
          ]), (3 /\ \ _ → pure $ Just [
            ColBool false,
            colString' "String",
            colInt' (-3),
            ColDate Nothing
          ])
        ] >>= updateRowData [
          (1 /\ \ _ → pure $ Nothing)
        ] -- >>= changeSortOrder [11 /\ true] --}
      void $ bodyElem +< [nd tbl]
    )
