module Web.Chain.UI.UISortableTable
  ( ColSpec
  , SortableTable
  , changeSortOrder
  , clearTable
  , mkSortableTable
  , sortTable
  , updateRowData
  )
  where

import Prelude

import Data.Array (snoc, sortBy)
import Data.Foldable (class Foldable, foldM, foldl, indexl, intercalate, length, traverse_)
import Data.HashMap as M
import Data.HashSet as S
import Data.Hashable (class Hashable)
import Data.Map.Mutable as MM
import Data.Maybe (Maybe, maybe)
import Data.Ordering (invert)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Chain (class EventTargetOp, el, eln, empty, nd, ndM, onM, remove)
import Web.Chain.HTML (td, tr)
import Web.DOM (Element, Node)
import Web.DOM.Class.ElementOp (class ElementOp)
import Web.DOM.Class.NodeOp (class NodeOp, appendChild)
import Web.HTML (HTMLTableRowElement)
import Web.HTML.Class.HTMLElementOp (class HTMLElementOp)

foreign import data SortableTable ∷
  Type →
  Type →
  (Type → Type) →
  (Type → Type) →
  (Type → Type) →
  Type

instance EventTargetOp (SortableTable k a f1 f2 f3) where
  toEventTarget = unsafeCoerce

instance NodeOp (SortableTable k a f1 f2 f3) where
  toNode = unsafeCoerce

instance ElementOp (SortableTable k a f1 f2 f3) where
  toElement = unsafeCoerce

instance HTMLElementOp (SortableTable k a f1 f2 f3) where
  toHTMLElement = unsafeCoerce

type ColSpec k a f1 f2 f3 = {
    classNames ∷ f2 String,
    formatter ∷ k -> Maybe a -> SortableTable k a f1 f2 f3 → Effect Node,
    heading ∷ (Effect Node /\ f3 String)
  }

foreign import _setColSpecs ∷ ∀ k a f1 f2 f3.
  f1 (ColSpec k a f1 f2 f3) → SortableTable k a f1 f2 f3 → Effect Unit
foreign import _getColSpecs ∷ ∀ k a f1 f2 f3.
  SortableTable k a f1 f2 f3 → Effect (f1 (ColSpec k a f1 f2 f3))

foreign import _setDataTableBody ∷ ∀ k a f1 f2 f3.
  Element → SortableTable k a f1 f2 f3 → Effect Unit
foreign import _getDataTableBody ∷ ∀ k a f1 f2 f3.
  SortableTable k a f1 f2 f3 → Effect (Element)

foreign import _setSortOrder ∷ ∀ k a f1 f2 f3.
  Array (Int /\ Boolean) → SortableTable k a f1 f2 f3 → Effect Unit
foreign import _getSortOrder ∷ ∀ k a f1 f2 f3.
  SortableTable k a f1 f2 f3 → Effect (Array (Int /\ Boolean))

foreign import _setTableData ∷ ∀ k a f1 f2 f3.
  MM.MMap k (HTMLTableRowElement /\ Array a) →
  SortableTable k a f1 f2 f3 →
  Effect Unit
foreign import _getTableData ∷ ∀ k a f1 f2 f3.
  SortableTable k a f1 f2 f3 →
  Effect (MM.MMap k (HTMLTableRowElement /\ Array a))

getSortOrder ∷ ∀ m k a f1 f2 f3.
  MonadEffect m ⇒
  SortableTable k a f1 f2 f3 →
  m (Array (Int /\ Boolean))
getSortOrder table = liftEffect $ _getSortOrder table

sortTable ∷ ∀ m f2 f3 a f1 k.
  MonadEffect m ⇒
  Ord a ⇒
  Hashable k ⇒
  Foldable f1 ⇒
  SortableTable k a f1 f2 f3 →
  m (SortableTable k a f1 f2 f3)
sortTable table = do
  tableBody ← liftEffect $ _getDataTableBody table
  sortOrder ← getSortOrder table
  liftEffect (_getTableData table) >>=
    MM.freeze >>=
    traverse_ (flip appendChild tableBody <<< fst) <<<
      sortBy (\ (_ /\ rowData1) (_ /\ rowData2) →
        let f weight =
              maybe
                EQ
                (\ (col /\ isAsc) →
                  let next = f $ weight + 1
                      dirF = if isAsc then identity else invert
                      mRec2 = indexl col $ rowData2
                  in
                  maybe
                    (maybe next (const $ dirF LT) mRec2)
                    (\ rec1 →
                      maybe
                        (dirF GT)
                        (\ rec2 →
                          case compare rec1 rec2 of
                            EQ → next
                            ord → dirF ord
                        )
                        mRec2
                    ) $
                    indexl col rowData1
                ) $
                indexl weight sortOrder
        in
        f 0
      ) <<< M.values
  pure table

-- \x25b2 \x25bc

changeSortOrder ∷ ∀ m f f2 f3 a f1 k.
  Foldable f1 ⇒
  MonadEffect m ⇒
  Ord a ⇒
  Foldable f ⇒
  Hashable k ⇒
  f (Int /\ Boolean) →
  SortableTable k a f1 f2 f3 →
  m (SortableTable k a f1 f2 f3)
changeSortOrder newOrder table = do
  colCount ← length <$> (liftEffect $ _getColSpecs table)
  let (newSortOrder /\ _) = foldl (\ (sortOrder /\ set) (col /\ dir) →
        if col < colCount && not (S.member col set)
        then (snoc sortOrder (col /\ dir) /\ S.insert col set)
        else (sortOrder /\ set)
        ) ([] /\ S.empty) newOrder
  liftEffect $ _setSortOrder newSortOrder table
  sortTable table

mkSortableTable ∷ ∀ m f k a f1 f2 f3.
  Foldable f1 ⇒
  MonadEffect m ⇒
  Ord a ⇒
  Foldable f3 ⇒
  Foldable f ⇒
  Hashable k ⇒
  f String →
  f1 (ColSpec k a f1 f2 f3) →
  m (SortableTable k a f1 f2 f3)
mkSortableTable classNames colSpecs = do
  ((sortOrder /\ headerCells) /\ _) <- foldM (\ ((sortOrder /\ headerCells) /\ col) colSpec → do
    th <- el
      "th"
      ["class" /\ intercalate " " (snd colSpec.heading)]
      [liftEffect $ fst colSpec.heading]
    pure ((snoc sortOrder (col /\ true) /\ snoc headerCells (nd th)) /\ (col + 1))
    ) (([] /\ []) /\ 0) colSpecs
  tableBody ← el "tbody" [] []
  table ← unsafeCoerce <$> el "table" ["class" /\ intercalate " " classNames] [
    eln "thead" [] [
      eln "tr" [] headerCells
    ],
    nd tableBody
  ]
  void $ foldM (\ col cell -> onM "click" (\ _ -> do
      sortOrder' <- getSortOrder table
      void $ changeSortOrder (foldl
          (\ acc s@(col' /\ _) -> if col' == col then acc else snoc acc s)
          [col /\ maybe true (\ (col' /\ isAsc) ->
            if col' == col
            then not isAsc
            else isAsc) (indexl 0 sortOrder')]
          sortOrder') table
      ) cell *> pure (col + 1)) 0 headerCells
  liftEffect $ _setColSpecs colSpecs table
  liftEffect $ _setDataTableBody tableBody table
  liftEffect $ _setSortOrder sortOrder table
  tableData ← MM.new
  liftEffect $ _setTableData tableData table
  pure table

updateRowData ∷ ∀ m f f2 f3 a f1 k.
  MonadEffect m ⇒
  Ord a ⇒
  Foldable f ⇒
  Foldable f1 ⇒
  Foldable f2 ⇒
  Hashable k ⇒
  f (k /\ (Maybe (Array a) → Effect (Maybe (Array a)))) →
  SortableTable k a f1 f2 f3 →
  m (SortableTable k a f1 f2 f3)
updateRowData updateFs table = do
  colSpecs ← liftEffect $ _getColSpecs table
  tableData ← liftEffect $ _getTableData table
  -- when false $
  traverse_ (\ (key /\ updateF) → do
      mRowData ← MM.lookup key tableData
      void $ maybe (pure unit) (void <<< remove <<< fst) mRowData
      liftEffect (updateF $ map snd mRowData) >>=
        maybe
          (MM.delete key tableData)
          (\ newData → do
            newRowUi ← tr [] <<< snd $ foldl
              (\ (col /\ acc) colSpec →
                (col + 1) /\
                  snoc acc
                    (ndM $ td
                      ["class" /\ intercalate " " (colSpec.classNames)]
                      [liftEffect $ colSpec.formatter key (indexl col newData) table]))
              (0 /\ [])
              colSpecs
            MM.insert key (newRowUi /\ newData) tableData
          )
    ) updateFs
  sortTable table

clearTable ∷ ∀ m k a f1 f2 f3.
  MonadEffect m ⇒
  SortableTable k a f1 f2 f3 →
  m (SortableTable k a f1 f2 f3)
clearTable table = liftEffect do
  tableData <- _getTableData table
  MM.clear tableData
  tableBody <- _getDataTableBody table
  void $ empty tableBody
  pure table
