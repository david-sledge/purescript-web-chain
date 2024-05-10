module Web.Chain.UI.UISortableTable
  ( ColSpec
  , UISortableTable
  , changeSortOrder
  , clearTable
  , getColumnNames
  , getSortOrder
  , mkSortableTable
  , mkSortableTableNoNames
  , sortTable
  , updateRowsByColPos
  , updateRowsByColName
  ) where

import Prelude hiding (div)

import Data.Array (drop, cons, snoc, sortBy)
import Data.Foldable (class Foldable, foldM, foldl, indexl, lookup, traverse_)
import Data.HashMap as M
import Data.HashSet as S
import Data.Hashable (class Hashable)
import Data.Map.Mutable as MM
import Data.Maybe (Maybe, maybe)
import Data.Number (pow)
import Data.Int (toNumber)
import Data.Ordering (invert)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Chain (class EventTargetOp, appendNodesM, el, eln, empty, nd, ndM, onM, remove, txn)
import Web.Chain.CSSOM (classAttr, conceal, revealM, setCssPropM, styleAttr)
import Web.Chain.HTML (div, span, table, td, th, tr)
import Web.DOM (Element, Node)
import Web.DOM.Class.ElementOp (class ElementOp)
import Web.DOM.Class.NodeOp (class NodeOp, appendChild)
import Web.HTML (HTMLSpanElement, HTMLTableRowElement)
import Web.HTML.Class.HTMLElementOp (class HTMLElementOp)

foreign import data UISortableTable
  ∷ Type
  → Type
  → (Type → Type)
  → (Type → Type)
  → Type

instance EventTargetOp (UISortableTable k a f1 f2) where
  toEventTarget = unsafeCoerce

instance NodeOp (UISortableTable k a f1 f2) where
  toNode = unsafeCoerce

instance ElementOp (UISortableTable k a f1 f2) where
  toElement = unsafeCoerce

instance HTMLElementOp (UISortableTable k a f1 f2) where
  toHTMLElement = unsafeCoerce

type ColSpec k a f1 f2 =
  { classNames ∷ f1 String
  , formatter ∷ k → Maybe a → UISortableTable k a f1 f2 → Effect Node
  , heading ∷ (Effect Node /\ f2 String)
  }

foreign import _setColSpecs
  ∷ ∀ k a f1 f2
  . Array (String /\ (ColSpec k a f1 f2 /\ HTMLSpanElement))
  → UISortableTable k a f1 f2
  → Effect Unit

foreign import _getColSpecs
  ∷ ∀ k a f1 f2
  . UISortableTable k a f1 f2
  → Effect (Array (String /\ (ColSpec k a f1 f2 /\ HTMLSpanElement)))

foreign import _setDataTableBody
  ∷ ∀ k a f1 f2
  . Element
  → UISortableTable k a f1 f2
  → Effect Unit

foreign import _getDataTableBody
  ∷ ∀ k a f1 f2
  . UISortableTable k a f1 f2
  → Effect (Element)

foreign import _setSortOrder
  ∷ ∀ k a f1 f2
  . Array (String /\ Boolean)
  → UISortableTable k a f1 f2
  → Effect Unit

foreign import _getSortOrder
  ∷ ∀ k a f1 f2
  . UISortableTable k a f1 f2
  → Effect (Array (String /\ Boolean))

foreign import _setTableData
  ∷ ∀ k a f1 f2
  . MM.MMap k (HTMLTableRowElement /\ M.HashMap String a)
  → UISortableTable k a f1 f2
  → Effect Unit

foreign import _getTableData
  ∷ ∀ k a f1 f2
  . UISortableTable k a f1 f2
  → Effect (MM.MMap k (HTMLTableRowElement /\ M.HashMap String a))

getSortOrder
  ∷ ∀ m k a f1 f2
  . MonadEffect m
  ⇒ UISortableTable k a f1 f2
  → m (Array (String /\ Boolean))
getSortOrder table = liftEffect $ _getSortOrder table

getColumnNames
  ∷ ∀ m k a f1 f2
  . MonadEffect m
  ⇒ UISortableTable k a f1 f2
  → m (Array String)
getColumnNames table = liftEffect $ map fst <$> _getColSpecs table

sortTable
  ∷ ∀ m k a f1 f2
  . MonadEffect m
  ⇒ Hashable k
  ⇒ Ord a
  ⇒ UISortableTable k a f1 f2
  → m (UISortableTable k a f1 f2)
sortTable table = do
  tableBody ← liftEffect $ _getDataTableBody table
  sortOrder ← getSortOrder table
  liftEffect (_getTableData table)
    >>= MM.freeze
    >>=
      traverse_ (flip appendChild tableBody <<< fst)
        <<< sortBy
          ( \(_ /\ rowData1) (_ /\ rowData2) →
              let
                f weight =
                  maybe
                    EQ
                    ( \(name /\ isAsc) →
                        let
                          next = f $ weight + 1
                          dirF = if isAsc then identity else invert
                          mRec2 = M.lookup name rowData2
                        in
                          maybe
                            (maybe next (const $ dirF LT) mRec2)
                            ( \rec1 →
                                maybe
                                  (dirF GT)
                                  ( \rec2 →
                                      case compare rec1 rec2 of
                                        EQ → next
                                        ord → dirF ord
                                  )
                                  mRec2
                            ) $
                            M.lookup name rowData1
                    ) $
                    indexl weight sortOrder
              in
                f 0
          )
        <<< M.values
  pure table

changeSortOrder
  ∷ ∀ m k a f1 f2 f3
  . MonadEffect m
  ⇒ Hashable k
  ⇒ Ord a
  ⇒ Foldable f3
  ⇒ f3 (String /\ Boolean)
  → UISortableTable k a f1 f2
  → m (UISortableTable k a f1 f2)
changeSortOrder newOrder table = do
  colSpecs ← liftEffect $ _getColSpecs table
  (newSortOrder /\ nameSet) <- foldWithItemCountM
    ( \cnt (sortOrder /\ set) (name /\ dir) →
        maybe (pure $ sortOrder /\ set)
          ( \(_ /\ sortDirUi) ->
              if S.member name set then pure $ sortOrder /\ set
              else do
                void $ empty sortDirUi
                  # appendNodesM [ (txn $ if dir then "\x25b2" else "\x25bc") ]
                  # revealM
                  # setCssPropM "opacity" (show $ 0.9 / (pow 2.0 $ toNumber cnt) + 0.1)
                pure $ snoc sortOrder (name /\ dir) /\ S.insert name set
          ) $ lookup name colSpecs
    )
    ([] /\ S.empty)
    newOrder
  traverse_
    ( \(name /\ (_ /\ sortDirUi)) ->
        unless (S.member name nameSet) <<< void $ conceal sortDirUi
    )
    colSpecs
  liftEffect $ _setSortOrder newSortOrder table
  sortTable table

foldlWithItemCount ∷ ∀ b f a. Foldable f ⇒ (Int → b → a → b) → b → f a → b
foldlWithItemCount f init = snd <<< foldl
  (\(cnt /\ acc) item → ((cnt + 1) /\ f cnt acc item))
  (0 /\ init)

foldWithItemCountM
  ∷ ∀ b f a m
  . Monad m
  ⇒ Foldable f
  ⇒ (Int → b → a → m b)
  → b
  → f a
  → m b
foldWithItemCountM f init = map snd <<< foldM
  ( \(cnt /\ acc) item → do
      acc' ← f cnt acc item
      pure ((cnt + 1) /\ acc')
  )
  (0 /\ init)

mkSortableTable_
  ∷ ∀ m k a f1 f2 f3 f4
   . MonadEffect m
  => Hashable k
  => Ord a
  => Foldable f2
  => Foldable f3
  => Foldable f4
  => (String -> Int -> Array (String /\ (ColSpec k a f1 f2 /\ HTMLSpanElement)) -> String)
  -> f3 String
  -> f4 (String /\ ColSpec k a f1 f2)
  -> m (UISortableTable k a f1 f2)
mkSortableTable_ f classNames colSpecs = do
  tRow ← tr [] []
  tableBody ← el "tbody" [] []
  tbl ← unsafeCoerce <$> table ( [] # classAttr classNames )
    [ eln "thead" []
        [ nd tRow
        ]
    , nd tableBody
    ]
  (sortOrder /\ newColSpecs) ←
    foldWithItemCountM
      ( \col (sortOrder /\ colSpecs') (name /\ colSpec) → do
          sortDirUi <- span ( [] # styleAttr ["opacity" /\ (show $ 0.9 / (pow 2.0 $ toNumber col) + 0.1)] ) [ txn "\x25b2" ]
          th
            ([] # classAttr (snd colSpec.heading))
            [ div ([] # styleAttr [ "display" /\ "inline-flex" ])
                [ div [] [ liftEffect $ fst colSpec.heading ] # ndM
                , span ( [] # styleAttr ["white-space" /\ "pre"] ) [ txn " " ] # ndM
                , nd sortDirUi
                ]
                # ndM
            ]
            # onM "click"
                ( const do
                    sortOrder' ← getSortOrder tbl
                    let changeSort sort = void $ changeSortOrder sort tbl
                    maybe
                      (changeSort [ name /\ true ])
                      ( \(name' /\ isAsc) →
                          if name == name'
                          -- when the column selected is already the primary sort column,
                          -- the ascending/descending indicator is flipped
                          then changeSort <<< cons (name /\ not isAsc) $ drop 1 sortOrder'
                          -- otherwise, the clicked column is becomes the primary sort column
                          -- without chaning ascending or descending
                          else
                            let
                              (newSortOrder /\ hasCol) =
                                foldl
                                  ( \(acc /\ hasCol) s@(name_ /\ _) →
                                      if name == name_ then (cons s acc /\ true)
                                      else (snoc acc s /\ hasCol)
                                  )
                                  ([] /\ false)
                                  sortOrder'
                            in
                              changeSort
                                if hasCol then newSortOrder
                                else cons (name /\ true) newSortOrder
                      )
                      $ indexl 0 sortOrder'
                )
            >>= flip appendChild tRow
          let name' = f name col colSpecs'
          pure (snoc sortOrder (name' /\ true) /\ snoc colSpecs' (name' /\ (colSpec /\ sortDirUi)))
      )
      ([] /\ [])
      colSpecs
  tableData ← MM.new
  liftEffect $ _setColSpecs newColSpecs tbl
  liftEffect $ _setDataTableBody tableBody tbl
  liftEffect $ _setSortOrder sortOrder tbl
  liftEffect $ _setTableData tableData tbl
  pure tbl

mkSortableTable
  ∷ ∀ m k a f1 f2 f3 f4
  . MonadEffect m
  ⇒ Hashable k
  ⇒ Ord a
  ⇒ Foldable f2
  ⇒ Foldable f3
  ⇒ Foldable f4
  ⇒ f3 String
  → f4 (String /\ ColSpec k a f1 f2)
  → m (UISortableTable k a f1 f2)
mkSortableTable classNames colSpecs = mkSortableTable_
  ( \name _ colSpecs' →
      maybe name
        ( \_ →
            let
              f num =
                let
                  numName = name <> "_" <> show num
                in
                  maybe numName (const <<< f $ num + 1) $ lookup numName colSpecs
            in
              f 0
        ) $ lookup name colSpecs'
  )
  classNames
  colSpecs

mkSortableTableNoNames
  ∷ ∀ m k a f1 f2 f3 f4
  . MonadEffect m
  ⇒ Hashable k
  ⇒ Ord a
  ⇒ Foldable f2
  ⇒ Foldable f4
  ⇒ Foldable f3
  ⇒ f3 String
  → f4 (String /\ ColSpec k a f1 f2)
  → m (UISortableTable k a f1 f2)
mkSortableTableNoNames classNames colSpecs =
  mkSortableTable_ (\_ col _ → "_" <> show col) classNames colSpecs

updateRows
  ∷ ∀ m k a f1 f2 f3 b c
  . MonadEffect m
  ⇒ Hashable k
  ⇒ Ord a
  ⇒ Foldable f1
  ⇒ Foldable f3
  ⇒ ( Array (String /\ (ColSpec k a f1 f2 /\ HTMLSpanElement))
      → (HTMLTableRowElement /\ M.HashMap String a)
      → b
    )
  → ( Array (String /\ (ColSpec k a f1 f2 /\ HTMLSpanElement))
      → c
      → M.HashMap String a
    )
  → f3 (k /\ (Maybe b → Effect (Maybe c)))
  → UISortableTable k a f1 f2
  → m (UISortableTable k a f1 f2)
updateRows f g updateFs table = do
  colSpecs ← liftEffect $ _getColSpecs table
  tableData ← liftEffect $ _getTableData table
  traverse_
    ( \(key /\ updateF) → do
        mRowData ← MM.lookup key tableData
        void $ maybe (pure unit) (void <<< remove <<< fst) mRowData
        liftEffect (updateF $ f colSpecs <$> mRowData) >>=
          maybe
            (MM.delete key tableData)
            ( \newArrayedData → do
                let newRowData = g colSpecs newArrayedData
                newRowUi ← tr [] $ foldl
                  ( \acc (name /\ (colSpec /\ _)) →
                      snoc acc
                        ( td
                            ( [] # classAttr colSpec.classNames )
                            [ liftEffect $ colSpec.formatter key (M.lookup name newRowData) table ]
                            # ndM
                        )
                  )
                  []
                  colSpecs
                MM.insert key (newRowUi /\ newRowData) tableData
            )
    )
    updateFs
  sortTable table

updateRowsByColPos
  ∷ ∀ m k a f1 f2 f3
  . MonadEffect m
  ⇒ Hashable k
  ⇒ Ord a
  ⇒ Foldable f1
  ⇒ Foldable f3
  ⇒ f3 (k /\ (Maybe (Array (Maybe a)) → Effect (Maybe (Array (Maybe a)))))
  → UISortableTable k a f1 f2
  → m (UISortableTable k a f1 f2)
updateRowsByColPos =
  updateRows
    ( \colSpecs (_ /\ rowData) →
        foldl
          ( \arrayedData (name /\ _) →
              snoc arrayedData $ M.lookup name rowData
          )
          []
          colSpecs
    )
    ( \colSpecs newArrayedData →
        foldlWithItemCount
          ( \col newRowData mColData →
              maybe newRowData
                ( \colData →
                    maybe newRowData
                      ( \(name /\ _) →
                          M.insert name colData newRowData
                      ) $ indexl col colSpecs
                )
                mColData
          )
          M.empty
          newArrayedData
    )

updateRowsByColName
  ∷ ∀ m k a f1 f2 f3
  . MonadEffect m
  ⇒ Hashable k
  ⇒ Ord a
  ⇒ Foldable f1
  ⇒ Foldable f3
  ⇒ f3 (k /\ (Maybe (M.HashMap String a) → Effect (Maybe (M.HashMap String a))))
  → UISortableTable k a f1 f2
  → m (UISortableTable k a f1 f2)
updateRowsByColName = updateRows (const $ snd) (const $ identity)

clearTable
  ∷ ∀ m k a f1 f2
  . MonadEffect m
  ⇒ UISortableTable k a f1 f2
  → m (UISortableTable k a f1 f2)
clearTable table = liftEffect do
  tableData ← _getTableData table
  MM.clear tableData
  tableBody ← _getDataTableBody table
  void $ empty tableBody
  pure table
