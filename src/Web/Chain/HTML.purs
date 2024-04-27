-- | Functions for specific types of HTML elements.
module Web.Chain.HTML
  ( button
  , check
  , checkbox
  , disable
  , div
  , enable
  , isEnabled
  , maxLen
  , minLen
  , setAutocomplete
  , setLenLimits
  , span
  , table
  , td
  , textField
  , th
  , tr
  , uncheck
  , val
  , valM
  )
  where

import Prelude

import Control.Bind (bindFlipped)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (class Foldable, intercalate)
import Data.Int (fromString, toNumber)
import Data.List (List(Nil), (:))
import Data.List.Util (s)
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Number.Format (toString)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error, throwException)
import Web.Chain.DOM (attrM, el, rmAttrM, setAttrsM)
import Web.DOM (Node)
import Web.DOM.Class.ElementOp (class ElementOp)
import Web.Event.Class.EventTargetOp (onM)
import Web.Event.Event (Event)
import Web.HTML (HTMLButtonElement, HTMLInputElement)
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLDivElement as HD
import Web.HTML.HTMLSpanElement as HS
import Web.HTML.HTMLInputElement (value)
import Web.HTML.HTMLInputElement as HI
import Web.HTML.HTMLTableCellElement as HTD
import Web.HTML.HTMLTableElement as HT
import Web.HTML.HTMLTableRowElement as HTR

-- | Create a plain ol' input field of type text with a default value.
textField ∷ ∀ m. MonadEffect m ⇒ String → m HTMLInputElement
textField defaultValue =
  maybe
    (liftEffect <<< throwException $ error "'Web.Chain.DOM.el \"input\" val' did not produce an HTMLInputElement")
    pure <<<
    HI.fromElement =<<
    el "input" [ ("value" /\ defaultValue), ("type" /\ "text") ] []

{-
text field functions:
autocomplete
disabled
id
maxlength
minlength
pattern
placeholder
readonly
required
value
isvalid
--}

-- | Set whether autocomplete is enabled for an input. The input is returned.
setAutocomplete ∷ ∀ m. MonadEffect m ⇒ String → m HTMLInputElement → m HTMLInputElement
setAutocomplete autocomplete mInput = do
  input ← mInput
  liftEffect $ HI.setAutocomplete autocomplete input
  pure input

-- | Enable an input. The input is returned.
enable ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ m e → m e
enable = rmAttrM "disabled"

-- | Disable an input. The input is returned.
disable ∷ ∀ e m. ElementOp e ⇒ MonadEffect m ⇒ m e → m e
disable = setAttrsM (s ("disabled" /\ "disabled"))

-- | Is this input enabled?
isEnabled ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ m e → m Boolean
isEnabled mInput =
  maybe
    (pure true)
    (pure <<< (/=) "disabled") =<< attrM "disabled" mInput

-- | Set the the range limits on the required number of characters of a text
-- | field. The text field is returned.
setLenLimits ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ Int → Maybe Int → m e → m e
setLenLimits min mMax mInput =
  either
    (\errMsgs → liftEffect <<< throwException <<< error $ intercalate "\n" errMsgs)
    (\attrs → setAttrsM attrs mInput) <<<
    either
      ( \errMsgs → Left $
          if min < 0 then "min may not be less than zero." : errMsgs
          else errMsgs
      )
      ( \attrs →
          if min < 0 then Left $ s "min may not be less than zero."
          else Right $
            if min > 0 then ("minlength" /\ toString (toNumber min)) : attrs
            else ("minlength" /\ "") : attrs
      ) $
    maybe
      (Right $ s ("maxlength" /\ ""))
      ( \max →
          if min > max then Left $ s "max may not be less than min."
          else if max < 1 then Left $ s "max may must be at least 1."
          else Right $ s ("maxlength" /\ toString (toNumber max))
      )
      mMax

-- | Get the max length setting.
maxLen ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ m e → m (Maybe Int)
maxLen mInput = maybe Nothing fromString <$> attrM "maxlength" mInput

-- | Get the min length setting.
minLen ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ m e → m Int
minLen mInput = maybe 0 (fromMaybe 0 <<< fromString) <$> attrM "minlength" mInput

-- | Get the text from a text field.
val ∷ ∀ m. MonadEffect m ⇒ HTMLInputElement → m String
val input = liftEffect $ value input

-- | Get the text from a text field.
valM ∷ ∀ m. MonadEffect m ⇒ m HTMLInputElement → m String
valM = bindFlipped val

testConversion :: forall m el. MonadEffect m => String -> String -> Maybe el -> m el
testConversion tag typeName = maybe (liftEffect <<< throwException $ error $ "'Web.Chain.DOM.el \"" <> tag <>"\"' did not produce " <> typeName) pure

-- | Create a button.
button ∷ ∀ f m a. MonadEffect m ⇒ Foldable f ⇒ f (m Node) → Maybe (Event → Effect a) → m HTMLButtonElement
button childNodesM mClick =
  el "button" Nil childNodesM # maybe identity (onM "click") mClick >>=
    testConversion "button" "HTMLButtonElement" <<< HB.fromElement

div ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HD.HTMLDivElement
div attributes children =
  el "div" attributes children >>=
    testConversion "div" "HTMLDivElement" <<< HD.fromElement

span ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HS.HTMLSpanElement
span attributes children =
  el "span" attributes children >>=
    testConversion "span" "HTMLSpanElement" <<< HS.fromElement

tr ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HTR.HTMLTableRowElement
tr attributes children =
  el "tr" attributes children >>=
    testConversion "tr" "HTMLTableRowElement" <<< HTR.fromElement

td ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HTD.HTMLTableCellElement
td attributes children =
  el "td" attributes children >>=
    testConversion "td" "HTMLTableCellElement" <<< HTD.fromElement

th ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HTD.HTMLTableCellElement
th attributes children =
  el "th" attributes children >>=
    testConversion "th" "HTMLTableCellElement" <<< HTD.fromElement

table ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HT.HTMLTableElement
table attributes children =
  el "table" attributes children >>=
    testConversion "table" "HTMLTableElement" <<< HT.fromElement

check :: forall m. MonadEffect m => HTMLInputElement -> m HTMLInputElement
check checkbx = do
  liftEffect $ HI.setChecked true checkbx
  pure checkbx

uncheck :: forall m. MonadEffect m => HTMLInputElement -> m HTMLInputElement
uncheck checkbx = do
  liftEffect $ HI.setChecked false checkbx
  pure checkbx

-- | Create a checkbox.
checkbox ∷ ∀ m a. MonadEffect m ⇒ Boolean → Maybe (Event → Effect a) → m HTMLInputElement
checkbox isChecked mChange =
  el "input" [ "type" /\ "checkbox" ] [] # maybe identity (onM "change") mChange >>=
    testConversion "button" "HTMLInputElement" <<< HI.fromElement >>= if isChecked then check else uncheck
