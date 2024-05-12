-- | Functions for specific types of HTML elements.
module Web.Chain.HTML
  ( button
  , check
  , checkbox
  , div
  , maxLen
  , minLen
  , numberField
  , passwordField
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
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (class Foldable, intercalate)
import Data.Int (fromString, toNumber)
import Data.List ((:))
import Data.List.Util (s)
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Number.Format (toString)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error, throwException)
import Web.Chain.DOM (attrM, el, setAttrsM)
import Web.DOM (Node)
import Web.DOM.Class.ElementOp (class ElementOp)
import Web.Event.Class.EventTargetOp (on)
import Web.Event.Event (Event)
import Web.HTML (HTMLButtonElement, HTMLInputElement)
import Web.HTML.HTMLButtonElement as HBu
import Web.HTML.HTMLDivElement as HD
import Web.HTML.HTMLSpanElement as HSp
import Web.HTML.HTMLInputElement (value)
import Web.HTML.HTMLInputElement as HIn
import Web.HTML.HTMLTableCellElement as HTD
import Web.HTML.HTMLTableElement as HT
import Web.HTML.HTMLTableRowElement as HTR

testCoercion ∷ ∀ m el. MonadEffect m ⇒ String → String → Maybe el → m el
testCoercion tag typeName = maybe (liftEffect <<< throwException $ error $ "'Web.Chain.DOM.el \"" <> tag <> "\"' did not produce " <> typeName) pure

-- | Create a plain ol' input field of type text with a default value.
textField ∷ ∀ m f. MonadEffect m ⇒ Foldable f ⇒ f (String /\ String) → String → m HTMLInputElement
textField attrs defaultValue =
  (el "input" attrs [] # setAttrsM [ "type" /\ "text", "value" /\ defaultValue ])
    >>= testCoercion "input" "HTMLInputElement" <<< HIn.fromElement

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
  liftEffect $ HIn.setAutocomplete autocomplete input
  pure input

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

-- | Create a plain ol' input field of type number with a default value.
numberField ∷ ∀ m f. MonadEffect m ⇒ Foldable f ⇒ f (String /\ String) → Maybe Number → m HTMLInputElement
numberField attrs mDefaultValue =
  (el "input" attrs [] # setAttrsM [ "type" /\ "number", "value" /\ (maybe "" show mDefaultValue) ])
    >>= testCoercion "input" "HTMLInputElement" <<< HIn.fromElement

-- | Create a password text field.
passwordField ∷ ∀ m f. MonadEffect m ⇒ Foldable f ⇒ f (String /\ String) → m HTMLInputElement
passwordField attrs =
  (el "input" attrs [] # setAttrsM [ "type" /\ "password" ])
    >>= testCoercion "input" "HTMLInputElement" <<< HIn.fromElement

-- | Create a button.
button ∷ ∀ m f1 f2 a. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → Maybe (HTMLButtonElement → Event → Effect a) → m HTMLButtonElement
button attributes childNodesM mClick = do
  btn ← el "button" attributes childNodesM >>=
    testCoercion "button" "HTMLButtonElement" <<< HBu.fromElement
  btn # maybe pure (on "click" <<< (#) btn) mClick

div ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HD.HTMLDivElement
div attributes children =
  el "div" attributes children >>=
    testCoercion "div" "HTMLDivElement" <<< HD.fromElement

span ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HSp.HTMLSpanElement
span attributes children =
  el "span" attributes children >>=
    testCoercion "span" "HTMLSpanElement" <<< HSp.fromElement

tr ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HTR.HTMLTableRowElement
tr attributes children =
  el "tr" attributes children >>=
    testCoercion "tr" "HTMLTableRowElement" <<< HTR.fromElement

td ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HTD.HTMLTableCellElement
td attributes children =
  el "td" attributes children >>=
    testCoercion "td" "HTMLTableCellElement" <<< HTD.fromElement

th ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HTD.HTMLTableCellElement
th attributes children =
  el "th" attributes children >>=
    testCoercion "th" "HTMLTableCellElement" <<< HTD.fromElement

table ∷ ∀ m f1 f2. MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ f1 (String /\ String) → f2 (m Node) → m HT.HTMLTableElement
table attributes children =
  el "table" attributes children >>=
    testCoercion "table" "HTMLTableElement" <<< HT.fromElement

check ∷ ∀ m. MonadEffect m ⇒ HTMLInputElement → m HTMLInputElement
check checkbx = do
  liftEffect $ HIn.setChecked true checkbx
  pure checkbx

uncheck ∷ ∀ m. MonadEffect m ⇒ HTMLInputElement → m HTMLInputElement
uncheck checkbx = do
  liftEffect $ HIn.setChecked false checkbx
  pure checkbx

-- | Create a checkbox.
checkbox ∷ ∀ m f a. MonadEffect m ⇒ Foldable f ⇒ f (String /\ String) → Boolean → Maybe (HTMLInputElement → Event → Effect a) → m HTMLInputElement
checkbox attributes isChecked mChange = do
  chk ← el "input" attributes []
    >>= testCoercion "input" "HTMLInputElement" <<< HIn.fromElement
    >>= if isChecked then check else uncheck
  chk # maybe pure (on "change" <<< (#) chk) mChange # setAttrsM [ "type" /\ "checkbox" ]
