-- | Functions for specific types of HTML elements.
module Web.Chain.HTML
( button
, disable
, enable
, isEnabled
, maxLen
, minLen
, setAutocomplete
, setLenLimits
, textField
, val
) where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, intercalate)
import Data.Int (fromString, toNumber)
import Data.List (List(..), (:))
import Data.List.Util (s, (&:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number.Format (toString)
import Data.Tuple.Util ((*&))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error, throwException)
import Web.Chain (class IsDocument, class IsElement)
import Web.Chain.DOM (N, attr, el, rmAttr, setAttrs)
import Web.Chain.Event (on)
import Web.Event.Event (Event)
import Web.HTML (HTMLButtonElement, HTMLInputElement)
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLInputElement (value)
import Web.HTML.HTMLInputElement as HI

-- | Create a plain ol' input field of type text with a default value.
textField ∷ ∀ m d. MonadAsk d m ⇒ MonadEffect m ⇒ IsDocument d ⇒ String → m HTMLInputElement
textField defaultValue = do
  element ← el "input" (("value" *& defaultValue) &: ("type" *& "text")) Nil
  case HI.fromElement element of
    Just input → pure input
    _ → liftEffect <<< throwException $ error "'Web.Chain.DOM.el \"input\" val' did not produce an HTMLInputElement"
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
setAutocomplete ∷ ∀ m. MonadEffect m ⇒ Boolean → m HTMLInputElement → m HTMLInputElement
setAutocomplete isOn mInput = do
  input ← mInput
  liftEffect $ HI.setAutocomplete isOn input
  pure input

-- | Enable an input. The input is returned.
enable ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ m e → m e
enable = rmAttr "disabled"

-- | Disable an input. The input is returned.
disable ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ m e → m e
disable = setAttrs (s ("disabled" *& "disabled"))

-- | Is this input enabled?
isEnabled ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ m e → m Boolean
isEnabled mInput = do
  mAttrVal ← attr "disabled" mInput
  pure $ case mAttrVal of
    Just attrVal → attrVal /= "disabled"
    _ → true

-- | Set the the range limits on the required number of characters of a text
-- | field. The text field is returned.
setLenLimits ∷ ∀ e m. IsElement e ⇒ MonadEffect m ⇒ Int → Maybe Int → m e → m e
setLenLimits min mMax mInput =
  case
  ( case
    ( case mMax of
      Just max → if min > max
        then Left $ s "max may not be less than min."
        else if max < 1
          then Left $ s "max may must be at least 1."
          else Right $ s ("maxlength" *& toString (toNumber max))
      _ → Right $ s ("maxlength" *& "")
    ) of
    Left errMsgs → Left $ if min < 0
      then "min may not be less than zero." : errMsgs
      else errMsgs
    Right attrs → if min < 0
      then Left $ s "min may not be less than zero."
      else Right $ if min > 0
        then ("minlength" *& toString (toNumber min)) : attrs
        else ("minlength" *& "") : attrs
  ) of
    Right attrs → setAttrs attrs mInput
    Left errMsgs → liftEffect <<< throwException <<< error $ intercalate "\n" errMsgs

-- | Get the max length setting.
maxLen ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ m e → m (Maybe Int)
maxLen mInput = maybe Nothing fromString <$> attr "maxlength" mInput

-- | Get the min length setting.
minLen ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ m e → m Int
minLen mInput = maybe 0 (fromMaybe 0 <<< fromString) <$> attr "minlength" mInput

-- | Get the text from a text field.
val ∷ ∀ m. MonadEffect m ⇒ m HTMLInputElement → m String
val mInput = liftEffect <<< value =<< mInput

-- | Create a button.
button ∷ ∀ d f m a. MonadAsk d m ⇒ MonadEffect m ⇒ IsDocument d ⇒ Foldable f ⇒ f (N m) → (Event → Effect a) → m HTMLButtonElement
button childNodesM click = do
  element ← el "button" Nil childNodesM # on "click" click
  case HB.fromElement element of
    Just buttonElement → pure buttonElement
    _ → liftEffect <<< throwException $ error "'Web.Chain.DOM.el \"button\" click' did not produce an HTMLButtonElement"
