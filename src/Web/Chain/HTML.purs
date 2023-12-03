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
  , valM
  )
  where

import Prelude

import Data.Either (Either(..), either)
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
import Web.Chain (class IsElement)
import Web.Chain.DOM (N, attrM, el, rmAttrM, setAttrsM)
import Web.Chain.Event (onM)
import Web.Event.Event (Event)
import Web.HTML (HTMLButtonElement, HTMLInputElement)
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLInputElement (value)
import Web.HTML.HTMLInputElement as HI

-- | Create a plain ol' input field of type text with a default value.
textField ∷ ∀ m. MonadEffect m ⇒ String → m HTMLInputElement
textField defaultValue = maybe
    (liftEffect <<< throwException $ error "'Web.Chain.DOM.el \"input\" val' did not produce an HTMLInputElement")
    pure <<<
      HI.fromElement =<<
        el "input" (("value" *& defaultValue) &: ("type" *& "text")) Nil
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
enable ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ m e → m e
enable = rmAttrM "disabled"

-- | Disable an input. The input is returned.
disable ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ m e → m e
disable = setAttrsM (s ("disabled" *& "disabled"))

-- | Is this input enabled?
isEnabled ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ m e → m Boolean
isEnabled mInput = maybe
    (pure true) (pure <<< (/=) "disabled") =<< attrM "disabled" mInput

-- | Set the the range limits on the required number of characters of a text
-- | field. The text field is returned.
setLenLimits ∷ ∀ e m. IsElement e ⇒ MonadEffect m ⇒ Int → Maybe Int → m e → m e
setLenLimits min mMax mInput = either
    (\ errMsgs → liftEffect <<< throwException <<< error $ intercalate "\n" errMsgs)
    (\ attrs → setAttrsM attrs mInput) <<<
    either
      (\ errMsgs → Left $ if min < 0
        then "min may not be less than zero." : errMsgs
        else errMsgs)
      (\ attrs → if min < 0
        then Left $ s "min may not be less than zero."
        else Right $ if min > 0
          then ("minlength" *& toString (toNumber min)) : attrs
          else ("minlength" *& "") : attrs) $
      maybe
        (Right $ s ("maxlength" *& ""))
        (\ max → if min > max
          then Left $ s "max may not be less than min."
          else if max < 1
            then Left $ s "max may must be at least 1."
            else Right $ s ("maxlength" *& toString (toNumber max)))
        mMax

-- | Get the max length setting.
maxLen ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ m e → m (Maybe Int)
maxLen mInput = maybe Nothing fromString <$> attrM "maxlength" mInput

-- | Get the min length setting.
minLen ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ m e → m Int
minLen mInput = maybe 0 (fromMaybe 0 <<< fromString) <$> attrM "minlength" mInput

-- | Get the text from a text field.
val ∷ ∀ m. MonadEffect m ⇒ HTMLInputElement → m String
val input = liftEffect $ value input

-- | Get the text from a text field.
valM ∷ ∀ m. MonadEffect m ⇒ m HTMLInputElement → m String
valM = (=<<) val

-- | Create a button.
button ∷ ∀ f m a. MonadEffect m ⇒ Foldable f ⇒ f (N m) → (Event → Effect a) → m HTMLButtonElement
button childNodesM click = do
  element ← el "button" Nil childNodesM # onM "click" click
  maybe (liftEffect <<< throwException $ error "'Web.Chain.DOM.el \"button\" click' did not produce an HTMLButtonElement") (pure) $ HB.fromElement element
