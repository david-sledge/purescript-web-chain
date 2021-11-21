-- | Functions for specific types of HTML elements.
module Web.Chain.HTML
( button
, textField
, val
) where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.List.Util ((&:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Util ((*&))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error, throwException)
import Web.Chain.Class (class IsDocument)
import Web.Chain.DOM (N, el)
import Web.Chain.Event (on)
import Web.Event.Event (Event)
import Web.HTML (HTMLButtonElement, HTMLInputElement)
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLInputElement (value)
import Web.HTML.HTMLInputElement as HI

-- | Create a plain ol' input field of type text with a default value.
textField :: forall m d. MonadAsk d m => MonadEffect m => IsDocument d => String -> m HTMLInputElement
textField defaultValue = do
  element <- el "input" (("value" *& defaultValue) &: ("type" *& "text")) Nil
  case HI.fromElement element of
    Just input -> pure input
    _ -> liftEffect <<< throwException $ error "'Web.Chain.DOM.el \"input\" val' did not produce an HTMLInputElement"

-- | Get the text from a text field.
val :: forall m. MonadEffect m => m HTMLInputElement -> m String
val mInput = liftEffect <<< value =<< mInput

-- | Create a button.
button :: forall d f m a. MonadAsk d m => MonadEffect m => IsDocument d => Foldable f => f (N m) -> (Event -> Effect a) -> m HTMLButtonElement
button childNodesM click = do
  element <- el "button" [] childNodesM # on "click" click
  case HB.fromElement element of
    Just buttonElement -> pure buttonElement
    _ -> liftEffect <<< throwException $ error "'Web.Chain.DOM.el \"button\" click' did not produce an HTMLButtonElement"
