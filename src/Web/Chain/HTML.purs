module Web.Chain.HTML where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.List (List(..))
import Data.List.Util ((&:))
import Data.Maybe (Maybe(..))
import Data.Tuple.Util ((*&))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error, throwException)
import Web.Chain.Class (class IsDocument)
import Web.Chain.DOM (el)
import Web.HTML.HTMLInputElement (HTMLInputElement, value)
import Web.HTML.HTMLInputElement as HI

textField :: forall m d. MonadAsk d m => MonadEffect m => IsDocument d => String -> m HTMLInputElement
textField defaultValue = do
  element <- el "input" (("value" *& defaultValue) &: ("type" *& "text")) Nil
  case HI.fromElement element of
    Just input -> pure input
    _ -> liftEffect <<< throwException $ error "'Web.Chain.DOM.el \"input\" val' did not produce an input element"

val :: forall m. MonadEffect m => m HTMLInputElement -> m String
val mInput = liftEffect <<< value =<< mInput
