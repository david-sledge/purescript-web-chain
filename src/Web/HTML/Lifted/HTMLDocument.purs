module Web.HTML.Lifted.HTMLDocument where

import Prelude

import Data.Maybe (Maybe(Just, Nothing), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.Chain.HTML.Util (testCoercion)
import Web.HTML (HTMLBodyElement, HTMLDocument, HTMLHeadElement)
import Web.HTML.HTMLBodyElement as HBo
import Web.HTML.HTMLHeadElement as HHe
import Web.HTML.HTMLDocument as HD

foreign import _setBody ∷ HTMLBodyElement → HTMLDocument → Effect Unit

foreign import _setHead ∷ HTMLHeadElement → HTMLDocument → Effect Unit

setBody ∷ ∀ m. MonadEffect m ⇒ HTMLBodyElement → HTMLDocument → m Unit
setBody = compose liftEffect <<< _setBody

setHead ∷ ∀ m. MonadEffect m ⇒ HTMLHeadElement → HTMLDocument → m Unit
setHead = compose liftEffect <<< _setHead

body ∷ ∀ m. MonadEffect m ⇒ HTMLDocument → m (Maybe HTMLBodyElement)
body doc =
  liftEffect
    (HD.body doc)
    >>= maybe (pure Nothing) (map Just <<< testCoercion "body" "HTMLBodyElement" <<< HBo.fromHTMLElement)

head ∷ ∀ m. MonadEffect m ⇒ HTMLDocument → m (Maybe HTMLHeadElement)
head doc =
  liftEffect
    (HD.head doc)
    >>= maybe (pure Nothing) (map Just <<< testCoercion "head" "HTMLHeadElement" <<< HHe.fromHTMLElement)
