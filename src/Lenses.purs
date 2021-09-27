module Taihe.Lenses where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))

_x :: forall t a r. Newtype t { x :: a | r } => Lens' t a
_x = _Newtype <<< prop (Proxy :: Proxy "x")

_y :: forall t a r. Newtype t { y :: a | r } => Lens' t a
_y = _Newtype <<< prop (Proxy :: Proxy "y")

_dragType :: forall t a r. Newtype t { dragType :: a | r } => Lens' t a
_dragType = _Newtype <<< prop (Proxy :: Proxy "dragType")

_canDrag :: forall t a r. Newtype t { canDrag :: a | r } => Lens' t a
_canDrag = _Newtype <<< prop (Proxy :: Proxy "canDrag")

_curDragEvt :: forall t a r. Newtype t { curDragEvt :: a | r } => Lens' t a
_curDragEvt = _Newtype <<< prop (Proxy :: Proxy "curDragEvt")

_deltaX :: forall t a r. Newtype t { deltaX :: a | r } => Lens' t a
_deltaX = _Newtype <<< prop (Proxy :: Proxy "deltaX")

_deltaY :: forall t a r. Newtype t { deltaY :: a | r } => Lens' t a
_deltaY = _Newtype <<< prop (Proxy :: Proxy "deltaY")

_isDragging :: forall t a r. Newtype t { isDragging :: a | r } => Lens' t a
_isDragging = _Newtype <<< prop (Proxy :: Proxy "isDragging")

_dragged :: forall t a r. Newtype t { dragged :: a | r } => Lens' t a
_dragged = _Newtype <<< prop (Proxy :: Proxy "dragged")

_lastDragEvt :: forall t a r. Newtype t { lastDragEvt :: a | r } => Lens' t a
_lastDragEvt = _Newtype <<< prop (Proxy :: Proxy "lastDragEvt")

_distance :: forall t a r. Newtype t { distance :: a | r } => Lens' t a
_distance = _Newtype <<< prop (Proxy :: Proxy "distance")

_width :: forall t a r. Newtype t { width :: a | r } => Lens' t a
_width = _Newtype <<< prop (Proxy :: Proxy "width")

_height :: forall t a r. Newtype t { height :: a | r } => Lens' t a
_height = _Newtype <<< prop (Proxy :: Proxy "height")

_mouseMove :: forall t a r. Newtype t { mouseMove :: a | r } => Lens' t a
_mouseMove = _Newtype <<< prop (Proxy :: Proxy "mouseMove")

_tapped :: forall t a r. Newtype t { tapped :: a | r } => Lens' t a
_tapped = _Newtype <<< prop (Proxy :: Proxy "tapped")
