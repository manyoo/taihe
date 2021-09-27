module Three.Core.Mesh where

import Prelude

import Effect (Effect)
import Three.Core.Geometry (class IsGeometry, BufferGeometry, LineGeometry, toGeometry)
import Three.Core.Material (class IsLineMaterial, class IsMaterial, LineMaterial, Material, MeshBasicMaterial, toLineMaterial, toMaterial)
import Three.Core.Object3D (class IsObject3D)
import Unsafe.Coerce (unsafeCoerce)

class IsMesh m where
    toMesh :: m -> Mesh

foreign import data Mesh :: Type
instance IsMesh Mesh where
    toMesh = identity

foreign import jsmkMesh :: BufferGeometry -> Material -> Effect Mesh
instance IsObject3D Mesh where
    toObject3D = unsafeCoerce

mkMesh :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect Mesh
mkMesh geo mat = jsmkMesh (toGeometry geo) (toMaterial mat)

foreign import isMesh :: Mesh -> Boolean

foreign import jsgeometry :: Mesh -> BufferGeometry

geometry :: forall m geo. IsMesh m => IsGeometry geo => m -> geo
geometry = unsafeCoerce <<< jsgeometry <<< toMesh

foreign import jssetGeometry :: BufferGeometry -> Mesh -> Effect Unit

setGeometry :: forall geo. IsGeometry geo => geo -> Mesh -> Effect Unit
setGeometry geo = jssetGeometry (toGeometry geo)

foreign import material :: Mesh -> MeshBasicMaterial
foreign import jssetMaterial :: Material -> Mesh -> Effect Unit

setMaterial :: forall mat. IsMaterial mat => mat -> Mesh -> Effect Unit
setMaterial mat = jssetMaterial (toMaterial mat)

foreign import data Line :: Type
instance IsMesh Line where
    toMesh = unsafeCoerce

foreign import jsmkLine :: LineGeometry -> LineMaterial -> Effect Line

mkLine :: forall mat. IsLineMaterial mat => LineGeometry -> mat -> Effect Line
mkLine geo = jsmkLine geo <<< toLineMaterial

foreign import computeLineDistances :: Line -> Effect Unit
instance isObject3DLine :: IsObject3D Line where
    toObject3D = unsafeCoerce
