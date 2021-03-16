module Homogeneity where

import Prelude (show)

import Type.Data.List (class Hom, type (:>), Nil')
import Type.Proxy (Proxy(..))

type Valid = ( Int :> Nil'  )

type Invalid = ( "Int" :> Nil' )


valid ::
  forall (k :: Type) xs
  . Hom k Valid xs
  => Proxy xs
valid = Proxy


valid_ :: String
valid_ = show valid


-- | Invalid use of `Hom` requires that the term
-- | should be consumed at least once in order
-- | for the compiler to invoke an error. This
-- | definition is relatively harmless otherwise.
invalid ::
  forall (k :: Type) xs
  . Hom k Invalid xs
  => Proxy xs
invalid = Proxy


-- invalid_ :: String
-- invalid_ = show invalid
