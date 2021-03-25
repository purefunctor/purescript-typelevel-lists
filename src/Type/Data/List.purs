-- | Type-level heterogenous list of kinds for PureScript
module Type.Data.List
  ( List'
  , Nil'
  , Cons'
  , type (:>)
  , class IsMember
  , class Concat
  , class IsEmpty
  , class Init
  , class Init'
  , class Last
  , class Length
  , class Length'
  , class Take
  , class Drop
  , class Zip
  , class Map
  , class Fold
  , ListProxy(..)
  )
  where


import Data.Tuple (Tuple)
import Prim.Boolean (True, False)
import Type.Data.Peano as Peano


-- | Represents a type-level list.
data List'


-- | Represents an empty `List'`.
foreign import data Nil' :: List'


-- | Prepends an item to a `List'`, creating a new `List'`.
foreign import data Cons' :: forall k. k -> List' -> List'


infixr 1 type Cons' as :>


-- | Performs membership testing given an item and a `List'`.
class IsMember :: forall k. k -> List' -> Boolean -> Constraint
class IsMember x xs r | x xs -> r


instance isMemberNil :: IsMember x Nil' False

else

instance isMemberNext :: IsMember x (x :> xs) True

else

instance isMemberRec :: IsMember x ys r => IsMember x (y :> ys) r


-- | Concatenates two `List'`s together.
class Concat (xs :: List') (ys :: List') (zs :: List') | xs ys -> zs


instance concatNil :: Concat Nil' ys ys

else

instance concatRec :: Concat xs ys zs => Concat (x :> xs) ys (x :> zs)


-- | Determines whether `List'` is empty.
class IsEmpty (xs :: List') (r :: Boolean) | xs -> r


instance nilIsEmpty :: IsEmpty Nil' True

else

instance listIsEmpty :: IsEmpty (x :> xs) False


-- | Internal type class that acts as an accumulator.
class Init' :: forall k. k -> List' -> List' -> Constraint
class Init' xs ys zs | xs ys -> zs


instance initBase :: Init' xs Nil' Nil'

else

instance initRec :: (Init' z zs ws) => Init' y (z :> zs) (y :> ws)


-- | Takes the `init` items of a `List'`.
class Init (xs :: List') (ys :: List') | xs -> ys


instance initList :: Init' x xs ys => Init (x :> xs) ys


-- | Returns the last item of a `List'`.
class Last :: forall k. List' -> k -> Constraint
class Last xs x | xs -> x


instance lastBase :: Last (x :> Nil') x

else

instance lastRec :: Last xs ys => Last (x :> xs) ys


-- | Internal type that acts as an accumulator
class Length' (xs :: List') (n :: Peano.Int) (r :: Peano.Int) | xs n -> r


instance lengthBase :: Length' Nil' n n

else

instance lengthRec ::
  ( Peano.SumInt n Peano.P1 m
  , Length' xs m r
  ) => Length' (x :> xs) n r


-- | Computes the length of a `List'` as a `Type.Data.Peano.Int`
class Length (xs :: List') (r :: Peano.Int) | xs -> r


instance lengthList :: Length' xs Peano.P0 r => Length xs r


-- | Takes an `n` amount of items from a `List'`.
class Take (n :: Peano.Int) (xs :: List') (ys :: List') | n xs -> ys


instance takeZero :: Take Peano.P0 xs Nil'

else

instance takeNil :: Take n Nil' Nil'

else

instance takeRec ::
  ( Peano.SumInt n Peano.N1 m
  , Take m xs ys
  ) => Take n (x :> xs) (x :> ys)


-- | Drops an `n` amount of items from a `List'`.
class Drop (n :: Peano.Int) (xs :: List') (ys :: List') | n xs -> ys


instance dropZero :: Drop Peano.P0 xs xs

else

instance dropNil :: Drop n Nil' Nil'

else

instance dropRec ::
  ( Peano.SumInt n Peano.N1 m
  , Drop m xs ys
  ) => Drop n (x :> xs) ys


-- | Zips together two `List'`s.
class Zip (x :: List') (y :: List') (z :: List') | x y -> z


instance zipLhsNil :: Zip Nil' y Nil'

else

instance zipRhsNil :: Zip x Nil' Nil'

else

instance zipRec ::
  ( Zip xs ys zs
  ) => Zip ( x :> xs ) ( y :> ys ) ( Tuple x y :> zs )


-- | Maps a type constructor to a `List'`.
class Map :: forall f. f -> List' -> List' -> Constraint
class Map f xs ys | f xs -> ys


instance mapNil :: Map f Nil' Nil'

else

instance mapRec ::
  ( Map f xs ys
  ) => Map f ( x :> xs ) ( f x :> ys )


-- | Folds a `List'` into a singular value
class Fold :: forall f z. f -> z -> List' -> z -> Constraint
class Fold f z xs r | f z xs -> r


instance foldNil :: Fold f z Nil' z

else

instance foldRec ::
  ( Fold f ( f z x ) xs r
  ) => Fold f z ( x :> xs ) r


-- | A value-level proxy for `List'`
data ListProxy (l :: List') = ListProxy
