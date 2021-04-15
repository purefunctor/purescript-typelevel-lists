-- | Type-level heterogenous list of kinds for PureScript
module Type.Data.List
  ( List'
  , Nil'
  , Cons'
  , type (:>)
  , class IsMember
  , isMember
  , class Concat
  , concat
  , class IsEmpty
  , isEmpty
  , class Init
  , init
  , class Init'
  , class Last
  , last
  , class Length
  , length
  , class Length'
  , class Take
  , take
  , class Drop
  , drop
  , class Zip
  , zip
  , class Map
  , class Fold
  , class Foldr
  , ListProxy(..)
  )
  where


import Data.Tuple (Tuple)
import Data.Unit (unit)
import Prim.Boolean (True, False)
import Type.Data.Peano as Peano
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


-- | Represents a type-level list.
data List' :: forall k. k -> Type
data List' k


-- | Represents an empty `List'`.
foreign import data Nil' :: forall k. List' k


-- | Prepends an item to a `List'`, creating a new `List'`.
foreign import data Cons' :: forall k. k -> List' k -> List' k


infixr 1 type Cons' as :>


-- | Performs membership testing given an item and a `List'`.
class IsMember :: forall k. k -> List' k -> Boolean -> Constraint
class IsMember x xs r | x xs -> r where
  isMember :: forall lproxy. Proxy x -> lproxy xs -> Boolean


instance isMemberNil :: IsMember x Nil' False where
  isMember _ _ = false

else

instance isMemberNext :: IsMember x (x :> xs) True where
  isMember _ _ = true

else

instance isMemberRec :: IsMember x ys r => IsMember x (y :> ys) r where
  isMember x _ = isMember x (Proxy :: _ ys)


-- | Concatenates two `List'`s together.
class Concat :: forall k. List' k -> List' k -> List' k -> Constraint
class Concat xs ys zs | xs ys -> zs where
  concat :: forall lproxy. lproxy xs -> lproxy ys -> lproxy zs


instance concatNil :: Concat Nil' ys ys where
  concat _ _ = unsafeCoerce unit

else

instance concatRec :: Concat xs ys zs => Concat (x :> xs) ys (x :> zs) where
  concat _ _ = unsafeCoerce unit


-- | Determines whether `List'` is empty.
class IsEmpty :: forall k. List' k -> Boolean -> Constraint
class IsEmpty xs r | xs -> r where
  isEmpty :: forall lproxy. lproxy xs -> Boolean


instance nilIsEmpty :: IsEmpty Nil' True where
  isEmpty _ = true

else

instance listIsEmpty :: IsEmpty (x :> xs) False where
  isEmpty _ = false


-- | Internal type class that acts as an accumulator.
class Init' :: forall k. k -> List' k -> List' k -> Constraint
class Init' xs ys zs | xs ys -> zs


instance initBase :: Init' xs Nil' Nil'

else

instance initRec :: (Init' z zs ws) => Init' y (z :> zs) (y :> ws)


-- | Takes the `init` items of a `List'`.
class Init :: forall k. List' k -> List' k -> Constraint
class Init xs ys | xs -> ys where
  init :: forall lproxy. lproxy xs -> lproxy ys


instance initList :: Init' x xs ys => Init (x :> xs) ys where
  init _ = unsafeCoerce unit


-- | Returns the last item of a `List'`.
class Last :: forall k. List' k -> k -> Constraint
class Last xs x | xs -> x where
  last :: forall lproxy. lproxy xs -> Proxy x


instance lastBase :: Last (x :> Nil') x where
  last _ = Proxy

else

instance lastRec :: Last xs ys => Last (x :> xs) ys where
  last _ = Proxy


-- | Internal type that acts as an accumulator
class Length' :: forall k. List' k -> Peano.Int -> Peano.Int -> Constraint
class Length' xs n r | xs n -> r


instance lengthBase :: Length' Nil' n n

else

instance lengthRec ::
  ( Peano.SumInt n Peano.P1 m
  , Length' xs m r
  ) => Length' (x :> xs) n r


-- | Computes the length of a `List'` as a `Type.Data.Peano.Int`
class Length :: forall k. List' k -> Peano.Int -> Constraint
class Length xs r | xs -> r where
  length :: forall lproxy iproxy. lproxy xs -> iproxy r


instance lengthList :: Length' xs Peano.P0 r => Length xs r where
  length _ = unsafeCoerce unit


-- | Takes an `n` amount of items from a `List'`.
class Take :: forall k. Peano.Int -> List' k -> List' k -> Constraint
class Take n xs ys | n xs -> ys where
  take :: forall lproxy iproxy. iproxy n -> lproxy xs -> lproxy ys


instance takeZero :: Take Peano.P0 xs Nil' where
  take _ _ = unsafeCoerce unit

else

instance takeNil :: Take n Nil' Nil' where
  take _ _ = unsafeCoerce unit

else

instance takeRec ::
  ( Peano.SumInt n Peano.N1 m
  , Take m xs ys
  ) => Take n (x :> xs) (x :> ys) where
  take _ _ = unsafeCoerce unit


-- | Drops an `n` amount of items from a `List'`.
class Drop :: forall k. Peano.Int -> List' k -> List' k -> Constraint
class Drop n xs ys | n xs -> ys where
  drop :: forall lproxy iproxy. iproxy n -> lproxy xs -> lproxy ys


instance dropZero :: Drop Peano.P0 xs xs where
  drop _ _ = unsafeCoerce unit

else

instance dropNil :: Drop n Nil' Nil' where
  drop _ _ = unsafeCoerce unit

else

instance dropRec ::
  ( Peano.SumInt n Peano.N1 m
  , Drop m xs ys
  ) => Drop n (x :> xs) ys where
  drop _ _ = unsafeCoerce unit


-- | Zips together two `List'`s.
class Zip :: forall k. List' k -> List' k -> List' k -> Constraint
class Zip x y z | x y -> z where
  zip :: forall lproxy. lproxy x -> lproxy y -> lproxy z


instance zipLhsNil :: Zip Nil' y Nil' where
  zip _ _ = unsafeCoerce unit

else

instance zipRhsNil :: Zip x Nil' Nil' where
  zip _ _ = unsafeCoerce unit

else

instance zipRec ::
  ( Zip xs ys zs
  ) => Zip ( x :> xs ) ( y :> ys ) ( Tuple x y :> zs ) where
  zip _ _ = unsafeCoerce unit


-- | Maps a type constructor to a `List'`.
class Map :: forall f k. f -> List' k -> List' k -> Constraint
class Map f xs ys | f xs -> ys


instance mapNil :: Map f Nil' Nil'

else

instance mapRec ::
  ( Map f xs ys
  ) => Map f ( x :> xs ) ( f x :> ys )


-- | Folds a `List'` into a singular value, left-associative.
class Fold :: forall f z. f -> z -> List' z -> z -> Constraint
class Fold f z xs r | f z xs -> r


instance foldNil :: Fold f z Nil' z

else

instance foldRec ::
  ( Fold f ( f z x ) xs r
  ) => Fold f z ( x :> xs ) r


-- | Folds a `List'` into a singular value, right-associative.
class Foldr :: forall f z. f -> z -> List' z -> z -> Constraint
class Foldr f z xs r | f z xs -> r


instance foldrNil :: Foldr f z Nil' z

else

instance foldrOne ::
  Foldr f z ( x :> Nil' ) ( f x z )

else

instance foldrRec ::
  ( Foldr f z xs r
  ) => Foldr f z ( x :> xs ) ( f x r )


-- | A value-level proxy for `List'`
data ListProxy :: forall k. List' k -> Type
data ListProxy l = ListProxy
