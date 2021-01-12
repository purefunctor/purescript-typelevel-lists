-- | Type-level heterogenous list of kinds for PureScript
module Type.Data.List
  ( kind List'
  , kind Item'
  , Nil'
  , Cons'
  , type (:>)
  , TypeItem
  , SymbolItem
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
  , ListProxy(..)
  , ItemProxy(..)
  )
  where


import Prim.Boolean (kind Boolean, True, False)
import Type.Data.Peano as Peano


-- | Represents the type-level list.
foreign import kind List'


-- | Represents type-level list items.
foreign import kind Item'


-- | Represents an empty `List'`.
foreign import data Nil' :: List'


-- | Prepends any `Item'` to a `List'`, creating a new `List'`.
foreign import data Cons' :: Item' -> List' -> List'


infixr 1 type Cons' as :>


-- | Wraps a `Type` into an `Item'`.
foreign import data TypeItem :: Type -> Item'


-- | Wraps a `Symbol` into an `Item'`.
foreign import data SymbolItem :: Symbol -> Item'


-- | Wraps a `Boolean` into an `Item'`.
foreign import data BooleanItem :: Boolean -> Item'


-- | Performs membership testing given an `Item'` and a `List'`.
class IsMember ( x :: Item' ) ( xs :: List' ) ( r :: Boolean ) | x xs -> r


instance isMemberNil :: IsMember x Nil' False

else

instance isMemberNext :: IsMember x (x :> xs) True

else

instance isMemberRec :: IsMember x ys r => IsMember x (y :> ys) r


-- | Concatenates two `List'`s together.
class Concat ( xs :: List' ) ( ys :: List' ) ( zs :: List' ) | xs ys -> zs


instance concatNil :: Concat Nil' ys ys

else

instance concatRec :: Concat xs ys zs => Concat (x :> xs) ys (x :> zs)


-- | Determines whether `List'` is empty.
class IsEmpty ( xs :: List' ) ( r :: Boolean ) | xs -> r


instance nilIsEmpty :: IsEmpty Nil' True

else

instance listIsEmpty :: IsEmpty (x :> xs) False


-- | Internal type class that acts as an accumulator.
class Init' ( xs :: Item' ) ( ys :: List' ) ( zs :: List' ) | xs ys -> zs


instance initBase :: Init' xs Nil' Nil'

else

instance initRec :: (Init' z zs ws) => Init' y (z :> zs) (y :> ws)


-- | Takes the `init` items of a `List'`.
class Init ( xs :: List' ) ( ys :: List' ) | xs -> ys


instance initList :: Init' x xs ys => Init (x :> xs) ys


-- | Returns the last item of a `List'`.
class Last ( xs :: List' ) ( x :: Item' ) | xs -> x


instance lastBase :: Last (x :> Nil') x

else

instance lastRec :: Last xs ys => Last (x :> xs) ys


-- | Internal type that acts as an accumulator
class Length' ( xs :: List' ) ( n :: Peano.Int ) ( r :: Peano.Int ) | xs n -> r


instance lengthBase :: Length' Nil' n n

else

instance lengthRec ::
  ( Peano.SumInt n (Peano.Pos (Peano.Succ Peano.Z)) m
  , Length' xs m r
  ) => Length' (x :> xs) n r


-- | Computes the length of a `List'` as a `Type.Data.Peano.Int`
class Length ( xs :: List' ) ( r :: Peano.Int ) | xs -> r


instance lengthList :: Length' xs (Peano.Pos Peano.Z) r => Length xs r


-- | Takes an `n` amount of `Item'`s from a `List'`.
class Take ( n :: Peano.Int ) ( xs :: List' ) ( ys :: List' ) | n xs -> ys


instance takeZero :: Take (Peano.Pos Peano.Z) xs Nil'

else

instance takeNil :: Take n Nil' Nil'

else

instance takeRec ::
  ( Peano.SumInt n (Peano.Neg (Peano.Succ Peano.Z)) m
  , Take m xs ys
  ) => Take n (x :> xs) (x :> ys)


-- | Drops an `n` amount of `Item'`s from a `List'`.
class Drop ( n :: Peano.Int ) ( xs :: List' ) ( ys :: List' ) | n ys -> ys


instance dropZero :: Drop (Peano.Pos Peano.Z) xs xs

else

instance dropNil :: Drop n Nil' Nil'

else

instance dropRec ::
  ( Peano.SumInt n (Peano.Neg (Peano.Succ Peano.Z)) m
  , Drop m xs ys
  ) => Drop n (x :> xs) ys


-- | A value-level proxy for `List'`
data ListProxy (l :: List') = ListProxy


-- | A value-level proxy for `Item'`
data ItemProxy (i :: Item') = ItemProxy
