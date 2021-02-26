-- | Type-level heterogenous list of kinds for PureScript
module Type.Data.List
  ( List'
  , Item'
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


import Prim.Boolean (True, False)
import Type.Data.Peano as Peano


-- | Represents the type-level list.
data List' :: forall k. k -> Type
data List' a


-- | Represents type-level list items.
data Item'


-- | Represents an empty `List'`.
foreign import data Nil' :: forall k. List' k


-- | Prepends any `Item'` to a `List'`, creating a new `List'`.
foreign import data Cons' :: forall k. k -> List' k -> List' k


infixr 1 type Cons' as :>


-- | Wraps a `Type` into an `Item'`.
foreign import data TypeItem :: Type -> Item'


-- | Wraps a `Symbol` into an `Item'`.
foreign import data SymbolItem :: Symbol -> Item'


-- | Wraps a `Boolean` into an `Item'`.
foreign import data BooleanItem :: Boolean -> Item'


-- | Wraps any kind into an `Item'`.
foreign import data AnyItem :: forall k. k -> Item'


-- | Performs membership testing given an `Item'` and a `List'`.
class IsMember :: forall k. k -> List' k -> Boolean -> Constraint
class IsMember x xs r | x xs -> r


instance isMemberNil :: IsMember x Nil' False

else

instance isMemberNext :: IsMember x (x :> xs) True

else

instance isMemberRec :: IsMember x ys r => IsMember x (y :> ys) r


-- | Concatenates two `List'`s together.
class Concat :: forall k. List' k -> List' k -> List' k -> Constraint
class Concat xs ys zs | xs ys -> zs


instance concatNil :: Concat Nil' ys ys

else

instance concatRec :: Concat xs ys zs => Concat (x :> xs) ys (x :> zs)


-- | Determines whether `List'` is empty.
class IsEmpty :: forall k. List' k -> Boolean -> Constraint
class IsEmpty xs r | xs -> r


instance nilIsEmpty :: IsEmpty Nil' True

else

instance listIsEmpty :: IsEmpty (x :> xs) False


-- | Internal type class that acts as an accumulator.
class Init' :: forall k. k -> List' k -> List' k -> Constraint
class Init' xs ys zs | xs ys -> zs


instance initBase :: Init' xs Nil' Nil'

else

instance initRec :: (Init' z zs ws) => Init' y (z :> zs) (y :> ws)


-- | Takes the `init` items of a `List'`.
class Init :: forall k. List' k -> List' k -> Constraint
class Init xs ys | xs -> ys


instance initList :: Init' x xs ys => Init (x :> xs) ys


-- | Returns the last item of a `List'`.
class Last :: forall k. List' k -> k -> Constraint
class Last xs x | xs -> x


instance lastBase :: Last (x :> Nil') x

else

instance lastRec :: Last xs ys => Last (x :> xs) ys


-- | Internal type that acts as an accumulator
class Length' :: forall k. List' k -> Peano.Int -> Peano.Int -> Constraint
class Length' xs n r | xs n -> r


instance lengthBase :: Length' Nil' n n

else

instance lengthRec ::
  ( Peano.SumInt n (Peano.Pos (Peano.Succ Peano.Z)) m
  , Length' xs m r
  ) => Length' (x :> xs) n r


-- | Computes the length of a `List'` as a `Type.Data.Peano.Int`
class Length :: forall k. List' k -> Peano.Int -> Constraint
class Length xs r | xs -> r


instance lengthList :: Length' xs (Peano.Pos Peano.Z) r => Length xs r


-- | Takes an `n` amount of `Item'`s from a `List'`.
class Take :: forall k. Peano.Int -> List' k -> List' k -> Constraint
class Take n xs ys | n xs -> ys


instance takeZero :: Take (Peano.Pos Peano.Z) xs Nil'

else

instance takeNil :: Take n Nil' Nil'

else

instance takeRec ::
  ( Peano.SumInt n (Peano.Neg (Peano.Succ Peano.Z)) m
  , Take m xs ys
  ) => Take n (x :> xs) (x :> ys)


-- | Drops an `n` amount of `Item'`s from a `List'`.
class Drop :: forall k. Peano.Int -> List' k -> List' k -> Constraint
class Drop n xs ys | n xs -> ys


instance dropZero :: Drop (Peano.Pos Peano.Z) xs xs

else

instance dropNil :: Drop n Nil' Nil'

else

instance dropRec ::
  ( Peano.SumInt n (Peano.Neg (Peano.Succ Peano.Z)) m
  , Drop m xs ys
  ) => Drop n (x :> xs) ys


-- | A value-level proxy for `List'`
data ListProxy :: forall k. List' k -> Type
data ListProxy (l :: List' k) = ListProxy


-- | A value-level proxy for `Item'`
data ItemProxy (i :: Item') = ItemProxy
