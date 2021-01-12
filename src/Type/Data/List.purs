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
  , ListProxy(..)
  , ItemProxy(..)
  )
  where


import Prim.Boolean (kind Boolean, True, False)


-- | The kind of the type-level list.
foreign import kind List'


-- | The kind of the type-level list items.
foreign import kind Item'


-- | Represents an empty type-level list.
foreign import data Nil' :: List'


-- | Prepends any `Item'` to a `List'`, creating a new `List'`.
foreign import data Cons' :: Item' -> List' -> List'


-- | Infix synonym for `Cons'`.
infixr 1 type Cons' as :>


-- | Wraps a `Type` into an `Item'`.
foreign import data TypeItem :: Type -> Item'


-- | Wraps a `Symbol` into an `Item'`.
foreign import data SymbolItem :: Symbol -> Item'


-- | Wraps a `Boolean` into an `Item'`.
foreign import data BooleanItem :: Boolean -> Item'


-- | Performs membership testing given an `Item'` and a `List'`.
class IsMember ( x :: Item' ) ( xs :: List' ) ( r :: Boolean ) | x xs -> r


-- | Base case, `Nil'`s have no items.
instance isMemberNil :: IsMember x Nil' False

else

-- | Matches the next item in the `List'`.
instance isMemberNext :: IsMember x (x :> xs) True

else

-- | Recurses deeper into the `List'`.
instance isMemberRec :: IsMember x ys r => IsMember x (y :> ys) r


-- | Concatenates two `List'`s together.
class Concat ( xs :: List' ) ( ys :: List' ) ( zs :: List' ) | xs ys -> zs


-- | Reflects RHS if given `Nil'` on the LHS.
instance concatNil :: Concat Nil' ys ys

else

-- | Recursivelly concatenates two `List'`s.
instance concatRec :: Concat xs ys zs => Concat (x :> xs) ys (x :> zs)


-- | Determines whether `List'` is empty.
class IsEmpty ( xs :: List' ) ( r :: Boolean ) | xs -> r


-- | `Nil'`s are inherently empty.
instance nilIsEmpty :: IsEmpty Nil' True

else

-- | Any other `List'` isn't empty.
instance listIsEmpty :: IsEmpty (x :> xs) False


-- | Internal type class that acts as an accumulator.
class Init' ( xs :: Item' ) ( ys :: List' ) ( zs :: List' ) | xs ys -> zs


-- | Reflect RHS if given `Nil` on the RHS.
instance initBase :: Init' xs Nil' Nil'

else

-- | Recursively collect items until the end.
instance initRec :: (Init' z zs ws) => Init' y (z :> zs) (y :> ws)


-- | Takes the `init` items of a `List'`.
class Init ( xs :: List' ) ( ys :: List' ) | xs -> ys


-- | Recursively collects items using `Init'`.
instance initList :: Init' x xs ys => Init (x :> xs) ys


-- | Returns the last item of a `List'`.
class Last ( xs :: List' ) ( x :: Item' ) | xs -> x


-- | The last item is always `Cons'`ed to a `Nil'`.
instance lastBase :: Last (x :> Nil') x

else

-- | Recursively search until the base case is hit.
instance lastRec :: Last xs ys => Last (x :> xs) ys


-- | A value-level proxy for `List'`
data ListProxy (l :: List') = ListProxy


-- | A value-level proxy for `Item'`
data ItemProxy (i :: Item') = ItemProxy
