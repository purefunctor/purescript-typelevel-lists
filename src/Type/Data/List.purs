module Type.Data.List
  ( kind List'
  , kind Item'
  , Nil'
  , Cons'
  , type (:>)
  , TypeItem
  , SymbolItem
  , class Member
  , class Concat
  , class Empty
  , ListProxy(..)
  , ItemProxy(..)
  )
  where


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


-- | Wraps a `Type`, converting it into an `Item'`.
foreign import data TypeItem :: Type -> Item'


-- | Wraps a `Symbol` , converting it into an `Item'`.
foreign import data SymbolItem :: Symbol -> Item'


-- | A typeclass for membership tests.
class Member ( x :: Item' ) ( xs :: List' )


-- | Matches the next item in the `List'`.
instance memberNext :: Member x (x :> xs)

else

-- | Recurses deeper into the `List'`.
instance memberRec :: Member x ys => Member x (y :> ys)


-- | A typeclass for `List'` concatenation
class Concat ( xs :: List' ) ( ys :: List' ) ( zs :: List' ) | xs ys -> zs


-- | Reflects RHS if given `Nil'` on the LHS.
instance concatNil :: Concat Nil' ys ys

else

-- | Recursivelly concatenates two `List'`s.
instance concatRec :: Concat xs ys zs => Concat (x :> xs) ys (x :> zs)


-- | A typeclass that checks for empty `List'`s.
class Empty ( xs :: List' )


instance emptyList :: Empty Nil'


-- | A value-level proxy for `List'`
data ListProxy (l :: List') = ListProxy


-- | A value-level proxy for `Item'`
data ItemProxy (i :: Item') = ItemProxy
