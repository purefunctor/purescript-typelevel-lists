module Basic where

import Prelude

import Prim.Boolean
import Type.Data.List


-- Membership testing
type Allowed = ( TypeItem Int :> TypeItem String :> Nil' )

verify :: forall t. IsMember ( TypeItem t ) Allowed True => t -> t
verify = identity

verifyInt :: Int
verifyInt = verify 42


-- Concatenation
concat :: forall xs ys zs. Concat xs ys zs
  => ListProxy xs  -> ListProxy ys  -> ListProxy zs
concat _ _ = ListProxy

xs :: ListProxy ( TypeItem Int :> TypeItem String :> Nil')
xs = ListProxy

ys :: ListProxy ( TypeItem String :> TypeItem Char :> Nil' )
ys = ListProxy

zs :: ListProxy ( TypeItem Int :> TypeItem String :> TypeItem String :> TypeItem Char :> Nil' )
zs = concat xs ys


-- Null-checking
mustBeEmpty :: forall xs. IsEmpty xs True => ListProxy xs -> ListProxy xs
mustBeEmpty = identity

empty :: ListProxy Nil'
empty = mustBeEmpty ( ListProxy :: ListProxy Nil' )

mustHaveItems :: forall xs. IsEmpty xs False => ListProxy xs -> ListProxy xs
mustHaveItems = identity

items :: ListProxy ( TypeItem Int :> Nil' )
items = mustHaveItems ( ListProxy :: ListProxy ( TypeItem Int :> Nil' ) )


-- Deconstruction through `Init`.
type Types = (TypeItem String :> TypeItem Char :> TypeItem Int :> Nil')

init :: forall j k. Init j k => ListProxy j -> ListProxy k
init _ = ListProxy

trunc :: ListProxy (TypeItem String :> TypeItem Char :> Nil')
trunc = init (ListProxy :: _ Types)


-- Deconstruction through `Last`.
last :: forall j i. Last j i => ListProxy j -> ItemProxy i
last _ = ItemProxy

final :: ItemProxy (TypeItem Int)
final = last (ListProxy :: _ Types)
