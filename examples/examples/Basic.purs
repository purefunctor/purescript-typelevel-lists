module Basic where

import Prelude

import Prim.Boolean
import Type.Data.List
import Type.Data.Peano


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


-- List lengths through `typelevel-peano`.
length :: forall xs r. Length xs r => ListProxy xs -> IProxy r
length _ = IProxy

peano :: IProxy P2
peano = length (ListProxy :: _ (TypeItem String :> TypeItem Char :> Nil'))


-- Take an `n` amount of items.
take :: forall n xs ys. Take n xs ys => IProxy n -> ListProxy xs -> ListProxy ys
take _ _ = ListProxy

took :: ListProxy (TypeItem Int :> TypeItem Char :> Nil')
took = take (IProxy :: _ P2) (ListProxy :: _ (TypeItem Int :> TypeItem Char :> TypeItem String :> Nil'))


-- Drop an `n` amount of items.
drop :: forall n xs ys. Drop n xs ys => IProxy n -> ListProxy xs -> ListProxy ys
drop _ _ = ListProxy

remains :: ListProxy (TypeItem String :> TypeItem Int :> Nil')
remains = drop (IProxy :: _ P2) (ListProxy :: _ (TypeItem Int :> TypeItem Char :> TypeItem String :> TypeItem Int :> Nil'))
