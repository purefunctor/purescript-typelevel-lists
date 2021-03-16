module Basic where

import Prelude

import Prim.Boolean (False, True)
import Type.Data.List (class Concat, class Drop, class Init, class IsEmpty, class IsMember, class Last, class Length, class Take, type (:>), ListProxy(..), Nil')
import Type.Data.Peano (IProxy(..), P2)
import Type.Proxy (Proxy(..))


-- Membership testing
type Allowed = ( Int :> String :> Nil' )

verify :: forall t. IsMember t Allowed True => t -> t
verify = identity

verifyInt :: Int
verifyInt = verify 42


-- Concatenation
concat :: forall xs ys zs. Concat xs ys zs
  => ListProxy xs  -> ListProxy ys  -> ListProxy zs
concat _ _ = ListProxy

xs :: ListProxy ( Int :> String :> Nil')
xs = ListProxy

ys :: ListProxy ( String :> Char :> Nil' )
ys = ListProxy

zs :: ListProxy ( Int :> String :> String :> Char :> Nil' )
zs = concat xs ys


-- Null-checking
mustBeEmpty :: forall xs. IsEmpty xs True => ListProxy xs -> ListProxy xs
mustBeEmpty = identity

empty :: ListProxy Nil'
empty = mustBeEmpty ( ListProxy :: ListProxy Nil' )

mustHaveItems :: forall xs. IsEmpty xs False => ListProxy xs -> ListProxy xs
mustHaveItems = identity

items :: ListProxy ( Int :> Nil' )
items = mustHaveItems ( ListProxy :: ListProxy ( Int :> Nil' ) )


-- Deconstruction through `Init`.
type Types = (String :> Char :> Int :> Nil')

init :: forall j k. Init j k => ListProxy j -> ListProxy k
init _ = ListProxy

trunc :: ListProxy (String :> Char :> Nil')
trunc = init (ListProxy :: _ Types)


-- Deconstruction through `Last`.
last :: forall j i. Last j i => ListProxy j -> Proxy i
last _ = Proxy

final :: Proxy (Int)
final = last (ListProxy :: _ Types)


-- List lengths through `typelevel-peano`.
length :: forall xs r. Length xs r => ListProxy xs -> Proxy r
length _ = Proxy

peano :: Proxy P2
peano = length (ListProxy :: _ (String :> Char :> Nil'))


-- Take an `n` amount of items.
take :: forall n xs ys. Take n xs ys => IProxy n -> ListProxy xs -> ListProxy ys
take _ _ = ListProxy

took :: ListProxy (Int :> Char :> Nil')
took = take (IProxy :: _ P2) (ListProxy :: _ (Int :> Char :> String :> Nil'))


-- Drop an `n` amount of items.
drop :: forall n xs ys. Drop n xs ys => IProxy n -> ListProxy xs -> ListProxy ys
drop _ _ = ListProxy

remains :: ListProxy (String :> Int :> Nil')
remains = drop (IProxy :: _ P2) (ListProxy :: _ (Int :> Char :> String :> Int :> Nil'))
