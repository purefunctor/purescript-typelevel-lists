# purescript-typelevel-lists
Type-level heterogenous list of kinds for PureScript.

# Examples

``` purescript
import Prelude

import Type.Data.List


-- Membership testing
type Allowed = ( TypeItem Int :> TypeItem String :> Nil' )

verify :: forall t. Member ( TypeItem t ) Allowed => t -> t
verify = identity

verifyInt = verify 42

-- verifyChar = verify 'a'


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
empty :: forall xs. Empty xs => ListProxy xs -> ListProxy xs
empty = identity

checkEmpty = empty (ListProxy :: ListProxy Nil')

-- checkNonEmpty = empty (ListProxy :: ListProxy (TypeItem String :> Nil'))
```
