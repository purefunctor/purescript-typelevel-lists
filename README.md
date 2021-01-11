# purescript-typelevel-lists
Type-level heterogenous list of kinds for PureScript.

# Examples

``` purescript
import Prelude

import Prim.Boolean
import Type.Data.List


-- Membership testing
type Allowed = ( TypeItem Int :> TypeItem String :> Nil' )

verify :: forall t. IsMember ( TypeItem t ) Allowed True => t -> t
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
mustBeEmpty :: forall xs. IsEmpty xs True => ListProxy xs -> ListProxy xs
mustBeEmpty = identity

empty = mustBeEmpty ( ListProxy :: ListProxy Nil' )

mustHaveItems :: forall xs. IsEmpty xs False => ListProxy xs -> ListProxy xs
mustHaveItems = identity

items = mustHaveItems ( ListProxy :: ListProxy ( TypeItem Int :> Nil' ) )
```

# Installation (Pre-Release)
This package is still under development and expects a few more features to be added
(and some free time) before being published to the PureScript [registry](https://github.com/purescript/registry),
[package-sets](https://github.com/purescript/package-sets) and [Pursuit](https://pursuit.purescript.org/).

For now, this package can be installed and used in a project using Spago's `packages.dhall` file.

``` dhall
let upstream =
      -- <Package Set URL> 

let additions =
  { typelevel-lists =
    { repo = "https://github.com/PureFunctor/purescript-typelevel-lists.git"
    , version = "v0.1.0"
    , dependencies = [ "console" , "effect" , "psci-support" ]
    }
  }

in  upstream // additions
```

After which, it can then be installed using:

``` sh
$ spago install typelevel-lists
```

