# purescript-typelevel-lists
Type-level heterogenous list of kinds for PureScript.

## Documentation
Documentation is published in [Pursuit](https://pursuit.purescript.org/packages/purescript-typelevel-lists).

## Examples
Examples can be found in the [examples](./examples) directory.

## Installation (Pre-Release)
This package is still under development and expects a few more features to be added
(and some free time) before being published to the PureScript [registry](https://github.com/purescript/registry),
[package-sets](https://github.com/purescript/package-sets).

For now, this package can be installed and used in a project using Spago's `packages.dhall` file.

``` dhall
let upstream =
      -- <Package Set URL> 

let additions =
  { typelevel-lists =
    { repo = "https://github.com/PureFunctor/purescript-typelevel-lists.git"
    , version = "v0.3.0"
    , dependencies = [ "typelevel-peano" ]
    }
  }

in  upstream // additions
```

After which, it can then be installed using:

``` sh
$ spago install typelevel-lists
```

