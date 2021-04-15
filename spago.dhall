{ name = "typelevel-lists"
, dependencies =
  [ "prelude"
  , "tuples"
  , "typelevel-peano"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/PureFunctor/purescript-typelevel-lists"
}
