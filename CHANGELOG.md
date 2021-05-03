# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
Breaking changes:

New features:
* Allowed `Map` operations to have a different resulting kind.
* Replaced the `func` type variable in `Map` with a higher-kinded type `(k -> l)`.

Bugfixes:

Other improvements:
* Reworded [CHANGELOG.md](./CHANGELOG.md) to use a more concise format.

## [2.0.0](https://github.com/PureFunctor/purescript-typelevel-lists/releases/tag/v2.0.0)
Breaking changes:
* Added the `k` parameter for `List'`, enforcing homogeneity of kinds.

New features:
* Added the `Foldr` type class.
* Added type class methods for operating on lists, with examples in the [test](./test) directory.

Other improvements:
* Fixed dependency warnings emitted by Spago 0.20.0
* Removed example code in favor of testing type class methods.

## [1.1.0](https://github.com/PureFunctor/purescript-typelevel-lists/releases/tag/v1.1.0)
New features:
* Added the `Zip`, `Map`, and `Fold` type classes.

Other improvements:
* Reworded documentation to remove references to `Item'`.

## [1.0.0](https://github.com/PureFunctor/purescript-typelevel-lists/releases/tag/v1.0.0)
Breaking changes:
* Migrated from PureScript 0.13.x to PureScript 0.14.x, refactoring deprecated syntax.
* Removed the  `Item'` kind and the `ItemProxy` data type.

## [0.3.x](https://github.com/PureFunctor/purescript-typelevel-lists/releases/tag/v0.3.1)
New features:
* List and item kinds, proxies, and associated operations; compiles on PureScript 0.13.x.
