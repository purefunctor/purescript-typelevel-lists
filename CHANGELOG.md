# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased] - BREAKING!
### Added
* The `k` parameter for `List'`, enforcing homogeneity of kinds.
* Type class methods for operating on lists, verified using tests.
* The `Foldr` type class with the `foldr` method.
### Changed
* Dependencies in spago.dhall to fix builds with Spago 0.20.0.
### Removed
* Example code in favor of testing value-level functions.

## [1.1.0]
### Added
* The `Zip`, `Map`, and `Fold` type classes.
### Changed
* Documentation wording to remove references to `Item'`.

## [1.0.0]
### Changed
* Migrated from PureScript 0.13.x to PureScript 0.14.x, refactoring deprecated syntax.
### Removed
* The `Item'` kind and the `ItemProxy` data type.

## [0.3.x] 
### Added
* List and item kinds, proxies, and associated operations; compiles on PureScript 0.13.x.
