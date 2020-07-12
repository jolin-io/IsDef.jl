# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2020-07-12
### Changed
- ``IsDef.return_type(::Type{<:Tuple})`` now takes a single Tuple type, which includes the function type
### Added
- when you overload `IsDef.return_type` for your custom type inference fixes, this is now also taken into account when using `apply`
- GithubActions for CI, Codecov and Documentation
- Documentation using Documenter.jl


## [0.2.0] - 2020-03-23
### Changed
- renamed ``IsDef._return_type`` to ``IsDef.return_type`` for analogy with `Core.Compiler.return_type`
- `Function` is now dealt like `Any` in the sense that no attempt is made to break it down into subtypes

### Fixed
- `isdef`/`Òut`/`return_type` now work correctly on `Type{...}` constructs. There was an unexpected behaviour with InteractiveUtils.subtypes which is now worked around.

## [0.1.0] - 2020-03-06

initial release
