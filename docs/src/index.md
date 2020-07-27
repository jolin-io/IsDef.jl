# IsDef.jl

This package provides primitives for dispatching on whether certain methods are implemented or not.
It exports two functions for general usage
* `isdef(f, Arg1Type, Arg2Type, ...)::Bool` and
* `Out(f, Arg1Type, Arg2Type)::ReturnType`
which build upon an internal function `IsDef.return_type(Tuple{typeof(f), Arg1Type, Arg2Type, ...})::ReturnType`.


## Installation

```julia
using Pkg
pkg"registry add https://github.com/JuliaRegistries/General"  # central julia registry
pkg"registry add https://github.com/schlichtanders/SchlichtandersJuliaRegistry.jl"  # custom registry
pkg"add IsDef"
```

After installation, use the package by simply
```julia
using IsDef
```
which makes `isdef` and `Out` available.


## Manual Outline

```@contents
Pages = ["manual.md"]
```

## [Library Index](@id main-index)

```@index
Pages = ["library.md"]
```
