# IsDef.jl

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://schlichtanders.github.io/IsDef.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://schlichtanders.github.io/IsDef.jl/dev)
[![Build Status](https://github.com/schlichtanders/IsDef.jl/workflows/CI/badge.svg)](https://github.com/schlichtanders/IsDef.jl/actions)
[![Coverage](https://codecov.io/gh/schlichtanders/IsDef.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/schlichtanders/IsDef.jl)


This package provides primitives for dispatching on whether certain methods are implemented or not.

For installation or usage, open julia repl and run
```julia
using IsDef
```
which gives you access to following functions

- `isdef(f, Arg1Type, Arg2Type, ...)::Bool` checks whether a function is defined for the given types.
- `Out(f, Arg1Type, Arg2Type)::ReturnType` returns the returntype of the given functioncall. Note, that it may return an abstract type that is wider than necessary, like e.g. `Any`. `Out` is internally used by `isdef`.

Internally of `Out(f, Arg1Type, Arg2Type)` a one-argument-version of `Out` is used which expects a single Tuple type, specifying the entire call signature. This is the heart of the `IsDef` package. For the example it would be
- `Out(Tuple{typeof(f), Arg1Type, Arg2Type})`

If you want to specify inference of your method (output of `Out`), or whether it is defined (output of `isdef`), you need to overload this very one-argument method of `Out`. For the example it could be
- `Out(::Type{<:Tuple{typeof(f), Arg1Type, Arg2Type, Vararg}}) = ReturnType`

Enjoy maintainable typeinference.