# IsDef.jl

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://schlichtanders.github.io/IsDef.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://schlichtanders.github.io/IsDef.jl/dev)
[![Build Status](https://github.com/schlichtanders/IsDef.jl/workflows/CI/badge.svg)](https://github.com/schlichtanders/IsDef.jl/actions)
[![Coverage](https://codecov.io/gh/schlichtanders/IsDef.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/schlichtanders/IsDef.jl)


This package provides primitives for dispatching on whether certain methods are implemented or not.
It exports two functions for general usage
* `isdef(f, Arg1Type, Arg2Type, ...)::Bool` and
* `Out(f, Arg1Type, Arg2Type)::ReturnType`

which build upon an internal function `IsDef.return_type(Tuple{typeof(f), Arg1Type, Arg2Type, ...})::ReturnType`.


Use `isdef` / `Out` for your dispatch cases, but be aware that sometimes Julia's inference is only approximate.
The package does a lot to improve over the default inference, but there are still limitations, just that you know:
1. sometimes the inferred type may be more general then the real concrete type
2. sometimes a type is inferred, while in real an error is thrown (this especially holds true if one of your ArgTypes is `Any`)

Accordingly, here some notes about safety:
* `isdef` is usually safe to use, as you don't care about the specific return type. In the rare case that some function was actually not defined despite saying so, you will get a loud MethodError at runtime, precisely stating which method was not defined. Then you can fix the missing type-inference yourself by overloading `IsDef.return_type` (see below) and you are safe to go.
* `Out` is definitely trickier, as here the precise type may matter. The general recommendation is actually to not use this kind of custom type-inference at all, but instead always try to use values directly and let the compiler optimise the code instead. For more information see for example [this discourse discussion](https://discourse.julialang.org/t/promote-op-and-preallocating-result-of-linear-operators/11332/2) about the similar function `Base.promote_op`. Use `Out` only if you cannot do the same with values only, e.g. if you work with empty containers which don't have any values, or some fancy constructs relying on functions. In addition, make sure it is okay for your usecase that `Out` may return more general types in some cases. That should be enough for the warning.


If you encounter limitations or too broad type-inference, you can always overload the underlying `return_type`
```julia
IsDef.return_type(::Type{Tuple{typeof(myfunction), Arg1Type, Arg2Type}}) = ReturnType
```
Specifically, if you want to indicate that a given function is not defined for certain argument types, you return `IsDef.NotApplicable`
```julia
# Union{} denotes being undefined
IsDef.return_type(::Type{Tuple{typeof(myfunction), Arg1Type, Arg2Type}}) = NotApplicable  
```

For more details check out the [documentation](https://schlichtanders.github.io/IsDef.jl/dev/).

## Installation

```julia
using Pkg
pkg"registry add https://github.com/JuliaRegistries/General"  # central julia repository
pkg"registry add https://github.com/schlichtanders/SchlichtandersJuliaRegistry.jl"  # custom repository
pkg"add IsDef"
```

After installation, use the package by simply
```julia
using IsDef
```
which makes `isdef` and `Out` available.
