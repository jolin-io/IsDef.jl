```@meta
CurrentModule = IsDef
DocTestSetup = quote
    using IsDef
end
```

# Manual

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


## Automatic Code Generation

In case no custom inference rule could be found for `Out`, some type-inference code will be generated for you automatically.

This means that you only need to define custom inference rules in a very few situations.
Here the most typical cases for when automatic code generation won't be enough:
- the default inference is too wide (you see this if `Out` returns some abstract type like `Number`, but actually you know that the inference could a concreter type like `Integer` or `Int64`)
- the default inference is too inefficient (this is quite hard to track down, but may be seen by profiling your code).
- you get a warning which explicitly recommends to define a custom inference rule. ⚠️ Note that `isdef` will not print warnings, you need to use `Out` to see warnings ⚠️.

If the automatic code generation fails, `Out` falls back to `IsDef.Core_return_type`, which wraps `Core.Compiler.return_type`. In most cases this leads good inference results, but sometimes infers types inaccurately, in which case `IsDef.Core_return_type` prints the above mentioned warning and returns a special return type `IsDef.UnsureWhetherApplicable` instead. ⚠️ While this is easily inspectible when using `Out` directly, please note that `isdef` follows a conservative semantics and hence will return `false` in such cases ⚠️.


## `isdef(f, ...)`

`isdef` checks whether a given function is defined for subsequent argument-types.
Note that in some cases `isdef` returns `false` despite the method being properly implemented for the given types (see the section [Automatic Code Generation](#automatic-code-generation) for details).

```jldoctest global
julia> isdef(+, Int, Int)
true
julia> isdef(-, Float32)
true
julia> isdef(-, String)
false
julia> isdef(-, AbstractString)
false
julia> isdef(-, Any)
false
```

## `Out(f, ...)`

`Out` follows the same syntax as `isdef` however instead of returning a Bool, it returns the actual inferred returntype.

<!-- the output of Type aliases like Vector changed between julia versions, hence we don't use jldoctest -->

```julia
julia> Out(Base.map, typeof(string), Vector{Int})
Vector{String} (alias for Array{String, 1})
```

If the function is not defined, it returns a special exported type `NotApplicable`. This ensures that `Out` can safely be used for dispatch.
```jldoctest global
julia> Out(-, AbstractString)
NotApplicable
```

You can also do higher-order inference, e.g. when working with `Base.map`. Usually you would need a concrete function for its first argument, like

<!-- the output of Type aliases like Vector changed between julia versions, hence we don't use jldoctest -->

```julia
julia> Out(Base.map, typeof(isodd), Vector{Int})
Vector{Bool} (alias for Array{Bool, 1})
```

Thanks to the package `FunctionWrappers` you can define Function types also directly, without having a concrete function:

<!-- the output of Type aliases like Vector changed between julia versions, hence we don't use jldoctest -->

```julia
julia> import FunctionWrappers: FunctionWrapper

julia> Out(Base.map, FunctionWrapper{Bool, Tuple{Any}}, Vector{Int})
Vector{Bool} (alias for Array{Bool, 1})
```

---

The special type `Any` will usually result in a `NotApplicable`.

```jldoctest global
julia> Out(-, Any)
NotApplicable
```

## What if I only have a function-type? Use `apply`.

`IsDef` also exports a little helper function `apply` which you can use to infer with function-types instead of function-instances.

```jldoctest global
julia> isdef(apply, typeof(sin), Int)
true
julia> Out(apply, typeof(sin), Int)
Float64
```

```@meta
DocTestSetup = nothing
```

## ValTypes

Julia's type inference does take into account types as well as some of values.
For instance, if you use a boolean variable somewhere which is known to be `true`, and have an if-else using it,
julia may well be able to statically optimize your code accordingly.

In order to simplify maintainable type inference which takes into account such value-level
type inference, `IsDef` exports a special Type `ValType` with separate constructor `ValTypeof`.
(Helper functions along this type can be found in the exported submodule `ValTypes`).

```jldoctest global
julia> ValTypeof(true)
ValType{Bool, true}
```

`isdef`, `Out` (and also `IsDef.Core_return_type`) all work on these ValTypes as well and
may return ValTypes.
```jldoctest global
julia> isdef(-, ValTypeof(3))
true

julia> Out(!, ValTypeof(true))
ValType{Bool, false}

julia> IsDef.Core_return_type(Tuple{typeof(&), ValTypeof(true), ValTypeof(false)})  # wraps Core.Compiler.return_type
ValType{Bool, false}

julia> Out(Tuple{typeof(|), ValTypeof(true), ValTypeof(false)})
ValType{Bool, true}
```

This way you can dispatch on concrete values when specifying custom inference rules.

If you want to dispatch on standard `Int` or `Symbol` or another bits-type, you can still do so:
Just combine the plain type with its ValType analog in a Union type like `Union{Int, ValType{Int}}`.
```jldoctest global
julia> ValTypeof(1)    <: Union{Int, ValType{Int}}
true
julia> ValTypeof(:two) <: Union{Int, ValType{Int}}
false
julia> ValTypeof(1)    <: (Union{T, ValType{T}} where T <: Union{Symbol, Int})
true
julia> ValTypeof(:two) <: (Union{T, ValType{T}} where T <: Union{Symbol, Int})
true
```
