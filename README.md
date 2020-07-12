# IsDef.jl

This package provides primitives for dispatching on whether certain methods are implemented or not.
It exports two functions for general usage `isdef(f, Arg1Type, Arg2Type, ...)::Bool` and `Out(f, Arg1Type, Arg2Type)::ReturnType` which build upon an internal function `IsDef.return_type(f, Tuple{Arg1Type, Arg2Type, ...})::ReturnType`.


Use `isdef` / `Out` for your dispatch cases, but be aware that sometimes Julia's inference is only approximate.
The package does a lot to improve over the default inference, but there are still limitations, just that you know.


If you encounter limitations or too broad type-inference, you can always overload the underlying `return_type`
```julia
IsDef.return_type(::Type{Tuple{typeof(myfunction), Arg1Type, Arg2Type}}) = ReturnType
```
Specifically, if you want to indicate that a given function is not defined for certain argument types, you return `Union{}`
```julia
# Union{} denotes being undefined
IsDef.return_type(::Type{Tuple{typeof(myfunction), Arg1Type, Arg2Type}}) = Union{}  
```

## Installation & Import

```julia
using Pkg
pkg"registry add https://github.com/JuliaRegistries/General"  # central julia repository
pkg"registry add https://github.com/schlichtanders/SchlichtandersJuliaRegistry.jl"  # custom repository
pkg"add IsDef"
```

After installation, use the package by simply
```jldoctest global
using IsDef
```
which makes `isdef` and `Out` available.


## `isdef(f, ...)`

`isdef` checks whether a given function is defined for subsequent argument-types

```jldoctest global
julia> isdef(+, Int, Int)
true
julia> isdef(-, AbstractFloat)
true
julia> isdef(-, String)
false
julia> isdef(-, AbstractString)
false
```

Caution has to be taken for the type ``Any``. It is the only special case. It is interpreted as if type-inference was in-accurate, and the concrete values are actually something more specific than Any. This is why, ``Any`` always results in ``true``.

```jldoctest global
julia> isdef(-, Any)
true
```

## `Out(f, ...)`

`Out` follows the same syntax as `isdef` however instead of returning a Bool, it returns the actual inferred returntype.
```jldoctest global
julia> Out(Base.map, typeof(string), Vector{Int})
Vector{String}
```

If the function is not defined, it returns a special exported type `NotApplicable` (and not the standard convention `Union{}`). This ensures that `Out` can be used for dispatch.
```jldoctest global
julia> Out(-, AbstractString)
IsDef.NotApplicable
```

You can also do higher-order inference, e.g. when working with `Base.map`. Usually you would need a concrete function for its first argument, like
```jldoctest global
julia> Out(Base.map, typeof(isodd), Vector{Int})
Array{Bool, 1}
```
But thanks to the package ``FunctionWrappers`` you can define Function types also directly, without having a concrete function:
```jldoctest global
julia> import FunctionWrappers: FunctionWrapper
julia> Out(Base.map, FunctionWrapper{Bool, Tuple{Any}}, Vector{Int})
Vector{Bool}
```

--------------------------

Be cautious about `Any`, as it will usually work for every function, resulting again in an ``Any``.
```julia
Out(-, Any)  # Any
```


## What if I only have a function-type? Use `apply`

`IsDef` also exports a little helper function `apply` which you can use to infer with function-types instead of function-instances.
```jldoctest global
julia> isdef(apply, typeof(sin), Int)
true
julia> Out(apply, typeof(sin), Int)
Float64
```

The implementation in the background actually ensures that this takes into account custom fixes of ``IsDef.return_type``, too.
```jldoctest global
struct MyNewType end
IsDef.return_type(::Type{Tuple{typeof(identity), MyNewType}}) = Union{}
isdef(apply, typeof(identity), MyNewType)

# Output

false
```

!!! warning "This only works with the `IsDef.apply`"
    A custom apply won't work
    ```jldoctest global
    isdef((f, args...) -> f(args...), typeof(identity), MyNewType)

    # output

    true
    ```
