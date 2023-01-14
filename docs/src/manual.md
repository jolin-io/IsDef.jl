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
- `isdef(f, arg1, arg2)::Bool` / `isdef(f, Arg1Type, Arg2Type)::Bool` checks whether a function is defined for the given types.

    If at least one of the arguments is not a type,
    all arguments are automatically converted to types for you.

- `Out(f, arg1, arg2)::ReturnType` / `Out(f, Arg1Type, Arg2Type)::ReturnType` returns the returntype of the given functioncall.

    Note, that `Out` may return an abstract type that is wider than necessary, like e.g. `Any`.
    If a functioncall is not defined, or predictably throws an error, `IsDef.NotApplicable` is returned.
    `Out` is internally used by `isdef`.

Internally of `Out(f, Arg1Type, Arg2Type)` a one-argument-version of `Out` is used which expects a single Tuple type, specifying the entire call signature. This is the heart of the `IsDef` package. For the example it would be
- `Out(Tuple{typeof(f), Arg1Type, Arg2Type})`

If you want to specify inference of your method (output of `Out`), or whether it is defined (output of `isdef`), you need to overload this very one-argument method of `Out`. For the example it could be
- `Out(::Type{<:Tuple{typeof(f), Arg1Type, Arg2Type, Vararg}}) = ReturnType`

Enjoy maintainable type inference.


## Example application: Use `isdef` for Traits

Traits are a common mean to get back something like multiple inheritance to Julia.

Let's say you want to dispatch on anything which provides a certain interface, which in
julia simply means that a set of respective functions are defined for the type.

```julia
struct MyType end
interface_func1(::MyType) = :hello
interface_func2(::MyType) = "world"
```

Then the standard Trait pattern (sometimes called Holy Traits) goes like follows:
```julia
abstract type AbstractMyTrait end
struct SupportsMyTrait <: AbstractMyTrait end

traits_func(a::A) where A = traits_func(AbstractMyTrait(A), a)
traits_func(::SupportsMyTrait, a) = println(interface_func1(a), interface_func2(a))
```
This defines a generic function which dispatches on our given trait type. Yet the trait needs
to be associated with our interface function. This is usually done by mere convention,
and we just link the trait-type with our custom type, assuming all functions are
readily defined
```julia
AbstractMyTrait(::Type{MyType}) = SupportsMyTrait()  # use instance
```
Now our traits function works
```julia
traits_func(MyType())
# helloworld
```

### No boilerplate with `isdef`

The standard Traits implementation creates a lot of boilerplate. Do you really need to define these extra abstract trait types, trait structs and connection between your custom type and the trait type?

You already defined that `MyType` implements `my_func`. We can reuse that information with `isdef`.
This is how to rewrite the above `traits_func` (we use `Val` to dispatch on `Bool` values):
```julia
using IsDef
supports_interface(a) = isdef(interface_func1, a) && isdef(interface_func2, a)
traits_func_new(a) = traits_func_new(Val{supports_interface(a)}(), a)
traits_func_new(::Val{true}, a) = println(interface_func1(a), interface_func2(a))
```
Which indeed works without defining any extra traits type:
```julia
traits_func_new(MyType())
# helloworld
```

The nested dispatch syntax, as well as the use of `Val` can be simplified further
by using a dedicated Trait systems like [WhereTraits.jl](https://github.com/jolin-io/WhereTraits.jl).
```julia
using WhereTraits
@traits function traits_func_new2(a) where {supports_interface(a)}
    println(interface_func1(a), interface_func2(a))
end
traits_func_new2(MyType())
# helloworld
```

Note that `isdef` does not rely on convention, but checks whether the interface functions are defined or not.

If `isdef` does return `false` despite it should be true, the fallback type inference
was not good enough and you need to overload `Out` for either your interface functions
or functions used within these.
⚠️ Never overload `isdef`, instead always overload `Out(::Type{<:Tuple{...}})` ⚠️.


## Why not using Julia's builtin type-inference directly?

Julia already has a type inference method, namely `Base.promote_op` which internally uses `Core.Compiler.return_type`.
So why do we need another type inference layer like `IsDef`?

### The problem: Instability

The issue with `Core.Compiler.return_type` is that we cannot rely on its output.
Sometimes just changing the order of Julia code will change the type inference.
Think of it as an implementation detail of Julia itself.

Here the official warning in the `Base.promote_op` docs:
> Due to its fragility, use of promote_op should be avoided. It is preferable to base the container eltype on the type of the actual elements. Only in the absence of any elements (for an empty result container), it may be unavoidable to call promote_op.

And even using `Base.promote_op` for empty containers is not a good idea, use `Any` instead, or `Union{}`.
Relying on an indeterministic implementation detail which may silently change its behaviour
on every minor julia version is just bad practice.

### Where IsDef's stability comes from

That said, Julia's type-inference is very useful in that it is the best approximation
we can get without any extra knowledge (remember, the worst type inference approximation is always `Any`).
Hence it would be great to somehow use it in a safe manner.

`IsDef` uses Julia's default type inference as a fallback, and adds three safety aspects:
- `IsDef` uses automatice deterministic code generation where possible in order to entirely circumvent Julia's internal type inference
- `IsDef` makes the type inference overloadable, so that if some fallback type inference changes between Julia versions, you can add a custom inference rule yourself to fix it.
- [ValTypes](#valtypes) define a clean interface to work with bits values as part of type inference.


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

Julia's type inference does take into account types as well as some values.
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

Limitations: Currently, Julia's typeinference does not work well if the value is not known
at compile time. Practically, this means you should use `ValTypeof` only for constants
or for values which you extracted from another `ValType`. (See [this discourse thread](https://discourse.julialang.org/t/surprising-type-widening-when-constructing-tuple/92840)
for more details.)