module IsDef
export apply
export isdef, Out, Out′
export NotApplicable, isapplicable
export promote_type_or_typevalue

using Compat
using IRTools
using IRTools: IR, @dynamo, recurse!, xcall

include("utils.jl")

"""
just applies a given function to arguments and keyword arguments

This little helper is crucial if you want to typeinfer
when only knowing the function type instead of the function instance.
"""
apply(f, args...; kwargs...) = f(args...; kwargs...)



# Core Interface
# ==============

# we decided to use type-level because `Out` normally returns types.
# hence it is more intuitive to use type, but also more natural, as `Out` may be used within type range as `Out(f, ...) <: NotApplicable`.
struct NotApplicable
  NotApplicable() = error("Please use `NotApplicable` type instead of `NotApplicable()` instance.")
end

isapplicable(::Type{NotApplicable}) = false
isapplicable(other) = true

"""
    isdef(func, ArgType1, ArgType2, ...)::Bool
    isdef(f, args...) = isdef(f, typeof.(args)...)

Checks whether the function is defined for the actual types or not.
This works in compile time and hence can be used to optimize code.

When called on values, the values will be cast to types via use of `typeof` for convenience.

!!! warning "CAUTION"
    If `isdef(...) == true`, still a MethodError might happen at runtime. This is due to incomplete type inference.

    SOLUTION: Overload `IsDef.return_type` if you experience unexpected behaviour for your types

    For instance to say that some call like `myfunc(::Int, ::String)` is not defined, then define the following
    ```julia
    function IsDef.return_type(::Type{Tuple{typeof(myfunc), TypeArg1, TypeArg2}})
      Union{}  # return empty Union to indicate something is not defined
    end
    ```

Semantics
---------
The implementation follows a closed-world semantics in that `isdef` will return true
for a given abstract type, if the function is defined for all currently defined leaf types of that
abstract type.

The only exceptions currently are `Any`, `Function`,  and `Exception`, for which `isdef` returns `true`.
"""
isdef(f, args...) = isdef(f, typeof.(args)...)
function isdef(f, types::Vararg{<:Type})
  isdef(apply, Core.Typeof(f), types...)
end
function isdef(::typeof(apply), types::Vararg{<:Type})
  signature_type = Tuple{types...}
  isapplicable(Out(signature_type))
end


"""
    Out(func, ArgType1, ArgType2, ...)::ReturnType
    Out′(f, args...) = Out(f, typeof.(args)...)

Returns outputtype of function application. Returns `IsDef.NotApplicable` if compiler notices that no Method can be
found.

!!! warning "CAUTION"
    If `Out(...) == Any`, still a MethodError might happen at runtime. This is due to incomplete type inference.

    SOLUTION: Overload `IsDef.return_type` if you experience unexpected behaviour for your types

    For instance to say that some call like `myfunc(::Int, ::String)` is not defined, then define the following
    ```julia
    function IsDef.return_type(::Type{Tuple{typeof(myfunc), TypeArg1, TypeArg2}})
      Union{}  # return empty Union to indicate something is not defined
    end
    ```

UnionAll types and abstract types are concretified to the Union of their existing subtypes, in order
to improve type-inference. The only exceptions so far are `Any`, `Function` and `Exception`, as they have way too
many subtypes to be of practical use.
"""
Out(f, types::Vararg{Any, N}; kwtypes...) where {N} = Out(apply, Core.Typeof(f), types...; kwtypes...)

function Out(::typeof(apply), types::Vararg{Any, N}; kwtypes...) where {N}
  foreach(ensure_typevalue_or_type, types)
  foreach(ensure_typevalue_or_type, values(kwtypes))
  
  if isempty(values(kwtypes))
    signature_type = Tuple_value_to_type(types)
    Out(signature_type)
  else
    kw_type = NamedTuple_value_to_type(values(kwtypes))
    signature_type = tuple(kwftype(types[1]), kw_type, types...)
    Out(signature_type)
  end
end

ensure_typevalue_or_type(type::Type) = type
function ensure_typevalue_or_type(other)
  istypevalue(other) || error("need either isbits, Symbol or plain Type as argument, bot got `$other`. See `istypevalue` for details.")
  other
end


# we support the all value version extra in order to not accidentily confuse the keyword argument usage
Out′(f, args::Vararg{Any, N}; kwargs...) where N = Out(f, map(to_type_or_typevalue, args)...; map(to_type_or_typevalue, kwargs.data)...)

# TODO this looks like it needs to be merged with extract_type_or_typevalue
to_type_or_typevalue(a) = istypevalue(a) ? a : Core.Typeof(a)
to_type_or_typevalue(a::Function) = Core.Typeof(a)  # functions are isbits surprisingly. Still it seems slightly more convenient to work on type level, as we can use Union then


# Generic Fallback using IR
# -------------------------

include("generic.jl")


# Ready Implementations of `Out`
# ------------------------------

include("functions-intrinsic.jl")
include("functions-builtin.jl")
include("functions-Base.jl")

end  # module
