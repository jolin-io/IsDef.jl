module IsDef
export isdef, Out, NotApplicable, isapplicable, apply

using Compat
using IRTools
using IRTools: IR, @dynamo, recurse!

include("utils.jl")


"""
just applies a given function to arguments and keyword arguments

This little helper is crucial if you want to typeinfer
when only knowing the function type instead of the function instance.
"""
apply(f, args...; kwargs...) = f(args...; kwargs...)



# Core Interface
# ==============

struct NotApplicable end
isapplicable(::NotApplicable) = false
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
    Out(f, args...) = isdef(f, typeof.(args)...)

Returns outputtype of function application. Returns `IsDef.NotApplicable` if compiler notices that no Method can be
found.

When called on values, the values will be cast to types via use of `typeof` for convenience.

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
Out(f, args...) = Out(f, Core.Typeof.(args)...)
function Out(f, types::Vararg{<:Type})
  Out(apply, Core.Typeof(f), types...)
end
function Out(::typeof(apply), types::Vararg{<:Type})
  signature_type = Tuple{types...}
  Out(signature_type)
end


# Further Details
# ===============

include("generic.jl")
include("instances.jl")

end # module
