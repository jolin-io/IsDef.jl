module IsDef
using Reexport
export apply
export isdef, Out, Out′
export NotApplicable, isapplicable
export promote_type_or_val, ValTypeof

using Compat

include("DataTypes.jl")
using .DataTypes

include("Utils/Utils.jl")
using .Utils.TypeUtils: kwftype, Tuple_value_to_type, NamedTuple_value_to_type, IntrinsicFunction
using .Utils.TypeValues: ensure_typevalue_or_type, Typeof, ValType, ValTypeof
using .Utils: _Core_return_type

"""
just applies a given function to arguments and keyword arguments

This little helper is crucial if you want to typeinfer
when only knowing the function type instead of the function instance.
"""
@inline apply(f, args...; kwargs...) = f(args...; kwargs...)



# Core Interface
# ==============

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
isdef(f, args::Vararg{<:Any, N}) where {N} = isdef(f, typeof.(args)...)
function isdef(f::F, types::Vararg{<:Type, N}) where {F, N}
  isdef(apply, Core.Typeof(f), types...)
end
function isdef(::typeof(apply), types::Vararg{<:Type, N}) where {N}
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
Out(f::F, types...; kwtypes...) where {F} = Out(apply, Core.Typeof(f), types...; kwtypes...)

function Out(::typeof(apply), types::Vararg{<:Any, N}; kwtypes...) where {N}
  types_ = map(ensure_typevalue_or_type, types)
  values_kwtypes = map(ensure_typevalue_or_type, values(kwtypes))

  if isempty(values(kwtypes))
    signature_type = Tuple_value_to_type(types_)
    Out(signature_type)
  else
    kw_type = NamedTuple_value_to_type(values_kwtypes)
    signature_type = tuple(kwftype(types_[1]), kw_type, types_...)
    Out(signature_type)
  end
end

# we support the all value version extra in order to not accidentily confuse the keyword argument usage
Out′(f::F, args::Vararg{<:Any, N}; kwargs...) where {F, N} = Out(f, map(Typeof, args)...; map(Typeof, kwargs.data)...)


# Generic Fallback using IR
# -------------------------

include("generic.jl")


# Ready Implementations of `Out`
# ------------------------------

include("functions-intrinsic.jl")
include("functions-builtin.jl")
include("functions-Base.jl")


# Type Inference Workarounds
# --------------------------

# TODO unfortunately this does not work yet for everything
# Cthulhu still tells that inference failed because of recursion

# define recursion_relation so that inference works better
# we probably have problems with true infinite cases now, but that should be still
# better in total

# It is a workaround this issue https://github.com/JuliaLang/julia/issues/45759

recursion_relation_always_true(@nospecialize(_...)) = true

for f in (isdef, Out, Out′, Out_implementation, _Out_implementation, _Out_dynamo, _Out_dynamo_implementation, _dynamointernals_innervalue_to_types_ANDTHEN_Out_ANDTHEN_dynamointernals_ensure_innervalue)
  for m in methods(f)
    if Base.moduleroot(m.module) === IsDef
      m.recursion_relation = recursion_relation_always_true
    end
  end
end

end  # module
