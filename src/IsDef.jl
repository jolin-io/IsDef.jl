module IsDef
using Reexport
export apply
export isdef, Out, Out′
export NotApplicable, isapplicable
export promote_type_or_valtype, ValType, ValTypeof

using Compat

"""
just applies a given function to arguments and keyword arguments

This little helper is crucial if you want to typeinfer
when only knowing the function type instead of the function instance.
"""
@inline apply(f, args...; kwargs...) = f(args...; kwargs...)


include("DataTypes.jl")
using .DataTypes

include("Utils/Utils.jl")
using .Utils.TypeUtils: kwftype, Tuple_value_to_type, NamedTuple_value_to_type, IntrinsicFunction
using .Utils.ValTypes: valtype_apply, ensure_valtype_or_type, Typeof, ValType, ValTypeof
using .Utils: Core_return_type


# Core Interface
# ==============

"""
    isdef(func, ArgType1, ArgType2, ...)::Bool
    isdef(f, args...) = isdef(f, typeof.(args)...)

Checks whether the function is defined for the actual types or not.
This works in compile time and hence can be used to optimize code.

When called on values, the values will be cast to types via use of `typeof` for
convenience.
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

Returns outputtype of function application. Returns `IsDef.NotApplicable` if compiler
notices that no Method can be found.
"""
function Out(f::F, types::Vararg{<:Any, N}; kwtypes...) where {F<:Function, N}
    Out(apply, Core.Typeof(f), types...; kwtypes...)
end

# one extra dispatch for general F, however needing at least one argument
function Out(f::F, type1, types::Vararg{<:Any, N}; kwtypes...) where {F, N}
    # type1 is just to distinguish this dispatch from the standard Out(Tuple{...}) call
    Out(apply, Core.Typeof(f), type1, types...; kwtypes...)
end


function Out(::typeof(apply), types::Vararg{<:Any, N}; kwtypes...) where {N}
    types_ = map(ensure_valtype_or_type, types)
    values_kwtypes = map(ensure_valtype_or_type, values(kwtypes))

    if isempty(values(kwtypes))
        signature_type = Tuple_value_to_type(types_)
        Out(signature_type)
    else
        kw_type = NamedTuple_value_to_type(values_kwtypes)
        signature_type = tuple(kwftype(types_[1]), kw_type, types_...)
        Out(signature_type)
    end
end

# Generic Fallback using IR
# -------------------------

include("generic.jl")


# Ready Implementations of `Out`
# ------------------------------

include("functions-intrinsic.jl")
include("functions-builtin.jl")
include("functions-Base.jl")

end  # module
