module IsDef
using Reexport
export apply
export isdef, Out
export isdef_debug, Out_debug
export ApplicabilityProblem, NotApplicable, UnsureWhetherApplicable, isapplicable
export ValTypes, ValType, ValTypeof

using Compat

"""
just applies a given function to arguments and keyword arguments

This little helper is crucial if you want to typeinfer
when only knowing the function type instead of the function instance.
"""
@inline apply(f, args...; kwargs...) = f(args...; kwargs...)

include("Utils/Utils.jl")
using .Utils.IOUtils: suppress_warnings
using .Utils.TypeUtils: Typeof
using .Utils.ValTypes: ValTypes, valtype_apply, ValType, ValTypeof
using .Utils.CoreReturnType: Core_return_type
using .Utils.Applicabilities: ApplicabilityProblem, NotApplicable, UnsureWhetherApplicable, isapplicable


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
function isdef(f::F, types::Vararg{Any, N}; kwtypes...) where {F, N}
    suppress_warnings() do
        # we use IsDef.Typeof(f) to ensure, that the callable is always interpreted value-like
        isapplicable(Out(apply, IsDef.Typeof(f), types...; kwtypes...))
    end
end

# function isdef(f, types...; kwtypes...)
#     error("`isdef` expects types as arguments, but got `isdef($(types...); $(kwtypes...))`")
# end


"""
    Out(func, ArgType1, ArgType2, ...)::ReturnType
    Out(func, bytevalue1, ArgType2, ...)::ReturnType

Returns outputtype of function application. Returns `IsDef.NotApplicable` if compiler
notices that no Method can be found.
"""
function Out(f::F, types::Vararg{Any, N}; kwtypes...) where {F<:Function, N}
    # we use IsDef.Typeof(f) to ensure, that the callable is always interpreted value-like
    Out(apply, IsDef.Typeof(f), types...; kwtypes...)
end

# one extra dispatch for general F, however needing at least one extra argument so that it is not confused with the generic Out(Tuple{...}) case
function Out(f::F, type1::Any, types::Vararg{Any, N}; kwtypes...) where {F, N}
    # type1 is just to distinguish this dispatch from the standard Out(Tuple{...}) call
    # we use IsDef.Typeof(f) to ensure, that the callable is always interpreted value-like
    Out(apply, IsDef.Typeof(f), type1, types...; kwtypes...)
end

function Out(::typeof(apply), typef, types::Vararg{Any, N}; kwtypes...) where {N}
    # as a compromise between convenience and safety, we either convert
    # all arguments to types or none.
    _types = Utils.got_some_nontypes(types) ? map(IsDef.Typeof, types) : types
    types_final = tuple(typef, _types...)

    _kwtypes = values(kwtypes)
    if isempty(_kwtypes)
        signature_type = Utils.Tuple_value_to_type(types_final)
        Out(signature_type)
    else
        kwtypes_final = Utils.got_some_nontypes(_kwtypes) ? map(IsDef.Typeof, _kwtypes) : _kwtypes
        kwtype = Utils.NamedTuple_value_to_type(kwtypes_final)
        signature_type = tuple(Utils.kwftype(types_final[1]), kwtype, types_final...)
        Out(signature_type)
    end
end


# Debug helper
# ============

include("Debug.jl")
using .Debug: Out_debug, isdef_debug

# Generic Fallback using IR
# =========================

include("generic.jl")


# Ready Implementations of `Out`
# ==============================

include("functions-intrinsic.jl")
include("functions-builtin.jl")
include("functions-Base.jl")

end  # module
