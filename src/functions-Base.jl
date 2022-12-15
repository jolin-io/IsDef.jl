using IsDef.Utils.ValTypes: promote_type_or_valtype, ValType, ValTypeof

# Bool operations
# ---------------

# operators with 2 arguments

function Out(::Type{Tuple{F, T1, T2}}) where {F <: Union{typeof(<), typeof(>), typeof(<=), typeof(>=)}, T1, T2}
    _Out_comparison(F, T1, T2)
end
@generated function _Out_comparison(::Type{F}, ::Type{T1}, ::Type{T2}) where {F<:Function, T1<:ValType, T2<:ValType}
    ValTypeof(F.instance(get(T1), get(T2)))
end
_Out_comparison(::Type, ::Type, ::Type) = Bool

# operators with 1 argument
Out(::Type{Tuple{typeof(!), ValType{Bool, V}}}) where {V} = ValType{Bool, !V}
Out(::Type{Tuple{typeof(!), Bool}}) = Bool



# Type Conversion
# ---------------

function Out(signature_typevalues::Type{Tuple{typeof(promote_type), Type{T1}, Type{T2}}}) where {T1, T2}
    # TODO hassignature cannot handle ValType types currently
    signature_notypevalues = signature_without_valtypes(signature_typevalues)
    static_hasmethod(signature_notypevalues) || return NotApplicable  # TODO: we are assuming that if there is a method, it also works
    Core.Typeof(promote_type_or_valtype(T1, T2))
end

function Out(::Type{Tuple{typeof(convert), Type{T1}, T2}}) where {T1, T2}
    signature_notypevalues = signature_without_valtypes(signature_typevalues)
    static_hasmethod(signature_notypevalues) || return NotApplicable  # TODO: we are assuming that if there is a method, it also works
    T1
end


# Container
# ---------


# map
# ...

function Out(::Type{Tuple{typeof(map), F, A}}) where {F, A}
    eltypeA = eltype(A)
    if eltypeA == Any
        # if the eltype is Any, it is usually better to use Core_return_type
        # TODO however this makes Out on map less reliable
        return IsDef.Core_return_type(Tuple{typeof(map), F, A})
    else
        new_element_type = Out(apply, F, eltypeA)
        new_element_type !== NotApplicable || return NotApplicable
        return Out(Tuple{typeof(similar), A, Type{new_element_type}})
    end
end


# iterate
# .......

# IsDef.Out(::Type{Tuple{typeof(iterate), OrdinalRange{T}, Any} where T}) = ...
