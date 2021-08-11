
# Addition and Multiplication
# ---------------------------

# TODO, is this always true?
Out(::Type{<:Tuple{Union{typeof(+), typeof(-), typeof(*), typeof(/)}, T1, T2}}) where {T1, T2} = promote_types_or_typevalues(T1, T2)
Out(::Type{<:Tuple{Union{typeof(+), typeof(-), typeof(*), typeof(/)}, Vararg{T}}}) where T = T


# Comparison
# ----------

function Out(::Type{Tuple{F, T1, T2}}) where {F <: Union{typeof(<), typeof(>), typeof(<=), typeof(>=)}, T1, T2}
  _Out_comparison(F, wrap_typevalue_into_Val(T1), wrap_typevalue_into_Val(T2))
end
@generated function _Out_comparison(::Type{F}, ::Val{T1}, ::Val{T2}) where {F, T1, T2}
  F.instance(T1, T2) 
end
_Out_comparison(::Type, ::Type, ::Type) = Bool


# Type Conversion
# ---------------

function Out(signature::Type{Tuple{typeof(promote_type), Type{T1}, Type{T2}}}) where {T1, T2}
  hassignature(signature) || return NotApplicable  # TODO: we are assuming that if there is a method, it also works
  Core.Typeof(promote_type(T1, T2))
end

function Out(signature::Type{Tuple{typeof(convert), Type{T1}, T2}}) where {T1, T2}
  hassignature(signature) || return NotApplicable  # TODO: we are assuming that if there is a method, it also works
  T1
end
