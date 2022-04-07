using IsDef.Utils.TypeValues: promote_type_or_val

# Addition and Multiplication
# ---------------------------

# TODO, is this always true?
Out(::Type{<:Tuple{Union{typeof(+), typeof(-), typeof(*), typeof(/)}, T1, T2}}) where {T1, T2} = promote_type_or_val(T1, T2)
Out(::Type{<:Tuple{Union{typeof(+), typeof(-), typeof(*), typeof(/)}, Vararg{T}}}) where T = T


# Comparison
# ----------

function Out(::Type{Tuple{F, T1, T2}}) where {F <: Union{typeof(<), typeof(>), typeof(<=), typeof(>=)}, T1, T2}
  _Out_comparison(F, T1, T2)
end
@generated function _Out_comparison(::Type{F}, ::Type{T1}, ::Type{T2}) where {F<:Function, T1<:VAL, T2<:VAL}
  F.instance(get(T1), get(T2))
end
_Out_comparison(::Type, ::Type, ::Type) = Bool


# Type Conversion
# ---------------

function Out(signature::Type{Tuple{typeof(promote_type), Type{T1}, Type{T2}}}) where {T1, T2}
  # TODO hassignature cannot handle VAL types currently
  hassignature(signature) || return NotApplicable  # TODO: we are assuming that if there is a method, it also works
  Core.Typeof(promote_type(T1, T2))
end

function Out(signature::Type{Tuple{typeof(convert), Type{T1}, T2}}) where {T1, T2}
  # TODO hassignature cannot handle VAL types currently
  hassignature(signature) || return NotApplicable  # TODO: we are assuming that if there is a method, it also works
  T1
end


# Container
# ---------

# similar
# .......

# generic fallbacks onto simple method
function Out(signature::Type{Tuple{typeof(similar), Type{Container}, Type{Element}}}) where {Container, Element}
  Out(Tuple{typeof(similar), Container, Element})
end
function Out(signature::Type{Tuple{typeof(similar), Container, Type{Element}}}) where {Container, Element}
  Out(Tuple{typeof(similar), Container, Element})
end
function Out(signature::Type{Tuple{typeof(similar), Type{Container}, Element}}) where {Container, Element}
  Out(Tuple{typeof(similar), Container, Element})
end

# function Out(signature::Type{Tuple{typeof(similar), <:Vector, Element}}) where {Element}
#   Vector{Element}
# end

function Out(signature::Type{Tuple{typeof(map), F, A}}) where {F, A}
  new_element_type = Out(apply, F, eltype(A))
  Out(Tuple{typeof(similar), F, new_element_type})
end