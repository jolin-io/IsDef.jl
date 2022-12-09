module TypeValues

export istypevalue, istypevaluetype, ValType, ValTypeof, signature_without_typevalues, promote_type_or_val, ensure_typevalue_or_type

using IsDef.Utils.TypeUtils: Tuple_type_to_value, Tuple_value_to_type, NamedTuple_value_to_type, NamedTuple_type_to_value


# typevalues & ValType
# ----------------

# The ValType is only used in signatures, and will never be seen as a concretevalue moving through the functions
# the reason is that we want to resuse as much code as possible of existing function implementations,
# hence dealing with plain values instead of ValType is simpler.
# This is most striking in the case of Bool true false, which have special handling with branches, where a ValType will immediately fail.

"""
    istypevalue(1)

like `Base.isbits`, however accounts for surprings facts like Symbol not being bits type.

"typevalue" is understood as in the documentation about Value Types https://docs.julialang.org/en/v1/manual/types/#%22Value-types%22
"""
istypevalue(::T) where T = istypevaluetype(T)

"""
    istypevaluetype(Int)

like `Base.isbitstype`, however accounts for surprings facts like Symbol not being bits type.

"typevalue" is understood as in the documentation about Value Types https://docs.julialang.org/en/v1/manual/types/#%22Value-types%22
"""
istypevaluetype(::Type{Symbol}) = true
istypevaluetype(::Type{<:Function}) = false  # functions are isbits, however are treated on type bases everywhere in Base, e.g. also in the standard signature types called ftypes.
istypevaluetype(type) = isbitstype(type)


"""
    ValType(value) -> ValType{typeof(value), value}

Alternative to Base.Val which makes the type of the value accessible for dispatch.

In order to retrieve the value you can simple use `Base.get` on either the instance or the type.

```julia
julia> ValTypeof(1)
ValType{Int, 1}

julia> get(ValTypeof(1))
1

julia> ValTypeof(1) <: ValType{Int}
true
```
"""
struct ValType{T, Value}
  ValType() = error("ValType is not meant for creating any instance. It is only used as a Type. Use `ValTypeof(:yourvalue)` for a convenient construction of a ValType.")
end
ValType(value) = error("ValType is not meant for creating any instance. You probably want to use `Valtypeof($value)` instead.")

"important: the ValType construct DOES NOT construct a value, but a type."
ValTypeof(Value) = ValType{typeof(Value), Value}


Base.get(::ValType{T, Value}) where {T, Value} = Value
Base.get(::Type{ValType{T, Value}}) where {T, Value} = Value


"""
    Typeof("hi") = String
    Typeof(:hi) = ValType{String, :hi}
    Typeof(1) = ValType(Int, 1)
    Typeof(Float32) = Type{Float32}

Version of Core.Typeof with support for ValType.

# Example
general example how to dispatch on Typeof values
```julia
julia> a = :hi
:hi

julia> Typeof(a) <: Union{typeof(a), ValType{typeof(a)}}
true
```
"""
Typeof(a) = istypevalue(a) ? ValTypeof(a) : Core.Typeof(a)
Typeof(a::Function) = Core.Typeof(a)  # functions are isbits surprisingly. Still it seems slightly more convenient to work on type level, as we can use Union then


ensure_typevalue_or_type(type::Type) = type
function ensure_typevalue_or_type(other)
  istypevalue(other) || error("need either isbits, Symbol or plain Type as argument, bot got `$other`. See `istypevalue` for details.")
  ValTypeof(other)
end


# cache results by @generated - it is a type-stable method, hence safe
@generated signature_without_typevalues(::Type{T}) where T<:Tuple = _without_typevalue(T)
_without_typevalue(typevalue::Type{<:ValType{T}}) where T = T
_without_typevalue(type::Type) = type
_without_typevalue(::Type{T}) where T<:Tuple = Tuple_value_to_type(map(_without_typevalue, Tuple_type_to_value(T)))
_without_typevalue(::Type{NT}) where NT<:NamedTuple = NamedTuple_value_to_type(map(_without_typevalue, NamedTuple_type_to_value(NT)))

"""
like `Base.promote_type`, but works seamlessly with ValType too
"""
promote_type_or_val(::Type{<:ValType{T}}) where T = T
promote_type_or_val(type::Type) = type

promote_type_or_val(::Type{<:ValType{A}}, B::Type) where A = promote_type(A, B)
promote_type_or_val(A::Type, ::Type{<:ValType{B}}) where B = promote_type(A, B)

promote_type_or_val(::Type{<:ValType{A}}, ::Type{<:ValType{B}}) where {A, B} = promote_type(A, B)
promote_type_or_val(A::Type, B::Type) = promote_type(A, B)

end