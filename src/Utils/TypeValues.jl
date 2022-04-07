module TypeValues

export istypevalue, istypevaluetype, VAL, signature_without_typevalues, promote_type_or_val, ensure_typevalue_or_type

using IsDef.Utils.TypeUtils: Tuple_type_to_value, Tuple_value_to_type


# typevalues & VAL
# ----------------

# The VAL is only used in signatures, and will never be seen as a concretevalue moving through the functions
# the reason is that we want to resuse as much code as possible of existing function implementations, 
# hence dealing with plain values instead of VAL is simpler.
# This is most striking in the case of Bool true false, which have special handling with branches, where a VAL will immediately fail. 

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
    VAL(value) -> VAL{typeof(value), value}

Alternative to Base.Val which makes the type of the value accessible for dispatch.

In order to retrieve the value you can simple use `Base.get` on either the instance or the type.

```julia
julia> get(VAL(1))
1
julia> get(typeof(VAL(1)))
1
```
"""
struct VAL{T, Value}
  VAL(Value) = new{typeof(Value), Value}()
end

const TypeValue = VAL

Base.get(::VAL{T, Value}) where {T, Value} = Value
Base.get(::Type{VAL{T, Value}}) where {T, Value} = Value


# TODO this looks like it needs to be merged with extract_type_or_typevalue
Typeof(a) = istypevalue(a) ? Core.Typeof(VAL(a)) : Core.Typeof(a)
Typeof(a::Function) = Core.Typeof(a)  # functions are isbits surprisingly. Still it seems slightly more convenient to work on type level, as we can use Union then


ensure_typevalue_or_type(type::Type) = type
function ensure_typevalue_or_type(other)
  istypevalue(other) || error("need either isbits, Symbol or plain Type as argument, bot got `$other`. See `istypevalue` for details.")
  Core.Typeof(VAL(other))
end



signature_without_typevalues(::Type{T}) where T<:Tuple = Tuple_value_to_type(map(_without_typevalue, Tuple_type_to_value(T)))
_without_typevalue(typevalue::Type{<:VAL{T}}) where T = T
_without_typevalue(type::Type) = type


"""
like `Base.promote_type`, but works seamlessly with Val and VAL too
"""
promote_type_or_val(::Type{<:Val{Value}}) where Value = typeof(Value)
promote_type_or_val(::Type{<:VAL{T}}) where T = T
promote_type_or_val(type::Type) = type

promote_type_or_val(::Type{<:Val{a}}, B::Type) where a = promote_type(typeof(a), B)
promote_type_or_val(A::Type, ::Type{<:Val{b}}) where b = promote_type(A, typeof(b))

promote_type_or_val(::Type{<:VAL{A}}, B::Type) where A = promote_type(A, B)
promote_type_or_val(A::Type, ::Type{<:VAL{B}}) where B = promote_type(A, B)

promote_type_or_val(::Type{<:VAL{A}}, ::Type{<:Val{b}}) where {A, b} = promote_type(A, typeof(b))
promote_type_or_val(::Type{<:Val{a}}, ::Type{<:VAL{B}}) where {a, B} = promote_type(typeof(a), B)

promote_type_or_val(::Type{<:VAL{A}}, ::Type{<:VAL{B}}) where {A, B} = promote_type(A, B)
promote_type_or_val(::Type{<:Val{a}}, ::Type{<:Val{b}}) where {a, b} = promote_type(typeof(a), typeof(b))
promote_type_or_val(A::Type, B::Type) = promote_type(A, B)

end