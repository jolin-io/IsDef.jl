module DynamoInternals

export TypeLevel, dynamointernals_ensure_innervalue, dynamointernals_innervalue_to_types

using IsDef.Utils.TypeValues: istypevalue, VAL, Typeof
using IsDef.Utils.TypeUtils: IntrinsicFunction


# Inner values & TypeLevel
# ------------------------

# innervalues can be either TypeLevel or plain value, but no VAL
# this is because we want to reuse as much as possible of existing code, which does not know how to with typelevel stuff, 
# hence it is fine to have typelevel extra,
# and it also does not know how to deal with VAL, but knows how to deal with plain values (like Bool true and false most importantly)

"""
    TypeLevel(type)

implementation detail, used to mark a type as already being on TypeLevel (i.e. an output from `Out`), 
some parts may be return values from `Out` calls, and others may be normal constants.
They somehow need to be distinguished 
"""
struct TypeLevel{T}
  value::T
  # we need Core.Typeof to collect all available type information
  TypeLevel(value) = new{Core.Typeof(value)}(value)
end
TypeLevel(typelevel::TypeLevel) = typelevel

function Base.show(io::IO, x::TypeLevel)
  print(io, "TypeLevel($(repr(x.value)))")
end

# we directly overload Tuple as several output arguments may be either Tuple or plain value, and in case of Tuple we want to broadcast
dynamointernals_ensure_innervalue(several::Union{Tuple, NamedTuple}) = map(dynamointernals_ensure_innervalue, several)
dynamointernals_ensure_innervalue(single::TypeLevel) = single
# typevalue (like 1, :symbol, true, ...) type information would get lost when put into TypeLevel
# hence we leave them literal, just as if someone would have written a literal value into the source code
dynamointernals_ensure_innervalue(single) = istypevalue(single) ? single : TypeLevel(single)
dynamointernals_ensure_innervalue(single::VAL{T, Value}) where {T, Value} = Value
dynamointernals_ensure_innervalue(single::Type{VAL{T, Value}}) where {T, Value} = Value


dynamointernals_innervalue_to_types(a::TypeLevel) = a.value
# TODO is this Tuple support really needed? Looks inconsistent
dynamointernals_innervalue_to_types(a::Union{Tuple, NamedTuple}) = map(dynamointernals_innervalue_to_types, a)
dynamointernals_innervalue_to_types(a) = Typeof(a)
# TODO is this special handling of intrinsic functions still needed?
dynamointernals_innervalue_to_types(a::Core.IntrinsicFunction) = IntrinsicFunction{a}

end