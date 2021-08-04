
Out(signature::Type{<:Tuple{Union{typeof(+), typeof(-), typeof(*), typeof(/)}, T1, T2}},
    kw::Type{NamedTupleEmpty}) where {T1, T2} = promote_types_or_bits(T1, T2)
Out(signature::Type{<:Tuple{Union{typeof(+), typeof(-), typeof(*), typeof(/)}, Vararg{T}}},
    kw::Type{NamedTupleEmpty}) where T = T

function Out(signature::Type{<:Tuple{F, T1, T2}},
             kw::Type{NamedTupleEmpty}) where {F <: Union{typeof(<), typeof(>), typeof(<=), typeof(>=)}, T1, T2}
  _Out_comparison(F, wrap_bits_into_Val(T1), wrap_bits_into_Val(T2))
end
@generated function _Out_comparison(::Type{F}, ::Val{T1}, ::Val{T2}) where {F, T1, T2}
  F.instance(T1, T2) 
end
_Out_comparison(::Type, ::Type, ::Type) = Bool

function Out(signature::Type{Tuple{typeof(promote_type), Type{T1}, Type{T2}}}, kw::Type{NamedTupleEmpty}) where {T1, T2}
  hassignature(signature) || return NotApplicable  # TODO: we are assuming that if there is a method, it also works
  Core.Typeof(promote_type(T1, T2))
end

function Out(signature::Type{Tuple{typeof(convert), Type{T1}, T2}}, kw::Type{NamedTupleEmpty}) where {T1, T2}
  hassignature(signature) || return NotApplicable  # TODO: we are assuming that if there is a method, it also works
  T1
end

function Out(signature::Type{<:Tuple{typeof(tuple), Vararg}}, kw::Type{NamedTupleEmpty})
  _, tupletype = signature_split_first(signature)
  tupletype
end

# TODO somehow the call syntax is not completely clear

raw"""
Core._apply_iterate implements the splatting syntax `...`
```
julia> function f(a)
         b = map(identity, a)
         tuple(b..., 42,  b..., keyword = 3, key2 = "hi")
       end
f (generic function with 1 method)

julia> @code_ir f([1,2])
1: (%1, %2)
  %3 = Main.map(Main.identity, %2)
  %4 = (:keyword, :key2)
  %5 = Core.apply_type(Core.NamedTuple, %4)
  %6 = Core.tuple(3, "hi")
  %7 = (%5)(%6)
  %8 = Core.kwfunc(Main.tuple)
  %9 = Core.tuple(%7, Main.tuple)
  %10 = Core.tuple(42)
  %11 = Core._apply_iterate(Base.iterate, %8, %9, %3, %10, %3)
  return %11
```
""" # TODO deal with all cases
# TODO IMPORTANT deal with Core.kwfunc and revise the use of keywords
function Out(::Type{Signature}, kw::Type{NamedTupleEmpty}) where {Func, Signature <: Tuple{typeof(Core._apply_iterate), typeof(iterate), Func, Vararg}}
  # `func(mytuple...)` is translated to `Core._apply_iterate(iterate, func, mytuple)`
  # similarly we translate the typeinference
  _apply_iterate, rest1 = signature_split_first(Signature)
  _iterate, rest2 = signature_split_first(rest1)
  _func, args = signature_split_first(rest2)
  Core.println("func = $Func, args = $args")

  map(convertTuple_type_to_value(args))
  Out(Tuple{Func, TupleArgs})
end


function Out(::Type{Signature}, kw::Type{NamedTupleEmpty}) where {Func, Signature <: Tuple{typeof(Core._apply_iterate), typeof(iterate), Func, Vararg}}
  # `func(mytuple...)` is translated to `Core._apply_iterate(iterate, func, mytuple)`
  # similarly we translate the typeinference
  _apply_iterate, rest1 = signature_split_first(Signature)
  _iterate, rest2 = signature_split_first(rest1)
  _func, args = signature_split_first(rest2)
  Core.println("func = $Func, args = $args")

  map(convertTuple_type_to_value(args))
  Out(Tuple{Func, TupleArgs})
end

function Out(signature::Type{Tuple{typeof(typeof), T}}, kw::Type{NamedTupleEmpty}) where T
  # As Out works on type-level, we can just return the type-level as the result of typeof
  Type{T}
end

function Out(signature::Type{Tuple{typeof(typeassert), Value, ValueType}}, kw::Type{NamedTupleEmpty}) where {Value, ValueType}
  Value isa ValueType || NotApplicableError
end



# getfield
# --------

function Out(signature::Type{Tuple{typeof(getfield), Typ, Field}}, kw::Type{NamedTupleEmpty}) where {Typ, Field}
  if Field isa Type
    Core.Compiler.return_type(getfield, Tuple{Typ, Field})
  else  # Field is a bits  
    # while this does not work with anonymous functions, type-inference indeed works with this little helper
    Core.Compiler.return_type(typedgetfield, Tuple{Typ, Val{Field}})
  end
end
typedgetfield(x, ::Val{field}) where {field} = getfield(x, field)


# Core.isa
# --------

Out(signature::Type{Tuple{typeof(isa), Instance, Typ}}, kw::Type{NamedTupleEmpty}) where {Instance, Typ} = _Out_isa(Instance, Typ)

_Out_isa(::Type{Instance}, ::Type{Type{UpperBound}}) where {Instance, UpperBound} = Instance <: UpperBound
_Out_isa(::Type{Instance}, ::Type{Typ}) where {Instance, Typ} = Instance isa Typ
_Out_isa(::IsBits, ::Type{Type{UpperBound}}) where {IsBits, UpperBound} = IsBits <: UpperBound
_Out_isa(::IsBits, ::Type{Typ}) where {IsBits, Typ} = IsBits isa Typ


# _Out_isa(::Type{Instance}, ::Type{Type{UpperBound}}) where {UpperBound, Instance <: UpperBound} = true
# _Out_isa(::Type{Instance}, ::Type{Type{UpperBound}}) where {UpperBound, Instance} = false
# _Out_isa(::Type{Instance}, ::Type{Typ}) where {Instance, Typ} = Instance isa Typ
# _Out_isa(::IsBits, ::Type{Type{UpperBound}}) where {UpperBound, IsBits <: UpperBound} = true
# _Out_isa(::IsBits, ::Type{Type{UpperBound}}) where {UpperBound, IsBits} = false
# _Out_isa(::IsBits, ::Type{Typ}) where {IsBits, Typ} = IsBits isa Typ



# Core.apply_type
# ---------------

@generated function Out(::Type{Signature}, kw::Type{NamedTupleEmpty}) where {Signature <: Tuple{typeof(Core.apply_type), Vararg}}
  func, typecall = signature_split_first(Signature)
  # Core.println("Signature = $Signature")
  type_T, typeargs_signature = signature_split_first(typecall)
  T = unwrap_type(type_T)
  typeargs = map(unwrap_type, Tuple_type_to_value(typeargs_signature))
  wheres = filter(x -> x isa TypeVar, typeargs)
  # Core.println("T = $T, typeargs = $typeargs, wheres = $wheres")
  type = if isempty(wheres)
    T{typeargs...}
  else
    foldl(wheres, init=T{typeargs...}) do type, typevar
      UnionAll(typevar, type)
    end
  end
  # Core.println("type = $type")

  Type{type}  # the typeof the constructed type is wanted
end

unwrap_type(::Type{Type{T}}) where T = T
unwrap_type(other) = other  # TODO this silent fallback is currently used for bits values and typevars 

# old:

# ensure_type_or_bits(T::Type) = T 
# ensure_type_or_bits(T::TypeLevel) = T.value
# ensure_type_or_bits(other) = isbits(other) ? other : error("tryed to create new type with Typevariable which is neither a Type nor bits. Got `$other`.")

# needs value-level because there may be byte-values in the type signature (like Array{Int, 2})
# function _Out_TypeLevel(::typeof(Core.apply_type), T, typeargs...)
#   T′ = ensure_type_or_bits(T)
#   if T isa TypeLevel && T′ isa TypeVar
#     error("assuming TypeLevel T to be a wrapper around Type{YourType}, but got `$(T.value)`")
#   end
#   typeargs′ = ensure_type_or_bits.(typeargs)
#   wheres = filter(x -> x isa TypeVar, typeargs′)
#   @show T′ typeargs′ wheres
#   type = if isempty(wheres)
#     T′{typeargs′...}
#   else
#     foldl(wheres, init=T′{typeargs′...}) do type, typevar
#       UnionAll(typevar, type)
#     end
#   end
#   @show type
#   to_typelevel_or_bits(Type{type})
# end