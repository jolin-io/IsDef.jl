# typeof(===)
# -----------

# TODO



# tuple
# -----

function Out(::Type{Signature}) where Signature <: Tuple{typeof(tuple), Vararg}
    _, tupletype = signature_split_first(Signature)
    tupletype
end


# typeof
# ------

function Out(::Type{Tuple{typeof(typeof), T}}) where T
    # As Out works on type-level, we can just return the type-level as the result of typeof
    Type{T}
end


# Core.typeassert
# ---------------

function Out(::Type{Tuple{typeof(typeassert), Value, ValueType}}) where {Value, ValueType}
    Value isa ValueType || NotApplicable
end



# getfield
# --------

function Out(::Type{Tuple{typeof(getfield), Typ, Field}}) where {Typ, Field}
    if Field isa Type
        Core.Compiler.return_type(getfield, Tuple{Typ, Field})
    else  # Field is a typevalue 
        # while this does not work with anonymous functions, type-inference indeed works with this little helper
        Core.Compiler.return_type(typedgetfield, Tuple{Typ, Val{Field}})
    end
end
typedgetfield(x, ::Val{field}) where {field} = getfield(x, field)



# isa
# ----

Out(::Type{Tuple{typeof(isa), Instance, Typ}}) where {Instance, Typ} = _Out_isa(Instance, Typ)

_Out_isa(::Type{Instance}, ::Type{Type{UpperBound}}) where {Instance, UpperBound} = Instance <: UpperBound
_Out_isa(::Type{Instance}, ::Type{Typ}) where {Instance, Typ} = Instance isa Typ
_Out_isa(::TypeValue, ::Type{Type{UpperBound}}) where {TypeValue, UpperBound} = TypeValue <: UpperBound
_Out_isa(::TypeValue, ::Type{Typ}) where {TypeValue, Typ} = TypeValue isa Typ


# _Out_isa(::Type{Instance}, ::Type{Type{UpperBound}}) where {UpperBound, Instance <: UpperBound} = true
# _Out_isa(::Type{Instance}, ::Type{Type{UpperBound}}) where {UpperBound, Instance} = false
# _Out_isa(::Type{Instance}, ::Type{Typ}) where {Instance, Typ} = Instance isa Typ
# _Out_isa(::TypeValue, ::Type{Type{UpperBound}}) where {UpperBound, TypeValue <: UpperBound} = true
# _Out_isa(::TypeValue, ::Type{Type{UpperBound}}) where {UpperBound, TypeValue} = false
# _Out_isa(::TypeValue, ::Type{Typ}) where {TypeValue, Typ} = TypeValue isa Typ



# Core.apply_type
# ---------------

@generated function Out(::Type{Signature}) where {Signature <: Tuple{typeof(Core.apply_type), Vararg}}
    func, typecall = signature_split_first(Signature)
    type_T, typeargs_signature = signature_split_first(typecall)
    T = unwrap_type(type_T)
    typeargs = map(unwrap_type, Tuple_type_to_value(typeargs_signature))
    wheres = filter(x -> x isa TypeVar, typeargs)
    type = if isempty(wheres)
        T{typeargs...}
    else
        foldl(wheres, init=T{typeargs...}) do type, typevar
            UnionAll(typevar, type)
        end
    end
    Type{type}  # the typeof the constructed type is wanted
end
  
unwrap_type(::Type{Type{T}}) where T = T
unwrap_type(other) = other  # TODO this silent fallback is currently used for typevalues and typevars 
  

# Core._apply_iterate
# -------------------

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
"""
@generated function Out(::Type{Signature}) where {Func, Signature <: Tuple{typeof(Core._apply_iterate), typeof(iterate), Func, Vararg}}
    # `func(mytuple...)` is translated to `Core._apply_iterate(iterate, func, mytuple)`
    # similarly we translate the typeinference
    _apply_iterate, rest1 = signature_split_first(Signature)
    _iterate, rest2 = signature_split_first(rest1)
    _func, args = signature_split_first(rest2)
    @debug "Out _apply_iterate func = $Func, args = $args"
    # some Tuples may be typevalues themselves, as Tuples of typevalues actually count as typevalues
    args′ = map(ensure_Tuple_type, Tuple_type_to_value(args))
    @debug "Out _apply_iterate func = $Func, args = $args, args′ = $args′"
    if all(arg -> isa(arg, Type{<:Tuple}), args′)
        new_signature = concat_Tuples(Tuple{Func}, args′...)
        :(IsDef.Out($new_signature))
    else
        args_while_tuple = [] 
        for arg in args′
            isa(arg, Type{<:Tuple}) || break
            push!(args_while_tuple, arg)
        end
        new_signature = concat_Tuples(Tuple{Func}, args_while_tuple..., Tuple{Vararg})
        :(IsDef.Out($new_signature))
    end
end

ensure_Tuple_type(::Type{T}) where T <: Tuple = T
ensure_Tuple_type(t::Tuple) = Tuple_value_to_type(t)

concat_Tuples(FirstTuple) = FirstTuple
concat_Tuples(FirstTuple, Tuples...) = Tuple{FirstTuple.parameters..., concat_Tuples(Tuples...).parameters...}
