
Out(::Type{<:Tuple{typeof(*), Vararg{T}}}) where T = T
Out(::Type{<:Tuple{typeof(+), Vararg{T}}}) where T = T

function Out(sigtype::Type{Tuple{typeof(promote_type), T1, T2}}) where {T1, T2}
  hassignature(sigtype) || return NotApplicable
  @assert length(T1.parameters) == 1 "expecting `Type{T}`-like parameters, got `$T1`"
  @assert length(T2.parameters) == 1 "expecting `Type{T}`-like parameters, got `$T2`"
  Core.Typeof(promote_type(T1.parameters[1], T2.parameters[1]))
end

function Out(sigtype::Type{Tuple{typeof(convert), T1, T2}}) where {T1, T2}
  hassignature(sigtype) || return NotApplicable  # IMPORTANT: we are assuming that if there is a method, it also works
  @assert length(T1.parameters) == 1 "expecting `Type{T}`-like parameters, got `$T1`"
  T1.parameters[1]
end

tail_args(first, rest...) = rest

function Out(sigtype::Type{<:Tuple{typeof(tuple), Vararg}})
  Tuple{tail_args(sigtype.parameters...)...}
end

function Out(sigtype::Type{Tuple{typeof(Core._apply_iterate), typeof(iterate), Func, TupleArgs}}) where {Func, TupleArgs}
  # `func(mytuple...)` is translated to `Core._apply_iterate(iterate, func, mytuple)`
  # similarly we translate the typeinference
  Out(Tuple{Func, TupleArgs.parameters...})
end

function Out(sigtype::Type{Tuple{typeof(typeof), T}}) where T
  # As Out works on type-level, we can just return the type-level as the result of typeof
  Type{T}
end

# methods which need a value-level implementation
# ===============================================

# getfield
# --------
"""
```julia
julia> IsDef._Out_TypeLevel(getfield, IsDef.TypeLevel(Tuple{Int, String}), 1)
IsDef.TypeLevel{Type{Int64}}(Int64)

julia> IsDef._Out_TypeLevel(getfield, IsDef.TypeLevel.(Tuple{Int, String}), 1)
IsDef.TypeLevel{Type{Int64}}(Int64)

julia> IsDef._Out_TypeLevel(getfield, IsDef.TypeLevel.((Int, String)), 1)
IsDef.TypeLevel{Type{Int64}}(Int64)
```
"""
@inline function _Out_TypeLevel(::typeof(getfield), T, field)
  mark_as_typelevel(getfield(T, field))
end
@inline function _Out_TypeLevel(::typeof(getfield), T::TypeLevel, field)
  mark_as_typelevel(Core.Compiler.return_type(typedgetfield, Tuple{T.value, Val{field}}))
end
@inline function _Out_TypeLevel(::typeof(getfield), T::TypeLevel, field::TypeLevel)
  # Caution this returns a Union
  mark_as_typelevel(Core.Compiler.return_type(typedgetfield, Tuple{T.value, Val}))
end
# while this does not work with anonymous functions, type-inference indeed works with this little helper
typedgetfield(x, ::Val{field}) where {field} = getfield(x, field)



# Core.isa
# --------

function _Out_TypeLevel(::typeof(isa), instance, type)
  
end


# Core.apply_type
# ---------------

# needs value-level because there may be byte-values in the type signature (like Array{Int, 2})

function _Out_TypeLevel(::typeof(Core.apply_type), T, typeargs...)
  
  function convert_to_type(other)
    if other isa Type || isbits(other)
      other
    else
      error("tryed to create new type with Typevariable which is neither a Type nor bits. Gor `$other`.")
    end
  end
  function convert_to_type(T::TypeLevel)
    if isdefined(T.value, :parameters) && length(T.value.parameters) == 1
      # assuming TypeLevel T to be a wrapper around Type{YourType}
      T.value.parameters[1]
    else
      # in other cases, we don't know what todo and fallback to where 
      TypeVar(gensym(:T))
    end
  end

  T′ = convert_to_type(T)
  if T isa TypeLevel && T′ isa TypeVar
    error("assuming TypeLevel T to be a wrapper around Type{YourType}, but got `$(T.value)`")
  end
  typeargs′ = convert_to_type.(typeargs)
  wheres = filter(x -> x isa TypeVar, typeargs′)
  @show T′ typeargs′ wheres
  type = if isempty(wheres)
    T′{typeargs′...}
  else
    foldl(wheres, init=T′{typeargs′...}) do type, typevar
      UnionAll(typevar, type)
    end
  end
  @show type
  mark_as_typelevel(Type{type})
end