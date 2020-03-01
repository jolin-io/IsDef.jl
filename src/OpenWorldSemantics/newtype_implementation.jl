"""
  wrapper arround Julia's type inference

This should be overloaded if you want to fix certain wrong typeinferences for
  your custom types.
Returning ``Union{}`` is interpreted as ``MethodError``.

It is used internally by both ``isdef`` and ``Out``.
"""
function _return_type_openworldassumption(f, types::Type{<:Tuple})
  # Note, we cannot easily make _return_type be @generated because of the dependency to `f`,
  # hence the ``newtype_signature`` and ``newtype_inverse`` are @generated for better type-inference support
  newtype_inverse(Core.Compiler.return_type(f, newtype_signature(types)))
end


@generated function newtype_inverse(::Type{T}) where T
  # IMPORTANT!!! also adapt @realod_newtype below
  inewtype(T)
end

# without ``@generated`` the function does not type-infere in detail
@generated function newtype_signature(::Type{T}) where T <: Tuple
  # IMPORTANT!!! also adapt @realod_newtype below
  Tuple{newtype.(T.parameters)...}
end

"""
    @reload_newtype

reset respective generated functions so that new definitions for newtype will actually take effect
"""
macro reload_newtype()
  # TODO we have problems with new newtype definitions being seen by newtype_signature
  # as a workaround we offer the possibility to redefine all respective generated functions
  esc(quote
    @generated function IsDef.newtype_signature(::Type{T}) where T <: Tuple
      Tuple{IsDef.newtype.(T.parameters)...}
    end
    @generated function IsDef.newtype_inverse(::Type{T}) where T
      IsDef.inewtype(T)
    end
  end)
end


# newtype implementation
# ======================

# newtype definition is split conveniently into the case where the type is an abstracttype and the case where it is not
# for all non-abstract types we can directly build a suitable newtype (or just take the original one)
# only abstract types need extra handling for unknown types, however as abstracttype dispatch interferes with respective
# non-abstract subtypes, we split the two cases into two functions

function newtype(any)
  if isabstracttype(any)
    newtype_abstract(any)
  else
    newtype_leaves(any)
  end
end

function newtype_leaves(T::Union)
  # union types are mapped to union of respective newtypes
  Union{newtype(T.a), newtype(T.b)}
end
function newtype_leaves(T::UnionAll)
  unionall = Base.unwrap_unionall(T).name.wrapper
  apply_typedetails_and_recurse_newtype(unionall, T)
end
newtype_leaves(::Type{Union{}}) = Union{}
# concrete types, numbers, symbols and other things in typeparameters are leaves which we can passthrough
newtype_leaves(any) = any


function newtype_abstract(T::Union)
  # union types are mapped to union of respective newtypes
  Union{newtype(T.a), newtype(T.b)}
end
# if someone dispatches on Type{SomeType}, while this is an abstractype it is already singleton like dispatch
# and we don't need to construct new types
function newtype_abstract(T::Type{Type{S}}) where S
  T
end
macro error_missing_definition()
  quote
    error("""
      Cannot find a newtype for abstract type $T.

      Please run either
      - ``Traits.@create_newtype $T``.
      - or ``Traits.@create_newtype AbstractType1 AbstractType2 ...`` if you have several new abstract types.
      - or define `IsDef.newtype_abstract(::Type{$T})` yourself, e.g. in case of a sealed abstract type you may want to
        define `IsDef.newtype_abstract(::Type{$T}) = IsDef.newtype(Union{ConcreteType1, ConcreteType2, ...})`
    """)
  end
end
function newtype_abstract(T::Type)
  @error_missing_definition
end
function newtype_abstract(T::UnionAll)
  @error_missing_definition
end
function newtype_abstract(value)
  error("this should never be reached")
end



function inewtype(T::Union)
  # union types are mapped to union of respective newtypes
  Union{inewtype(T.a), inewtype(T.b)}  # recurse to private version, as these may contain unionall
end
function inewtype(T::UnionAll)
  unionall = Base.unwrap_unionall(T).name.wrapper
  apply_typedetails_and_recurse_inewtype(unionall, T)
end
inewtype(::Type{Union{}}) = Union{}
# all newtypes are concrete, i.e. the standard inverse has to always work
inewtype(any) = any


# macro to create a proper newtype
# ================================

"""
    @create_newtype MyNewAbstractType

Use thise macro to create a newtype for proper type-inference for your custom abstract type.

Please run either
- ``IsDef.@create_newtype MyNewAbstractType``.
- or ``IsDef.@create_newtype AbstractType1 AbstractType2 ...`` if you have several new abstract types.
- or define `IsDef.newtype_abstract(::Type{MyNewAbstractType})` yourself, e.g. in case of a sealed abstract type you may want to
  define `IsDef.newtype_abstract(::Type{MyNewAbstractType}) = IsDef.newtype(Union{ConcreteType1, ConcreteType2, ...})`

Important!

    After ``@create_newtype`` you have to call
    ```
    @reload_newtype
    ```
    For all necessary generated functions to be redefined (apparently this is needed, otherwise the new definitions may
    be lost in the first place)
"""
macro create_newtype(typename)
  if typename isa Expr && typename.head == :block
    esc(create_newtype(__module__, typename.args))
  else
    esc(create_newtype(__module__, typename))
  end
end
macro create_newtype(typenames...)
  esc(create_newtype(__module__, typenames))
end

function apply_typedetails_and_recurse_newtype(target, original)
  apply_typedetails_and_recurse(target, original, newtype)
end

function apply_typedetails_and_recurse_inewtype(target, original)
  apply_typedetails_and_recurse(target, original, inewtype)
end

function create_newtype(__module__, typename)
  type = Base.eval(__module__, typename)
  @assert isabstracttype(type) "``@create_newtype MyAbstractType`` only makes sense on abstracttypes, no need to call it elsewhere."
  base, typevars = split_unionall(type)
  newtype_name = Symbol("'", "__newtype__.", type, "'")

  isdefined(__module__, newtype_name) && return nothing

  if isempty(typevars)
    quote
      struct $newtype_name <: $type end
      function IsDef.newtype_abstract(::Type{$type})
        $newtype_name
      end
      function IsDef.inewtype(::Type{$newtype_name})
        $type
      end
    end
  else
    typevars_symbols = Symbol.(typevars)
    quote
      struct $newtype_name{$(typevars_symbols...)} <: $type{$(typevars_symbols...)} end
      # map concrete types, unionall type, partly union-all types
      function IsDef.newtype_abstract(T::Type{<:$type})
        !isabstracttype(T) ? T : IsDef.apply_typedetails_and_recurse_newtype($newtype_name, T)
      end
      function IsDef.inewtype(T::Type{<:$newtype_name})
        !isabstracttype(T) ? T : IsDef.apply_typedetails_and_recurse_inewtype($type, T)
      end
    end
  end
end

function create_newtype(__module__, typenames::Union{Vector, Tuple})
  expr = Expr(:block)
  for typename in typenames
    push!(expr.args, create_newtype(__module__, typename))
  end
  expr
end
