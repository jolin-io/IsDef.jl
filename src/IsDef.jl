"""
To more easily dispatch on the availability of functions, this package offers two essential helpers:

    `isdef(f, TypeArg1, TypeArg2, ...)` and `Out(f, TypeArg1, TypeArg2, ...)`

Currently the implementation follows a closed-world semantics in that ``isdef`` will return true
for a given abstract type, if the function is defined for all currently defined leaf types of that
abstract type.

The only exception is Any, for which `isdef` returns true only if given an abstract type, the function is defined
for a newtype of the abstract type. (E.g. ``f(a) = 1`` )
"""
module IsDef
export isdef, Out, NotApplicable, ∨, apply
import InteractiveUtils


"""
just applies a given function to arguments and keyword arguments

This little helper is crucial if you want to typeinfer
when only knowing the function type instead of the function instance.
"""
@inline apply(f, args...; kwargs...) = f(args...; kwargs...)


# alias for promote_type to deal with types more compact
# we choose the symbol for join ``∨`` because promote_type is kind of a maximum (in type-hierarchy, with Any being the top)
# see https://en.wikipedia.org/wiki/Join_and_meet
"""
`∨` (latex `\vee`) is alias for ``promote_type``

when called on values, the values will be cast to types via use of `typeof` for convenience
"""
function ∨ end
T1::Type ∨ T2::Type = promote_type(T1, T2)
∨(Ts::Type...) = promote_type(Ts...)
val1 ∨ val2 = typeof(val1) ∨ typeof(val2)
∨(values...) = ∨(typeof.(values)...)



# Core Interface
# ==============


"""
  checks whether the function is defined for the actual types or not

This works in compile time and hence can be used to optimize code.

IMPORTANT: Overload ``IsDef.return_type`` if you experience unexpected behaviour for your types
For instance to say that some call like ``myfunc(::Int, ::String)`` is not defined define the following
```julia
function IsDef.return_type(::Typeof(myfunc), ::Type{Tuple{TypeArg1, TypeArg2}})
  Union{}  # return empty Union to indicate something is not defined
end
```
"""
function isdef(f::F, types::Vararg{<:Type}) where F
  signature_type = Tuple{F, types...}
  !isbottom(return_type(signature_type))
end
isdef(f, args...) = isdef(f, typeof.(args)...)


struct NotApplicable end

"""
  returns outputtype of function application

Returns ``Traits.NotApplicable`` if compiler notices that no Method can be found

CAUTION: If ``Out(...) == Any``, still a MethodError might happen at runtime. This is due to incomplete type inference.

SOLUTION: Overload ``IsDef.return_type`` if you experience unexpected behaviour for your types
For instance to say that some call like ``myfunc(::Int, ::String)`` is not defined define the following
```julia
function IsDef.return_type(::typeof(myfunc), ::Type{Tuple{TypeArg1, TypeArg2}})
  Union{}  # return empty Union to indicate something is not defined
end
```
"""
function Out(f::F, types::Vararg{<:Type}) where F
  signature_type = Tuple{F, types...}
  outputtype = return_type(signature_type)
  isbottom(outputtype) ? NotApplicable : outputtype
end
Out(f, args...) = Out(f, typeof.(args)...)

"""
  wrapper arround Julia's type inference

This should be overloaded if you want to fix certain wrong typeinferences for
  your custom types.
Returning ``Union{}`` is interpreted as ``MethodError``.

It is used internally by both ``isdef`` and ``Out``.
"""
@generated function return_type(::Type{Ts}) where {Ts <: Tuple}
  _return_type(Type2Union(Ts))
end

function _return_type(T::Union)
  Union{_return_type(T.a), _return_type(T.b)}
end
function _return_type(T::Type)
  Core.Compiler.return_type(apply, T)
end

"""
if you want to infere the return type, but you only have the type if a function, and not its instance
you can include it into the argument type Tuple on first position and just ommit the extra function argument
"""
return_type(f::F, types::Type{<:Tuple}) where F = return_type(apply, Tuple{F, types.parameters...})


# generated functions for better typeinference
# ============================================

isbottom(T::Type{Union{}}) = true
isbottom(value) = false
@generated function isbottom(::Type{T}) where T
  _isbottom(T)
end
function _isbottom(T::Type)
  base = Base.unwrap_unionall(T)
  any(base.parameters) do p
    isbottom(p)
  end
end
function _isbottom(T::Union)
  _isbottom(T.a) || _isbottom(T.b)
end


# Helpers
# =======

split_unionall(T) = T, []
function split_unionall(T::UnionAll)
  S, vars = split_unionall(T.body)
  S, [T.var; vars]
end


# Some Types need to stay abstract because they have way too many subtypes
Type2Union(T::Type{Any}) = Any
leaftypes(::Type{Any}) = [Any]
allsubtypes(::Type{Any}) = [Any]

Type2Union(T::Type{Function}) = Function
leaftypes(::Type{Function}) = [Function]
allsubtypes(::Type{Function}) = [Function]

# Tuples{Union{...}} -> Union{Tuple{...}}
function Type2Union(::Type{T}) where T <: Tuple
  alltypes = Base.uniontypes.(Type2Union.(T.parameters))
  combinations = collect(Iterators.product(alltypes...))
  Union{map(c -> Tuple{c...}, combinations)...}
end

# Other Types can be mapped to the Union of their concrete subtypes
function Type2Union(T)
  leaftypes_with_abstract_typevariables = leaftypes(T)
  # apparently the current typeinference already works super well if instead of a typevariable-upperbound
  # ``Vector{<: Number}``
  # we just use the union
  # ``Union{Vector{subtype1}, Vector{subtype2}, ... for subtypei in allsubtypes(Number)}``
  plain_leaftypes = unionall_to_union.(leaftypes_with_abstract_typevariables)
  # TODO apparently julia's type-inference stops after a Union of three types with the approximate result ``Any``
  # that is of course not optimal...
  # I guess best is to wait for a better solution and just overload the return_type function with your specific needs
  Union{plain_leaftypes...}
end


unionall_to_union(type) = unionall_to_union(type, IdDict())
unionall_to_union(any, dict) = any  # bytetypes in parameters
function unionall_to_union(typevar::TypeVar, dict::IdDict)
  # as soon as we visit a typevariable WITHIN the typeparameters (not within the wheres)
  # this should already be mapped in the dict
  dict[typevar]
end
function unionall_to_union(type::Type, dict::IdDict)
  if isempty(type.parameters)
    type
  else
    unionall = type.name.wrapper
    newparameters = [unionall_to_union(p, dict) for p in type.parameters]
    unionall{newparameters...}
  end
end
function unionall_to_union(type::UnionAll, dict::IdDict)
  subtypes = allsubtypes(type.var.ub)
  function recurse_with_new_dict(subtype)
    newdict = copy(dict)
    if subtype === Any
      # in case of Any, we do not do anything practically
      newdict[type.var] = type.var
      newbody = unionall_to_union(type.body, newdict)
      UnionAll(type.var, type.body)  # rewrap into UnionAll
    else
      newdict[type.var] = subtype
      unionall_to_union(type.body, newdict)
    end
  end
  Union{recurse_with_new_dict.(subtypes)...}
end

function subtypes(T)
  # InteractiveUtils.subtypes is super mighty in that it can already deal with UnionAll types
  sub = InteractiveUtils.subtypes(T)
  if length(sub) == 1 && sub[1] === T
    # InteractiveUtils has a small bug that sometimes the  type is itself returned as a subtype
    # which is in theory true, however leads to infinite recursion
    []
  else
    sub
  end
end

function leaftypes(T)
  sub = subtypes(T)
  if isempty(sub)
    [T]
  else
    vcat((leaftypes(S) for S in sub)...)
  end
end

function allsubtypes(T)
  sub = subtypes(T)
  if isempty(sub)
    [T]
  else
    vcat(T, (allsubtypes(S) for S in sub)...)
  end
end


"""
for some reasons the @generated macro triggers too early and leads Union{} for some types

calling this macro will redefine the @generated functions accordingly so that new TypeDefinitions are properly included
"""
macro redefine_generated()
  quote
    @generated function IsDef.return_type(::Type{Ts}) where {Ts <: Tuple}
      _return_type(Type2Union(Ts))
    end
    @generated function IsDef.isbottom(::Type{T}) where T
      _isbottom(T)
    end
    nothing
  end
end

function __init__()
  @redefine_generated
end
end # module
