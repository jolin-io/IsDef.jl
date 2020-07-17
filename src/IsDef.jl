module IsDef
export isdef, Out, NotApplicable, ∨, apply

using Compat
import InteractiveUtils


"""
just applies a given function to arguments and keyword arguments

This little helper is crucial if you want to typeinfer
when only knowing the function type instead of the function instance.
"""
apply(f, args...; kwargs...) = f(args...; kwargs...)


# alias for promote_type to deal with types more compact
# we choose the symbol for join `∨` because promote_type is kind of a maximum (in type-hierarchy, with Any being the top)
# see https://en.wikipedia.org/wiki/Join_and_meet
@doc raw"""
    TypeA ∨ TypeB = promote_type(TypeA, TypeB)
    ∨(values...) = ∨(typeof.(values)...)

`∨` (latex `\vee`) is alias for `promote_type`.

When called on values, the values will be cast to types via use of `typeof` for convenience.
"""
function ∨ end
T1::Type ∨ T2::Type = promote_type(T1, T2)
∨(Ts::Type...) = promote_type(Ts...)
val1 ∨ val2 = typeof(val1) ∨ typeof(val2)
∨(values...) = ∨(typeof.(values)...)



# Core Interface
# ==============

struct NotApplicable end
# this will be overwritten below for certain types like Any
isapplicable(type) = !(NotApplicable <: type)
# despite NotApplicable <: Any, we interprete Any always as applicable
isapplicable(::Type{Any}) = true


"""
    isdef(func, ArgType1, ArgType2, ...)::Bool
    isdef(f, args...) = isdef(f, typeof.(args)...)

Checks whether the function is defined for the actual types or not.
This works in compile time and hence can be used to optimize code.

When called on values, the values will be cast to types via use of `typeof` for convenience.

!!! warning "CAUTION"
    If `isdef(...) == true`, still a MethodError might happen at runtime. This is due to incomplete type inference.

    SOLUTION: Overload `IsDef.return_type` if you experience unexpected behaviour for your types

    For instance to say that some call like `myfunc(::Int, ::String)` is not defined, then define the following
    ```julia
    function IsDef.return_type(::Type{Tuple{typeof(myfunc), TypeArg1, TypeArg2}})
      Union{}  # return empty Union to indicate something is not defined
    end
    ```

Semantics
---------
The implementation follows a closed-world semantics in that `isdef` will return true
for a given abstract type, if the function is defined for all currently defined leaf types of that
abstract type.

The only exceptions currently are `Any`, `Function`,  and `Exception`, for which `isdef` returns `true`.
"""
function isdef(f, types::Vararg{<:Type})
  signature_type = Tuple{typeof(f), types...}
  isapplicable(return_type(signature_type))
end
# we need to handle Type constructors specifically, as the typeof function forgets
function isdef(f::Type{T}, types::Vararg{<:Type}) where T
  signature_type = Tuple{Type{T}, types...}
  isapplicable(return_type(signature_type))
end
function isdef(::typeof(apply), types::Vararg{<:Type})
  signature_type = Tuple{types...}
  isapplicable(return_type(signature_type))
end
isdef(f, args...) = isdef(f, typeof.(args)...)


"""
    Out(func, ArgType1, ArgType2, ...)::ReturnType
    Out(f, args...) = isdef(f, typeof.(args)...)

Returns outputtype of function application. Returns `Traits.NotApplicable` if compiler notices that no Method can be
found.

When called on values, the values will be cast to types via use of `typeof` for convenience.

!!! warning "CAUTION"
    If `Out(...) == Any`, still a MethodError might happen at runtime. This is due to incomplete type inference.

    SOLUTION: Overload `IsDef.return_type` if you experience unexpected behaviour for your types

    For instance to say that some call like `myfunc(::Int, ::String)` is not defined, then define the following
    ```julia
    function IsDef.return_type(::Type{Tuple{typeof(myfunc), TypeArg1, TypeArg2}})
      Union{}  # return empty Union to indicate something is not defined
    end
    ```

UnionAll types and abstract types are concretified to the Union of their existing subtypes, in order
to improve type-inference. The only exceptions so far are `Any`, `Function` and `Exception`, as they have way too
many subtypes to be of practical use.
"""
function Out(f, types::Vararg{<:Type})
  signature_type = Tuple{typeof(f), types...}
  return_type(signature_type)
end
# we need to handle Type constructors specifically, as the typeof function forgets
function Out(f::Type{T}, types::Vararg{<:Type}) where T
  signature_type = Tuple{Type{T}, types...}
  return_type(signature_type)
end
function Out(::typeof(apply), types::Vararg{<:Type})
  signature_type = Tuple{types...}
  return_type(signature_type)
end
Out(f, args...) = Out(f, typeof.(args)...)


# Core Background
# ===============

"""
  wrapper arround Julia's type inference

This should be overloaded if you want to fix certain wrong typeinferences for your custom types.
It is used internally by both `isdef` and `Out`.

Returning `Union{}` is interpreted as `MethodError`.
"""
return_type(::Type{Ts}) where {Ts <: Tuple} = core_return_type(abstracttype_to_list_of_concretetypes(Ts))
# TODO we tried making this function the @generated one (and not abstracttype_to_list_of_concretetypes),
# however we immediately get NotApplicable for the very first test
# @test Out(Base.map, typeof(x->2x), Vector{Int}) == Vector{Int}

"""
`core_return_type` can either except
- a Union (which may resolved type-wise)
- or a Tuple-Type (which will be the final call signature)

When something fails, it won't return `Union{}` (or `Array{Union{}, 1}`), but `NotApplicable`.
This also simplifies the Union part, as Union{} would simply disappear as soon as put into Union{args...}.
A `NotApplicable` will stay.
"""
function core_return_type(T::Union)
  Union{core_return_type(T.a), core_return_type(T.b)}
end

# NOTE we cannot dispatch on Type{<:Tuple}, as this is more specific than Union, and we only have Union of Tuple,
# hence the Union clause would never be called
# TODO we need to add another function layer as otherwise typeinference again fails...
# The test which failed in all cases is `@test Out(Out, typeof(Base.map), typeof(x->2x), Vector{Int}) == Type{Vector{Int}}`
# TODO we need to forward Tuple{T} and cannot use Tuple{T} in the same function as Core.Compiler.return_type
# Otherwise the meta test fails
# Expression: Out(Out, typeof(Base.map), typeof((x->2x)), Vector{Int}) == Type{Vector{Int}}
# Evaluated: DataType == Type{Array{Int64,1}}
function core_return_type(T::Type)
  _core_return_type(Tuple{T})
end

# TODO for some unclarified and not yet minimized reason, the following function signature does not infere well
# `_core_return_type(::Type{T}) where T` and hence neither `_core_return_type(::Type{T}) where T <: Tuple`
# apparently the Type{T} looses some information given by the value, by restricting to type-level only.
# TODO restricting type to `Type{<:Tuple}` somehow looses typeinformation similarly
# The test which failed in all cases is `@test Out(Out, typeof(Base.map), typeof(x->2x), Vector{Int}) == Type{Vector{Int}}`
function _core_return_type(T::Type)
  # TODO we need to workaround some surprising type inference issues. See https://github.com/JuliaLang/julia/issues/36626
  apply_internal(full_call) = full_call[1](Base.tail(full_call)...)
  emptyunion_to_notapplicable(Core.Compiler.return_type(apply_internal, T))
end


# check whether a type means that there was an error
# ==================================================

emptyunion_to_notapplicable(::Type{Union{}}) = NotApplicable
@generated function emptyunion_to_notapplicable(::Type{T}) where T
  _emptyunion_to_notapplicable(T)
end
_emptyunion_to_notapplicable(T) = isbottom(T) ? NotApplicable : T


isbottom(T::Type{Union{}}) = true
isbottom(value) = false
@generated function isbottom(::Type{T}) where T
  _isbottom(T)
end

function _isbottom(T::DataType)
  any(T.parameters) do p
    isbottom(p)
  end
end
_isbottom(T::UnionAll) = _isbottom(Base.unwrap_unionall(T))
_isbottom(T::Union) = _isbottom(T.a) || _isbottom(T.b)

# Helpers
# =======

# Some Types need to stay abstract because they have way too many subtypes
const DontTouchTypes = (Any, Function, Exception)

for type in DontTouchTypes
  eval(quote
    Type2Union(T::Type{$type}) = $type
    leaftypes(::Type{$type}) = [$type]
    allsubtypes(::Type{$type}) = [$type]
  end)
end


# Tuples{Union{...}} -> Union{Tuple{...}}
# however we represent it as a tuple instead of a Union because this gives us better type inference
# (we just tried it out to get to know)


# we only generate the tuple version, as this is the entry point we need
# all other Type2Union are just for recursion and nested types
# but also bringing them @generated does not add much, hence we follow recommendations and leave them as functions
"""
takes a type like
```
Tuple{Int, AbstractFloat}
```
and returns Union
```
Union{
  Tuple{Int, BigFloat},
  Tuple{Int, Float16},
  Tuple{Int, Float32},
  Tuple{Int, Float64},
}
```
"""
@generated function abstracttype_to_list_of_concretetypes(::Type{T}) where T <: Tuple
  _abstracttype_to_list_of_concretetypes(T)
end
_abstracttype_to_list_of_concretetypes(T) = Type2Union(T)


function Type2Union(::Type{T}) where T <: Tuple
  alltypes = Base.uniontypes.(Type2Union.(T.parameters))
  combinations = collect(Iterators.product(alltypes...))
  Union{map(c -> Tuple{c...}, combinations)...}
end

# Other Types can be mapped to the Union of their concrete subtypes
Type2Union(T::Union) = Union{Type2Union(T.a), Type2Union(T.b)}
function Type2Union(::Type{T}) where T
  leaftypes_with_abstract_typevariables = leaftypes(T)
  # apparently the current typeinference already works super well if instead of a typevariable-upperbound
  # `Vector{<: Number}`
  # we just use the union
  # `Union{Vector{subtype1}, Vector{subtype2}, ... for subtype in allsubtypes(Number)}`
  plain_leaftypes = unionall_to_union.(leaftypes_with_abstract_typevariables)
  # TODO apparently julia's type-inference stops after a Union of three types with the approximate result `Any`
  # that is of course not optimal...
  # I guess best is to wait for a better solution and just overload the return_type function with your specific needs
  Union{plain_leaftypes...}
end

# TODO is it possible to combine Type2Union and unionall_to_union together? looks like it could be possible
unionall_to_union(type) = unionall_to_union(type, IdDict())
unionall_to_union(any, dict) = any  # bytetypes in parameters
function unionall_to_union(typevar::TypeVar, dict::IdDict)
  # as soon as we visit a typevariable WITHIN the typeparameters (not within the wheres)
  # this should already be mapped in the dict
  dict[typevar]
end
function unionall_to_union(type::Union, dict::IdDict)
  Union{unionall_to_union(type.a, dict), unionall_to_union(type.b, dict)}
end
function unionall_to_union(type::DataType, dict::IdDict)
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
    if subtype in DontTouchTypes
      # in case of Any, we do not do anything practically
      newdict[type.var] = type.var
      newbody = unionall_to_union(type.body, newdict)
      _UnionAll(type.var, newbody) # rewrap into UnionAll
    else
      newdict[type.var] = subtype
      unionall_to_union(type.body, newdict)
      # here the type.var was replaced by an actual type, hence the result is already a valid type and does not need to be rewrapped into UnionALl
    end
  end
  Union{recurse_with_new_dict.(subtypes)...}
end

# UnionAll of Unions have buggy behaviour currently
# see https://github.com/JuliaLang/julia/issues/35910 for updates
_UnionAll(typevar, type::Union) = Union{_UnionAll(typevar, type.a), _UnionAll(typevar, type.b)}
_UnionAll(typevar, other) = UnionAll(typevar, other)



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
    @generated function abstracttype_to_list_of_concretetypes(::Type{T}) where T <: Tuple
      _abstracttype_to_list_of_concretetypes(T)
    end
    @generated function IsDef.emptyunion_to_notapplicable(::Type{T}) where T
      _emptyunion_to_notapplicable(T)
    end
    @generated function IsDef.isbottom(::Type{T}) where T
      _isbottom(T)
    end
    nothing
  end
end

end # module
