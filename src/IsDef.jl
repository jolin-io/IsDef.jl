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
export isdef, Out, NotApplicable, @reload
import InteractiveUtils


# Core Interface
# ==============


"""
  checks whether the function is defined for the actual types or not

This works in compile time and hence can be used to optimize code.

IMPORTANT: Overload ``IsDef._return_type`` if you experience unexpected behaviour for your types
For instance to say that some call like ``myfunc(::Int, ::String)`` is not defined define the following
```julia
function IsDef._return_type(::Typeof(myfunc), ::Type{Tuple{TypeArg1, TypeArg2}})
  Union{}  # return empty Union to indicate something is not defined
end
```
"""
function isdef(f, types::Vararg{<:Type})
  signature_type = Tuple{types...}
  _return_type(f, signature_type) !== Union{}
end
isdef(f, args...) = isdef(f, typeof.(args)...)


struct NotApplicable end

"""
  returns outputtype of function application

Returns ``Traits.NotApplicable`` if compiler notices that no Method can be found

CAUTION: If ``Out(...) == Any``, still a MethodError might happen at runtime. This is due to incomplete type inference.

SOLUTION: Overload ``IsDef._return_type`` if you experience unexpected behaviour for your types
For instance to say that some call like ``myfunc(::Int, ::String)`` is not defined define the following
```julia
function IsDef._return_type(::typeof(myfunc), ::Type{Tuple{TypeArg1, TypeArg2}})
  Union{}  # return empty Union to indicate something is not defined
end
```
"""
function Out(f, types::Vararg{<:Type})
  signature_type = Tuple{types...}
  _Out(_return_type(f, signature_type))
end
Out(f, args...) = Out(f, typeof.(args)...)
_Out(outtype::Type{Union{}}) = NotApplicable
_Out(outtype) = outtype


"""
  wrapper arround Julia's type inference

This should be overloaded if you want to fix certain wrong typeinferences for
  your custom types.
Returning ``Union{}`` is interpreted as ``MethodError``.

It is used internally by both ``isdef`` and ``Out``.
"""
function _return_type(f, types::Type{<:Tuple})
  # TODO we cannot easily make _return_type be @generated because of the dependency to `f`,
  # hence the ``newtype_signature`` and ``newtype_inverse`` are @generated for better type-inference support
  # however for faster user-experience it probably would be good if _return_type as well as isdef and Out can be @generated
  newtype_inverse(Core.Compiler.return_type(f, newtype_signature(types)))
end



# Helpers
# =======


newtype_inverse(T::Type{NewType}) = Any
newtype_inverse(T) = T

# without ``@generated`` the function does not type-infere in detail
@generated function newtype_signature(::Type{T}) where T <: Tuple
  # IMPORTANT!!! also adapt @relaod below
  Tuple{Type2Union.(T.parameters)...}
end

macro reload()
  esc(quote
    @generated function IsDef.newtype_signature(::Type{T}) where T <: Tuple
      Tuple{IsDef.Utils.Type2Union.(T.parameters)...}
    end
  end)
end

Type2Union(T) = Union{leaftypes(T)...}
Type2Union(T::Type{Any}) = NewType  # only Any is dealed with as open world

struct NewType end

function leaftypes(T)
  # InteractiveUtils.subtypes is super mighty in that it can already deal with UnionAll types
  subtypes = InteractiveUtils.subtypes(T)
  if isempty(subtypes)
    [T]
  else
    vcat((leaftypes(S) for S in subtypes)...)
  end
end

end # module
