module Utils
using Reexport

include("TricksAdapted.jl")
@reexport using .TricksAdapted

include("TypeUtils.jl")
@reexport using .TypeUtils

include("TypeValues.jl")
@reexport using .TypeValues

include("DynamoInternals.jl")
@reexport using .DynamoInternals

include("IRToolsUtils.jl")
@reexport using .IRToolsUtils

export _Core_return_type, Core_return_type
using IsDef: NotApplicable, IsDef

is_union_or_concretetype_or_union_of_concretetypes(type) = isconcretetype(type)
is_union_or_concretetype_or_union_of_concretetypes(::typeof(Union{})) = true
function is_union_or_concretetype_or_union_of_concretetypes(type::Union)
    isconcretetype(type.a) && is_union_or_concretetype_or_union_of_concretetypes(type.b)
end


# we need an extra helper struct, because ValType cannot have instances
# but Core.Compiler.return_type only works well if an instance is constructed
struct _ValType{Typ, Value} end
_ValType_totype(::Type{_ValType{Typ, Value}}) where {Typ, Value} = ValType{Typ, Value}
_ValType_totype(::Type{<:_ValType{Typ}}) where Typ = Typ

function _Union_with_ValType_to_Union_without(u::Union)
    Union{_Union_with_ValType_to_Union_without(u.a), _Union_with_ValType_to_Union_without(u.b)}
end
_Union_with_ValType_to_Union_without(::Type{<:_ValType{T}}) where T = T
_Union_with_ValType_to_Union_without(::Type{T}) where T = T

function _Core_return_type(f::F, t::T) where {F, T}

    function typevalue_wrapper(args...; kwargs...)
        result = f(args...; kwargs...)
        if istypevalue(result)
            return _ValType{Core.Typeof(result), result}()
        else
            result
        end
    end

    _ret = Core.Compiler.return_type(typevalue_wrapper, t)

    # extract ValType information
    ret = if _ret isa typeof(Union{})
        NotApplicable
    elseif _ret <: _ValType
        _ValType_totype(_ret)
    elseif _ret isa Union
        _Union_with_ValType_to_Union_without(_ret)
    else
        _ret
    end

    if !is_union_or_concretetype_or_union_of_concretetypes(ret)
        # we want to warn if the return type is not concrete
        # but we only want to warn for pure call to Out and not a call to isdef
        # we distinguish this via the stacktrace

        trace = stacktrace()
        called_from_within_isdef = any(trace) do stackfr
            isnothing(stackfr.linfo) && return false
            isa(stackfr.linfo, Core.MethodInstance) || return false
            spec_types = stackfr.linfo.specTypes.parameters
            !isempty(spec_types) && spec_types[1] === typeof(IsDef.isdef)
        end

        if !called_from_within_isdef
            Core.println(Core.stderr,
                """WARNING: We falled back to using Core.Compiler.return_type,
                however the inference returned a non-concrete type $(ret), which means that
                we cannot fully trust the inference result.

                Hence, you may want to check the original arguments:
                > Core.compiler.return_type($f, $t)
                """
            )
        end
    end
    return ret
end

function Core_return_type(f::F, t::T) where {F, T}
    t2 = signature_without_typevalues(t)
    return _Core_return_type(f, t2)
end

"""
    @not someboolean

a human readable alternative to `!`

Example
-------

```julia
julia> if @not isodd(2) && @not iseven(3) && true
           println("works")
       end
works

julia> @macroexpand if @not isodd(2) & @not iseven(3) & true
           println("works")
       end
:(if (!)(Main.isodd(2)) & ((!)(Main.iseven(3)) & true)
      #= REPL[93]:2 =#
      println("works")
  end)
```
"""
macro not(expr)
    if isa(expr, Expr) && expr.head == :call && expr.args[1] ∈ (:&&, :||, :&, :|)
        not2 = Expr(:call, !, expr.args[2])
        Expr(:call, expr.args[1], not2, expr.args[3:end]...)
    else
        Expr(:call, !, expr)
    end
end

end