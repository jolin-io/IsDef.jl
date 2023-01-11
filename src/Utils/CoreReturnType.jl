module CoreReturnType

export Core_return_type

using IsDef: IsDef
using IsDef.Utils.Applicabilities: NotApplicable, UnsureWhetherApplicable
using IsDef.Utils.ValTypes: ValType, valtype_apply, isvaltypevalue, signature_without_valtypes
using IsDef.Utils.IOUtils: is_suppress_warnings, is_suppress_warnings_or_isdef
using Crayons.Box

is_union_or_concretetype_or_union_of_concretetypes(type) = isconcretetype(type)
is_union_or_concretetype_or_union_of_concretetypes(::typeof(Union{})) = true
function is_union_or_concretetype_or_union_of_concretetypes(type::Union)
    isconcretetype(type.a) && is_union_or_concretetype_or_union_of_concretetypes(type.b)
end

function is_possibly_inaccurate_typeinference(type)
    !is_union_or_concretetype_or_union_of_concretetypes(type)
end

ValType_totype(::Type{<:ValType{Typ, Value}}) where {Typ, Value} = ValType{Typ, Value}
ValType_totype(::Type{<:ValType{Typ}}) where {Typ} = Typ
ValType_totype(valtype) = valtype

function _Union_with_ValType_to_Union_without(u::Union)
    Union{_Union_with_ValType_to_Union_without(u.a), _Union_with_ValType_to_Union_without(u.b)}
end
_Union_with_ValType_to_Union_without(::Type{<:ValType{T}}) where T = T
_Union_with_ValType_to_Union_without(::Type{T}) where T = T

function _myvaltype_apply(f, args...; kwargs...)
    result = valtype_apply(f, args...; kwargs...)
    if isvaltypevalue(result)
        return ValType(Val{:only_for_internal_purposes}(), result)
    else
        result
    end
end

function Core_return_type(::Type{signature_valtypes}) where signature_valtypes <: Tuple

    _ret = Core.Compiler.return_type(_myvaltype_apply, signature_valtypes)
    ret = if _ret isa typeof(Union{})
        NotApplicable
    elseif _ret === ValType
        # for some cases like `Tuple{typeof(/), Int64, Union{Float32, Int64}}` we
        # get a Union of valtypes, like `Union{Float32, Float64}`, which unfortunately
        # get interpreted as the UnionAll ValType by our special `_myvaltype_apply` function
        # extract ValType information
        Core.Compiler.return_type(valtype_apply, signature_valtypes)
    elseif _ret <: ValType
        ValType_totype(_ret)
    elseif _ret isa Union
        _Union_with_ValType_to_Union_without(_ret)
    else
        _ret
    end

    if is_possibly_inaccurate_typeinference(ret)
        # we want to warn if the return type is not concrete
        # but we only want to warn for pure call to Out and not a call to isdef
        # we distinguish this via the stacktrace

        if !is_suppress_warnings_or_isdef()
            signature_novaltypes = signature_without_valtypes(signature_valtypes)
            Core.println(Core.stderr,
                """$(YELLOW_FG("┌ Warning:")) We falled back to using Core.Compiler.return_type,
                $(YELLOW_FG("│")) however the inference returned a non-concrete type $(ret), which means that
                $(YELLOW_FG("│")) we cannot fully trust the inference result.
                $(YELLOW_FG("│"))
                $(YELLOW_FG("│")) Hence, you may want to check the original arguments:
                $(YELLOW_FG("│")) > Core.Compiler.return_type(IsDef.valtype_apply, $signature_valtypes)
                $(YELLOW_FG("└")) > Core.Compiler.return_type(IsDef.apply, $signature_novaltypes)
                """
            )
        end
        return UnsureWhetherApplicable
    else
        return ret
    end
end

end # module