module CoreReturnType

export Core_return_type

using IsDef: IsDef, NotApplicable, UnsureWhetherApplicable
using IsDef.Utils.ValTypes: ValType, valtype_apply, isvaltypevalue, signature_without_valtypes


is_union_or_concretetype_or_union_of_concretetypes(type) = isconcretetype(type)
is_union_or_concretetype_or_union_of_concretetypes(::typeof(Union{})) = true
function is_union_or_concretetype_or_union_of_concretetypes(type::Union)
    isconcretetype(type.a) && is_union_or_concretetype_or_union_of_concretetypes(type.b)
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

    # extract ValType information
    ret = if _ret isa typeof(Union{})
        NotApplicable
    elseif _ret <: ValType
        ValType_totype(_ret)
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
            signature_novaltypes = signature_without_valtypes(signature_valtypes)
            Core.println(Core.stderr,
                """WARNING: We falled back to using Core.Compiler.return_type,
                however the inference returned a non-concrete type $(ret), which means that
                we cannot fully trust the inference result.

                Hence, you may want to check the original arguments:
                > Core.Compiler.return_type(IsDef.valtype_apply, $signature_valtypes)
                > Core.Compiler.return_type(IsDef.apply, $signature_novaltypes)
                """
            )
        end
        return UnsureWhetherApplicable
    else
        return ret
    end
end

end # module