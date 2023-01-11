module IOUtils
export suppress_warnings, is_suppress_warnings, is_suppress_warnings_or_isdef, is_called_within_method

using IsDef: IsDef

@noinline suppress_warnings(run) = run()

function is_suppress_warnings()
    is_called_within_method(suppress_warnings)
end

function is_suppress_warnings_or_isdef()
    is_called_within_method(suppress_warnings, IsDef.isdef)
end

function is_called_within_method(methods::Vararg{Any, N}) where N
    trace = stacktrace()
    called_from_within = any(trace) do stackfr
        isnothing(stackfr.linfo) && return false
        isa(stackfr.linfo, Core.MethodInstance) || return false
        spec_types = stackfr.linfo.specTypes.parameters
        !isempty(spec_types) && is_one_of(spec_types[1], methods)
    end
    return called_from_within
end

@generated function is_one_of(methodtype, ::MethodsTupleType) where MethodsTupleType
    MethodTypes = fieldtypes(MethodsTupleType)
    comparisons = []
    for type in MethodTypes
        push!(comparisons, :(methodtype === $type))
    end
    return foldr(comparisons) do comp, acc
        Expr(:||, comp, acc)
    end
end

end  # module