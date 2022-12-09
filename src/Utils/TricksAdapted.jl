module TricksAdapted

using Base: rewrap_unionall, unwrap_unionall, uncompressed_ast
using Base: CodeInfo

export static_hasmethod, covering_method_instances

# This is used to create the CodeInfo returned by static_hasmethod.
_hasmethod_false(@nospecialize(t)) = false
_hasmethod_true(@nospecialize(t)) = true

"""
    static_hasmethod(sig_tuple::Type{<:Tuple)

Like `hasmethod` but runs at compile-time (and does not accept a worldage argument).
"""
@generated function static_hasmethod(@nospecialize(sig::Type{Signature})) where {Signature<:Tuple}
    # The signature type:
    covering_method_insts = covering_method_instances(Signature)
    method_doesnot_exist = isempty(covering_method_insts)
    ret_func = method_doesnot_exist ? _hasmethod_false : _hasmethod_true
    ci_orig = uncompressed_ast(typeof(ret_func).name.mt.defs.func)
    ci = ccall(:jl_copy_code_info, Ref{CodeInfo}, (Any,), ci_orig)

    # Now we add the edges so if a method is defined this recompiles
    if method_doesnot_exist
        # No method so attach to method table
        f = fieldtypes(Signature)[1]
        mt = f.name.mt
        ci.edges = Core.Compiler.vect(mt, Signature)
    else  # method exists, attach edges to all instances
        ci.edges = covering_method_insts
    end
    return ci
end

# Like Core.Compiler.method_instances, but accepts f as a _type_ instead of an instance.
function covering_method_instances(@nospecialize(Signature))
    if fieldtypes(Signature)[1] <: Core.Builtin
        throw(ArgumentError("argument is not a generic function"))
    end
    lim, world = -1, typemax(UInt)
    mft = Core.Compiler._methods_by_ftype(Signature, lim, world)
    method_insts = Core.MethodInstance[_specialize_method(match) for match in mft]
    covering_method_insts = [mi for mi in method_insts if Signature <: mi.def.sig]
    return covering_method_insts
end

# MethodMatch is only defined in v1.6+, so the values returned from _methods_by_ftype need
# a bit of massaging here.
if VERSION < v"1.6"
    # _methods_by_ftype returns a triple
    _get_method((mtypes, msp, method)) = method
    # Core.Compiler.specialize_method(::MethodMatch) is only defined on v1.6+
    function _specialize_method(method_data)
        mtypes, msp, m = method_data
        instance = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any), m, mtypes, msp)
        return instance
    end
else
    # _methods_by_ftype returns a MethodMatch
    _get_method(method_match) = method_match.method
    _specialize_method = Core.Compiler.specialize_method
end

end  # module