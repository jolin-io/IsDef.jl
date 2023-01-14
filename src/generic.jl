using IRTools: IRTools, @dynamo, IR, xcall, emptyargs!, arguments, block
using MacroTools: MacroTools, isexpr
using SimpleMatch: @match

using IsDef.Utils.TricksAdapted: static_hasmethod, static_hasnomethod, covering_method_instances
using IsDef.Utils.Applicabilities: NotApplicable, isapplicable
using IsDef.Utils.TypeUtils: Tuple_type_to_value, Tuple_value_to_type, IntrinsicFunction, signature_split_first, isleaf_type
using IsDef.Utils.ValTypes: ValType, ValTypeof, signature_without_valtypes
using IsDef.Utils.IRToolsUtils: ir_detect_cycle, ir_keep_only_what_is_explicitly_used!, ir_shortcycle_if_notapplicable!, ir_lift_ifelse!, ir_typify!, ir_arguments_from_signature!

using Crayons.Box


# Out
# ===

const TOO_GENERIC_TYPES = begin
    types = (Any, Function, Type, Module)
    types_vararg = map(T -> Vararg{T}, types)
    tuple(types..., types_vararg...)
end


function Out(::Type{signature_typevalues}) where {signature_typevalues <: Tuple{Core.Builtin, Vararg}}
    # special handling of Builtin functions, as `static_hasmethod` does not work on them
    # we simply fall back to Core.Compiler.return_type directly
    Core_return_type(signature_typevalues)
end


function Out(::Type{Tuple{IntrinsicFunction{Func}, Vararg}}) where {Func}
    # special handling of IntrinsicFunctions, because
    # 1) `static_hasmethod` does not work on them
    # 2) they don't have a proper type

    ftype, argstype = signature_split_first(signature_typevalues)
    Core_return_type(Tuple{Func, argstype...})
end


function _Out_dynamo_expr(function_name; isdynamo)
    expr = :(
    function $function_name(::Type{signature_valtypes}) where {signature_valtypes<:Tuple}

        # ApplicabilityProblems
        # =====================

        # this is type-stable
        all(isapplicable, signature_valtypes.parameters) || return NotApplicable


        # "Leaf methods"
        # ==============

        # we do not recurse into these methods, but fallback to IsDef.Core_return_type
        # we also apply some heuristics to check whether a new custom inference rule
        # should be defined

        signature_novaltypes = signature_without_valtypes(signature_valtypes)

        if all(isleaf_type, fieldtypes(signature_novaltypes))
            # functions which consists purely out of Base/Core stuff are handled by falling back to Core.Compiler.return_type
            # we need to make sure, that this is not too general, hence we check whether the to be called methods are too generic
            # TODO what about containers like tuples?
            mts = covering_method_instances(signature_novaltypes)
            length(mts) > 1 && error(
                "not sure what this is about. Found several matching method instances. Maybe an ambiguity error. methods = $mts"
            )
            if !isempty(mts)
                method_instance = only(mts)
                parameters = if isa(method_instance.def.sig, UnionAll)
                    fieldtypes(method_instance.def.sig)
                else
                    method_instance.def.sig.parameters
                end
                found_generic_base_function = any(t -> t ∈ TOO_GENERIC_TYPES, parameters)
                found_generic_base_function && Core.println(Core.stderr, """
                    $(YELLOW_FG("┌ Warning:")) Recursed to a generic leaf function (everything is from Base or Core, and found at least one too generic type $TOO_GENERIC_TYPES).
                    $(YELLOW_FG("│"))
                    $(YELLOW_FG("│")) Please, overwrite `IsDef.Out` for the general case, e.g. with a fallback to Core.Compiler.return_type like so:
                    $(YELLOW_FG("│")) ```julia
                    $(YELLOW_FG("│")) IsDef.Out(::Type{T}) where T <: ($(method_instance.def.sig)) = IsDef.Core_return_type(IsDef.apply, T)
                    $(YELLOW_FG("│")) ```
                    $(YELLOW_FG("│"))
                    $(YELLOW_FG("│")) Alternatively you can overwrite the given concrete case
                    $(YELLOW_FG("│")) ```julia
                    $(YELLOW_FG("│")) IsDef.Out(::Type{$signature_valtypes}) = ...
                    $(YELLOW_FG("│")) ```
                    $(YELLOW_FG("│")) Or overwrite another function called before this one in the callstack.
                    $(YELLOW_FG("│"))
                    $(YELLOW_FG("│")) It is always recommended not to use Core_return_type if you know the return type by other means.
                    $(YELLOW_FG("└")) """)
            end

            return quote
                :all_types_are_from_BaseCore_fallback
                $static_hasmethod($signature_novaltypes) || return $NotApplicable
                $Core_return_type($signature_valtypes)
            end
        end


        # Code generation
        # ===============

        # get the code of the original method from which we want to construct a type version
        ir = IR(Tuple_type_to_value(signature_novaltypes)...)


        # Error out if no IR available
        # ----------------------------

        isnothing(ir) && return quote
            $static_hasmethod($signature_novaltypes) || return $NotApplicable
            # TODO improve error message
            error("""
                NOTAPPLICABLE: Encountered signature type with no IR (intermediate representation), please overwrite `IsDef.Out` respectively.
                signature_typevalues = $($signature_valtypes)
                signature_notypevalues = $($signature_novaltypes)
            """)
        end


        # Simplify ir
        # -----------
        # all function calls which return arguments which are not used can be safely ignored,
        # because for type inference mutation can be ignored

        ir_keep_only_what_is_explicitly_used!(ir)


        # Cyclic ir
        # ---------
        # If cyclic ir, our code generation fails, hence falling back to Core_return_type

        if ir_detect_cycle(ir)
            return quote
                :cycle_detected_in_ir
                $static_hasmethod($signature_novaltypes) || return $NotApplicable
                $Core_return_type($signature_valtypes)
            end
        end


        # Typify everything
        # -----------------
        # bits to ValTypes, calls to Out, ...
        # Note, that it is actually important to do this on IR, as we only now
        # here whether some constants are really constants and hence can be
        # wrapped into ValTypes.

        ir_typify!(ir)


        # if else
        # -------
        # Change all branch.condition such that they can work with `Bool` in addition to
        # `true` and `false`

        # TODO with new signature_typevalues (was a signature_notypevalues before),
        # we can actually already take out branches immediately, so that the code simplifies in all cases
        ir_lift_ifelse!(ir)


        # Short-cycling on ApplicabilityProblem
        # -------------------------------------
        # We need to do shortcycling because a return value may be used as a Bool.
        # Bool have special handling as they can define branching conditions.
        # Things will fail in such situations, if we do not short-cycle on a NotApplicable.

        ir_shortcycle_if_notapplicable!(ir)


        # Call this IR as Out would be called
        # -----------------------------------

        ir_arguments_from_signature!(ir, signature_valtypes, signature_novaltypes)


        # Extra handling of New
        # ---------------------
        # replace new with the first argument (i.e. the type going to be constructed)

        ir = MacroTools.prewalk(ir) do x
            isexpr(x, :new) ? x.args[1] : x
        end

        return ir
    end)

    if isdynamo
        return Expr(:macrocall, getfield(IRTools, Symbol("@dynamo")), nothing, expr)
    else
        return expr
    end
end


"""
because revise does not work well for generated functions, dynamo and eval, we provide
a manual way to revise the implementation
"""
function revise!()
    # create two versions, one dynamo, one implementation
    IsDef.eval(_Out_dynamo_expr(:Out, isdynamo=true))
    IsDef.eval(_Out_dynamo_expr(:_Out_implementation, isdynamo=false))
    nothing
end

# initial eval
revise!()