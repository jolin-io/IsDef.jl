using IRTools: IRTools, @dynamo, IR, xcall, emptyargs!, arguments, block
using MacroTools: MacroTools, isexpr
using SimpleMatch: @match

using IsDef.Utils.TricksAdapted: static_hasmethod, static_hasnomethod, covering_method_instances
using IsDef.Utils.Applicabilities: NotApplicable, isapplicable
using IsDef.Utils.TypeUtils: Tuple_type_to_value, Tuple_value_to_type, IntrinsicFunction, signature_split_first, isleaf_type
using IsDef.Utils.ValTypes: ValType, ValTypeof, signature_without_valtypes, ValTypeFunction
using IsDef.Utils.IRToolsUtils: new_out, signature_to_irargs, irargs_to_signature, type_to_irvalue, irvalue_to_type, detect_cycle, keep_only_what_is_explicitly_used!, shortcycle_if_notapplicable!, lift_ifelse!, iterateblocks

using Crayons.Box

# Out
# ===

"""
special handling of TypeLevelFunctions which already work on type-TypeLevel

these are mainly functions which are defined for internal purposes
"""
function Out(::Type{signature_typevalues}) where {signature_typevalues <: Tuple{ValTypeFunction, Vararg}}
    functype, args... = Tuple_type_to_value(signature_typevalues)
    func = @match(functype) do f
        f(::Type{ValTypeFunction{F}}) where F = F.instance
    end
    return func(args...)
end

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

function _Out_dynamo_expr(function_name=:Out; isdynamo=true)
    expr = :(
    function $function_name(::Type{signature_valtypes}) where {signature_valtypes<:Tuple}
        # type-stable fallback
        all(isapplicable, signature_valtypes.parameters) || return NotApplicable

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
            else
                # # Core.println(Core.stderr, "Warning: found empty method_instances for signature $signature_novaltypes.")
                # in the case that the method table is empty, no method is available yet, hence static_hasmethod will return false, but retrigger
                # compilation if the method is redefined. We simply fallback to Core.Compiler.return_type in this case, even though we do not know
                # whether we use it on a generic method.
                # Don't know any better behaviour.
            end

            return quote
                :all_types_are_from_BaseCore_fallback
                $static_hasmethod($signature_novaltypes) || return $NotApplicable
                $Core_return_type($signature_valtypes)
            end
        end


        # fall back to code generation if method is too generic

        # get the code of the original method from which we want to construct a type version
        ir = IR(Tuple_type_to_value(signature_novaltypes)...)

        isnothing(ir) && return quote
            $static_hasmethod($signature_novaltypes) || return $NotApplicable
            error("""
                NOTAPPLICABLE: Encountered signature type with no IR (intermediate representation), please overwrite `IsDef.Out` respectively.
                signature_typevalues = $($signature_valtypes)
                signature_notypevalues = $($signature_novaltypes)
            """)
        end

        # all function calls which return arguments which are not used can be safely ignored,
        # because for type inference mutation can be ignored
        keep_only_what_is_explicitly_used!(ir)

        if detect_cycle(ir)
            return quote
                :cycle_detected_in_ir
                $static_hasmethod($signature_novaltypes) || return $NotApplicable
                $Core_return_type($signature_valtypes)
            end
        end

        # if else
        # -------

        # TODO with new signature_typevalues (was a signature_notypevalues before),
        # we can actually already take out branches immediately, so that the code simplifies in all cases

        # Change all branch.condition such that they can work with `Bool` in addition to
        # `true` and `false`
        lift_ifelse!(ir)


        # replace functioncalls with calls to Out
        # ---------------------------------------

        # as this introduces new branches, it has to be done after lifting ifelse
        for block in iterateblocks(ir)
            # we mutate the block, and here we don't want to see our mutations
            # this still works, as the iterator returns variables and statements via variables
            # which is stable against mutation of the block
            # NOTE: length is defined for Block, but seems buggy
            # TODO file an issue on IRTools about this

            # hence we store the variables ourselves
            vars = keys(block)  # returns an Array
            for var in vars
                statement = block.ir[var]
                isexpr(statement.expr, :call) || continue

                var_tuple_type = insert!(ir, var, xcall(irargs_to_signature, statement.expr.args...))
                var_Out = insert!(ir, var, xcall(Out, var_tuple_type))
                ir[var] = xcall(type_to_irvalue, var_Out)
                # We need to do shortcycling because a return value may be used as a Bool.
                # Bool have special handling as they can define branching conditions.
                # Things will fail in such situations, if we do not short-cycle on a NotApplicable.
                shortcycle_if_notapplicable!(ir, var)
                break # this block is now done as we shortcycled it
            end

            # We need to make sure that all return_types are actually
            # reconverted to normal Type information

            branch_last = IRTools.branches(block)[end]
            IRTools.isreturn(branch_last) || continue
            var_return = IRTools.returnvalue(branch_last)
            var_return_new = push!(block, xcall(irvalue_to_type, var_return))
            branch_last.args[1] = var_return_new
        end

        # Call this IR as Out would be called
        # -----------------------------------

        nargs = length(IRTools.arguments(ir))

        # get dummy first block
        block = IRTools.block!(ir, 1)
        # which should have one single argument (the signature_typevalues)
        IRTools.argument!(block)
        # we start with a check wether the original method actually existed
        # this also ensures us that code get's regenerated if this changes
        var_hasnomethod = IRTools.push!(block, :($static_hasnomethod($signature_novaltypes)))

        ir_args = signature_to_irargs(signature_valtypes)
        var_args = IRTools.Variable[]
        for i in 1:nargs-1
            push!(var_args, IRTools.push!(block, ir_args[i]))
        end
        uses_args = which(signature_novaltypes).isva
        final_arg = if uses_args
            ir_args[nargs:end]
        else
            ir_args[nargs]
        end
        push!(var_args, IRTools.push!(block, final_arg))

        IRTools.branch!(block, IRTools.block(ir, 2), var_args..., unless=var_hasnomethod)
        IRTools.return!(block, NotApplicable)

        # Extra Handling
        # --------------

        # replace new with the first argument (i.e. the type going to be constructed)
        ir = MacroTools.prewalk(ir) do x
            isexpr(x, :new) ? Expr(:call, new_out, x.args[1]) : x
        end

        return ir
    end)

    if isdynamo
        return Expr(:macrocall, getfield(IRTools, Symbol("@dynamo")), nothing, expr)
    else
        return expr
    end
end

# create two versions, one dynamo, one implementation
IsDef.eval(_Out_dynamo_expr())
IsDef.eval(_Out_dynamo_expr(:_Out_implementation, isdynamo=false))

"""
because revise does not work well for generated functions and dynamo we provide
a manual way to revise the implementation
"""
function revise!()
    IsDef.eval(_Out_dynamo_expr())
    IsDef.eval(_Out_dynamo_expr(:_Out_implementation, isdynamo=false))
    nothing
end