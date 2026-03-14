using IsDef
using IsDef: ir_keep_only_what_is_explicitly_used!, ir_lift_ifelse!, dynamointernals_innervalue_to_types, iterateblocks, ir_shortcycle_if_notapplicable!, isexpr, static_hasmethod, ValTypeof, ValType, signature_without_valtypes, Tuple_type_to_value, dynamointernals_ensure_innervalue
using IRTools: IRTools, xcall

function ifelse2(t, a, b)
    println("before")
    t = !t
    if t
        println("a = $a")
        a
    else
        println("b = $b")
        b
    end
end

signature_valtypes = Tuple{typeof(ifelse2), ValTypeof(true), Int, String}
signature_novaltypes = signature_without_valtypes(signature_typevalues)

ir = IRTools.IR(Tuple_type_to_value(signature_novaltypes)...)

ir_keep_only_what_is_explicitly_used!(ir)
ir_lift_ifelse!(ir)

function ir_args_to_tuple_type(args...)
    Tuple_value_to_type(map(dynamointernals_innervalue_to_types, args))
end

# replace functioncalls with calls to Out
# as this introduces new branches, it has to be done after lifting ifelse
for block in iterateblocks(ir)
    # we mutate the block, and here we don't want to see our mutations
    # this still works, as the iterator returns variables and statements via variables
    # which is stable against mutation of the block
    for (v, st) in collect(block)
        isexpr(st.expr, :call) || continue

        var_tuple_type = insert!(ir, v, xcall(ir_args_to_tuple_type, st.expr.args...))
        var_Out = insert!(ir, v, xcall(Out, var_tuple_type))
        ir[v] = xcall(dynamointernals_ensure_innervalue, var_Out)
        # We need to do shortcycling because a return value may be used as a Bool.
        # Bool have special handling as they can define branching conditions.
        # Things will fail in such situations, if we do not short-cycle on a NotApplicable.
        ir_shortcycle_if_notapplicable!(ir, v)
        break # this block is now done as we shortcycled it
    end

    # We need to make sure that all return_types are actually
    # reconverted to normal Type information

    branch_last = IRTools.branches(block)[end]
    IRTools.isreturn(branch_last) || continue
    var_return = IRTools.returnvalue(branch_last)
    var_return !== NotApplicable || continue
    var_return_new = push!(block, xcall(dynamointernals_innervalue_to_types, var_return))
    branch_last.args[1] = var_return_new
end


# Call this IR as Out would be called
# -----------------------------------

static_hasnomethod(::Type{T}) where {T<:Tuple} = static_hasmethod(T)

_arguments = IRTools.arguments(ir)

# get dummy first block
block = IRTools.block!(ir, 1)
# which should have one single argument (the signature_typevalues)
IRTools.argument!(block)
# we start with a check wether the original method actually existed
# this also ensures us that code get's regenerated if this changes
var_hasnomethod = IRTools.push!(block, :($static_hasnomethod($signature_novaltypes)))

ir_args = dynamointernals_ensure_innervalue(Tuple_type_to_value(signature_valtypes))
var_args = IRTools.Variable[]
for i in 1:length(_arguments)-1
    push!(var_args, IRTools.push!(block, ir_args[i]))
end
uses_args = which(signature_novaltypes).isva
final_arg = if uses_args
    ir_args[length(_arguments):end]
else
    ir_args[length(_arguments)]
end
push!(var_args, IRTools.push!(block, final_arg))

IRTools.branch!(block, IRTools.block(ir, 2), var_args..., unless=var_hasnomethod)
IRTools.return!(block, NotApplicable)


# Ready

ir



dynamointernals_ensure_innervalue

function _dynamointernals_innervalue_to_types_ANDTHEN_Out_ANDTHEN_dynamointernals_ensure_innervalue(args::Vararg{Any, N}) where N
    args′ =
    out = Out(args′)
    result = dynamointernals_ensure_innervalue(out)
    result
  end


