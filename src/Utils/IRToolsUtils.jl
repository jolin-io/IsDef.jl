module IRToolsUtils

export new_out, irargs_to_signature, signature_to_irargs, type_to_irvalue, irvalue_to_type, detect_cycle, keep_only_what_is_explicitly_used!, shortcycle_if_notapplicable!, lift_ifelse!, iterateblocks

using IsDef.DataTypes: NotApplicable, UnsureWhetherApplicable, ApplicabilityProblem, isapplicable, TypeValueFunction
using IsDef.Utils.ValTypes: isvaltypevalue, ValType, ValTypeof, Typeof
using IsDef.Utils.TypeUtils: IntrinsicFunction, Tuple_value_to_type, Tuple_type_to_value, NamedTuple_value_to_type

using Setfield
using IRTools: IRTools, IR, @dynamo, recurse!, xcall
using MacroTools: MacroTools, isexpr

import IsDef


# Covnert to and from inner IR values
# -----------------------------------

# There multiple things to consider:
# 1) we need to track which value is a Type because it was already in the code as a Type,
# or which value is a Type because it was returned by one of our rewritten Out functions
# The difference is marked by an extra type `TypeLevel`.
#
# 2) plain values like literal `true` should be seen as literal `true` within the ir
# however we want to represent it on type level with the special `ValType` type.

"""
        TypeLevel(type)

implementation detail, used to mark a type as already being on TypeLevel (i.e. an output from `Out`),
some parts may be return values from `Out` calls, and others may be normal constants.
They somehow need to be distinguished
"""
struct TypeLevel{T}
    value::T
    # we need Core.Typeof to collect all available type information
    TypeLevel(value) = new{Core.Typeof(value)}(value)
end
TypeLevel(typelevel::TypeLevel) = typelevel

function Base.show(io::IO, x::TypeLevel)
    print(io, "TypeLevel($(repr(x.value)))")
end



# we directly overload Tuple as several output arguments may be either Tuple or plain value, and in case of Tuple we want to broadcast
# this is especially needed for splash ...
type_to_irvalue(several::Union{Tuple, NamedTuple}) = map(type_to_irvalue, several)
type_to_irvalue(single::TypeLevel) = single
# typevalue (like 1, :symbol, true, ...) type information would get lost when put into TypeLevel
# hence we leave them literal, just as if someone would have written a literal value into the source code
type_to_irvalue(single) = isvaltypevalue(single) ? single : TypeLevel(single)
type_to_irvalue(single::ValType{T, Value}) where {T, Value} = Value
type_to_irvalue(single::Type{ValType{T, Value}}) where {T, Value} = Value
type_to_irvalue(::Type{T}) where {T<:ApplicabilityProblem} = T


irvalue_to_type(a::TypeLevel) = a.value
# this is really needed, namely for splash ... arguments which internally get
# represented as tuples. If we don't rewrap TypeLevel in there, we get Type problems
irvalue_to_type(a::Tuple) = Tuple_value_to_type(map(irvalue_to_type, a))
irvalue_to_type(a::NamedTuple) = NamedTuple_value_to_type(map(irvalue_to_type, a))
irvalue_to_type(a) = Typeof(a)
# TODO is this special handling of intrinsic functions still needed?
irvalue_to_type(a::Core.IntrinsicFunction) = IntrinsicFunction{a}
irvalue_to_type(::Type{T}) where {T<:ApplicabilityProblem} = T


function irargs_to_signature(args...)
    Tuple_value_to_type(map(irvalue_to_type, args))
end

function signature_to_irargs(::Type{signature_valtypes}) where signature_valtypes<:Tuple
    type_to_irvalue(Tuple_type_to_value(signature_valtypes))
end

"""
the output type of calling the internal function new, given the target output type
"""  # if we already receive a type-level, we need to strap of one layer
new_out(::TypeLevel{Type{Type{T}}}) where T = TypeLevel(T)
new_out(::TypeLevel) = error("this should not happen")
# if we receive value-level, we can directly treat it as type-level
new_out(type) = type_to_irvalue(type)


# IRTools helpers
# ===============


# adding better show support for CompileError
function Base.show(io::IO, ::MIME"text/plain", err::IRTools.Inner.CompileError)
    Base.showerror(io, err)
end


function isargument(ir, var)
    (block, i) = get(ir.defs, var.id, (-1, -1))
    block != -1 || error("variable $var does not exist within ir = $ir")
    return i <= 0
end

blockidx(ir, var) = if isargument(ir, var)
    for block in IRTools.blocks(ir)
        bb = IRTools.BasicBlock(block)
        if var ∈ bb.args
            return (block, 0)
        end
    end
else
    return IRTools.Inner.blockidx(ir, var)
end


struct IRIterateBlocks
    ir::IRTools.IR
end
iterateblocks(ir::IRTools.IR) = IRIterateBlocks(ir)

Base.length(wrapper::IRIterateBlocks) = length(wrapper.ir.blocks)
IRTools.block(wrapper::IRIterateBlocks, i) = IRTools.block(wrapper.ir, i)

function Base.iterate(wrapper::IRIterateBlocks, state = 1)
    state <= length(wrapper) || return nothing
    IRTools.block(wrapper, state), (state+1)
end

# detect_cycle
# ------------

"""
        detect_cycle(ir)::Bool

Returns true if the blocks have a cyclic dependency, otherwise false.
"""
function detect_cycle(ir::IRTools.IR)
    # start with return blocks
    current_blocks = [IRTools.Block(ir, 1)]
    branch_trace = Set{IRTools.Block}()

    while !isempty(current_blocks)
            next_block = popfirst!(current_blocks)
            successors = IRTools.successors(next_block)
            append!(current_blocks, successors)
            if isdisjoint(branch_trace, successors)
                    union!(branch_trace, successors)
            else
                    return true
            end
    end
    return false
end


# delete
# ------

"""
deletes unused variables from the ir
"""
function keep_only_what_is_explicitly_used!(ir)
    counts = count_return_variables(ir)
    unused_vars = [var for (var, count) in counts if count == 0]
    # iteratively delete all unused variables
    while !isempty(unused_vars)
        for var in unused_vars
            delete!(ir, var)
        end
        counts = count_return_variables(ir)
        unused_vars = [var for (var, count) in counts if count == 0]
    end
    return ir
end


function count_return_variables(ir::IRTools.IR)
    counts = Dict(IRTools.Variable(i) => 0
        for (i, (id_block, id_var_withinblock)) ∈ enumerate(ir.defs)
        if id_block > 0 && id_var_withinblock > 0)
    _count_variables!(ir, counts)
    return counts
end


function count_all_variables(ir::IRTools.IR)
    counts = Dict(IRTools.Variable(i) => 0 for i ∈ eachindex(ir.defs))
    _count_variables!(ir, counts)
    return counts
end

function _count_variables!(ir::IRTools.IR, dict!)
    for block ∈ IRTools.blocks(ir)
        _count_variables!(block, dict!)
    end
end

function _count_variables!(block::IRTools.Block, dict!)
    for (var, st) ∈ block
        _count_variables!(st.expr, dict!)
    end
    for b ∈ IRTools.branches(block)
        _count_variables!(b, dict!)
    end
end

function _count_variables!(expr::Expr, dict!)
    for a ∈ expr.args
        _count_variables!(a, dict!)
    end
end

function _count_variables!(var::IRTools.Variable, dict!)
    if haskey(dict!, var)
        dict![var] += 1
    end
end

function _count_variables!(branch::IRTools.Branch, dict!)
    _count_variables!(branch.condition, dict!)
    for a ∈ branch.args
        _count_variables!(a, dict!)
    end
end

function _count_variables!(other, dict!)
end



# shortcylce
# ----------

is_applicability_problem(_) = false
is_applicability_problem(::Type{<:ApplicabilityProblem}) = true

@doc raw"""
        shortcycle_if_notapplicable_error!(ir::IRTools.IR, var::IRTools.Variable)

Rewrites the intermediate representation `ir` right after the given `var` such that the algorithm immediately returns `NotApplicable` in case that `var === NotApplicable`.

Examples
--------

```jldoctest
julia> using IRTools, IsDef

julia> foo(::Int) = NotApplicable
foo (generic function with 1 method)

julia> foo(x::String) = x
foo (generic function with 2 methods)

julia> function g(x)
                     y = "foo(x) = $(foo(x))"
                     z = "y = $y"
                     z
             end
g (generic function with 1 method)

julia> ir_g = @code_ir g(1)
1: (%1, %2)
    %3 = Main.foo(%2)
    %4 = Base.string("foo(x) = ", %3)
    %5 = Base.string("y = ", %4)
    return %5

julia> IsDef.shortcycle_if_notapplicable!(ir_g, IRTools.var(3))
1: (%1, %2)
    %3 = Main.foo(%2)
    %8 = (===)(%3, NotApplicable)
    br 2 unless %8
    return IsDef.NotApplicable
2:
    %4 = Base.string("foo(x) = ", %3)
    %5 = Base.string("y = ", %4)
    return %5

julia> IRTools.func(ir_g)(g, 3)
NotApplicable

julia> IRTools.func(ir_g)(g, "hi")
"y = foo(x) = hi"
```
"""
function shortcycle_if_notapplicable!(ir::IRTools.IR, var::IRTools.Variable)
    condition = xcall(is_applicability_problem, var)
    shortcycle_after_var_if_condition!(ir, var, condition) do shortcycle_block, _continuation_block, _blockid_mapping, _var_is_condition
        IRTools.return!(shortcycle_block, var)
    end
end

function shortcycle_after_var_if_condition!(finish_block!::Function, ir::IRTools.IR, after_var_or_endofblock, condition)
    if isa(after_var_or_endofblock, IRTools.Variable)
        var = after_var_or_endofblock
        oldblock, i_var_oldblock = blockidx(ir, var)
    elseif isa(after_var_or_endofblock, IRTools.Block)
        oldblock = after_var_or_endofblock
        i_var_oldblock = length(IRTools.BasicBlock(oldblock).stmts)
    else
        throw(ArgumentError("var_or_block is neither a IRTools.Variable nor IRTools.Block, but a $(typeof(after_var_or_endofblock))."))
    end

    newblock = IRTools.block!(ir, oldblock.id+1)  # insert right after given block
    blockid_mapping = Dict(
        i => i <= oldblock.id ? i : i+1
        for i in eachindex(IRTools.blocks(ir))
    )

    # move next statements to new block
    oldblock_n_statements = length(IRTools.BasicBlock(oldblock).stmts)
    for j in (i_var_oldblock+1):oldblock_n_statements
            push!(newblock, oldblock[j])
            oldblock[j] = nothing
    end
    # move variable ids to new block
    for (i, (i_block, i_var_block)) in enumerate(ir.defs)
            if i_block == oldblock.id && i_var_block > i_var_oldblock
                    ir.defs[i] = (newblock.id, i_var_block - i_var_oldblock)
            # we need to delete the original references which have been created for newblock
            elseif i_block == newblock.id
                    ir.defs[i] = (-1, -1)
            end
    end

    # move branches to new block
    append!(IRTools.branches(newblock), IRTools.branches(oldblock))
    empty!(IRTools.branches(oldblock))

    # add shortcycling if not-applicable-error was found
    var_is_condition = if isa(condition, IRTools.Variable)
        condition
    else
        insert!(oldblock, i_var_oldblock+1, condition)  # also works if i_var_oldblock == 0
    end
    IRTools.branch!(oldblock, newblock, unless = var_is_condition)
    finish_block!(oldblock, newblock, blockid_mapping, var_is_condition)
    return var_is_condition
end




# ifelse
# ------


function lift_ifelse!(ir::IRTools.IR)
    IRTools.explicitbranch!(ir)
    # Change all branch.condition such that they can work with `Bool` in addition to `true` and `false`
    all_ifelse = Set{IRTools.Variable}()
    for block in iterateblocks(ir)
        for branch in IRTools.branches(block)
            condition_var = branch.condition
            # skip over unconditional or return branches
            isnothing(condition_var) && continue
            # skip over already handled var
            condition_var ∈ all_ifelse && continue

            # it might be that the condition_var is just an argument and if so won't have an expression mapped to it
            if haskey(ir, condition_var)
                # skip over isbooltype conditions, they are safe as we introduce them right here
                condition_statement = ir[condition_var]
                expr = condition_statement.expr
                isexpr(expr, :call) && expr.args[1] === isbooltype && continue
            end

            # support Bool
            lift_ifelse!(ir, block, condition_var)
            push!(all_ifelse, condition_var)
        end
    end
    return ir
end

function lift_ifelse!(ir::IRTools.IR, original_block::IRTools.Block, var_ifelse::IRTools.Variable)
    condition = xcall(TypeValueFunction(isbooltype), var_ifelse)

    # we need to copy before doing shortcycling,
    # because within shortcycling do-block the ir is in a corrupt intermediate state
    blockid_mapping_copy, _var_mapping = copy_all_successors!(original_block)

    shortcycle_after_var_if_condition!(ir, original_block, condition) do shortcycle_block, continuation_block, blockid_mapping_shortcycle, var_isbooltype

        raw"""
        think about these two examples when trying to understand the code
        ```julia
        julia> using IRTools, IsDef

        julia> getval(::Val{T}) where T = T
        getval (generic function with 1 method)

        julia> ifelse(t, a, b) = getval(t) ? a : b
        ifelse (generic function with 1 method)

        julia> ir_ifelse = @code_ir ifelse(true, 1, 2)
        1: (%1, %2, %3, %4)
            br 2 unless %2
            return %3
        2:
            return %4

        julia> function ifelse2(t, a, b)
                println("before")
                if getval(t)
                    println("a = $a")
                    a
                else
                    println("b = $b")
                    b
                end
            end
        ifelse2 (generic function with 1 method)

        julia> ir_ifelse2 = @code_ir ifelse2(true, 1, 2)
        1: (%1, %2, %3, %4)
            %5 = Main.println("before")
            br 3 unless %2
        2:
            %6 = Base.string("a = ", %3)
            %7 = Main.println(%6)
            return %3
        3:
            %8 = Base.string("b = ", %4)
            %9 = Main.println(%8)
            return %4
        ```
        """

        # all blockid are shifted by 1 additionally, because the shortcycle adds an extra block
        blockid_mapping = Dict(
            blockid_mapping_shortcycle[k] => blockid_mapping_shortcycle[v]
            for (k, v) in blockid_mapping_copy
        )

        branch_ifnot_block = only(branches_for_condition(continuation_block, var_ifelse))
        branch_if_block = only(branches_for_condition(continuation_block, nothing))

        ifnot_block_copied = IRTools.block(ir, blockid_mapping[branch_ifnot_block.block])

        if branch_if_block.block == 0
            # if branch only has a return, we can plainly concentrate on ifnot_block
            # add unconditional branch to ifnot block
            IRTools.branch!(shortcycle_block, ifnot_block_copied, branch_ifnot_block.args..., unless = nothing)
            insert_Union_into_returns_step2_ifnotbranch(ifnot_block_copied, only(branch_if_block.args))

        else
            # if branch is its own other branch, possibly complex
            if_block_copied = IRTools.Block(ir, blockid_mapping[branch_if_block.block])
            # add unconditional branch to if block
            IRTools.branch!(shortcycle_block, if_block_copied, branch_if_block.args..., unless = nothing)

            # change if-block to account for both-semantics
            insert_Union_into_returns_step1_ifbranch(if_block_copied, ifnot_block_copied)
        end
    end
end


function insert_Union_into_returns_step1_ifbranch(if_block, ifnot_block)
    insert_Union_into_returns_step1_ifbranch(if_block, ifnot_block, Set{IRTools.Block}())
end

function insert_Union_into_returns_step1_ifbranch(if_block, ifnot_block, visited_blocks)
    ir = if_block.ir

    if_block ∉ visited_blocks || return nothing
    push!(visited_blocks, if_block)  # we use mutation because different branches may diamond-like come to the same branch down the road.

    for (i, branch) in enumerate(IRTools.branches(if_block))
        if branch.block != 0  # no return branch, i.e. conditional or unconditional branch
            branch_block = IRTools.block(ir, branch.block)
            # REMOVE branch to ifnotblock
            if branch_block == ifnot_block
                deleteat!(IRTools.branches(if_block), i)
                # update branch_block to subsequent branch
                # there is always such a subsequent branch, as at least an unconditional branch to if_block follows
                branch_block = IRTools.block(ir, IRTools.branches(if_block)[i].block)
            end
            insert_Union_into_returns_step1_ifbranch(branch_block, ifnot_block, visited_blocks)

            # anything coming after an unconditional branch can never happen
            isnothing(branch.condition) && break

        else # return branch
            # unconditional branch to ifnotblock INSTEAD of return
            return_value = only(branch.args)
            IRTools.branches(if_block)[i] = IRTools.branch(ifnot_block, unless = nothing)
            insert_Union_into_returns_step2_ifnotbranch(ifnot_block, return_value, visited_blocks)
            # if we encounter multiple return, we ignore those which are never called
            break
        end
    end
end


function insert_Union_into_returns_step2_ifnotbranch(ifnot_block, T)
    insert_Union_into_returns_step2_ifnotbranch(ifnot_block, T, Set{IRTools.Block}())
end

function insert_Union_into_returns_step2_ifnotbranch(ifnot_block, T, visited_blocks)
    ir = ifnot_block.ir

    ifnot_block ∉ visited_blocks || return nothing
    push!(visited_blocks, ifnot_block)

    for branch in IRTools.branches(ifnot_block)
        if branch.block != 0  # branch branch
            insert_Union_into_returns_step2_ifnotbranch(IRTools.block(ir, branch.block), T, visited_blocks)
            # anything coming after an unconditional branch can never happen
            isnothing(branch.condition) && break

        else # return branch
            # finally we can combine the two strengths
            i_endofblock = length(IRTools.BasicBlock(ifnot_block).stmts)
            newreturnexpr = xcall(TypeValueFunction(create_union), T, only(branch.args))
            newreturnvariable = insert!(ifnot_block, i_endofblock+1, newreturnexpr)
            branch.args[1] = newreturnvariable
            # if we encounter multiple return, we ignore those which are never called
            break
        end
    end
end

create_union(a, b) = Union{a, b}

isbooltype(::Type{Bool}) = true
isbooltype(other) = false



function branches_for_condition(block, condition)
    [branch for branch in IRTools.branches(block) if branch.condition == condition]
end



"""
        copy_all_successors!(IRTools.block(ir, 1))

will duplicate all blocks which are successors of the given block.
This is useful if you would like to create an alternative branch, slightly altering the original code
"""
function copy_all_successors!(branchblock)
    ir = branchblock.ir

    subsequent_computation = sort(
        collect(all_successors(branchblock)),
        by = block -> block.id
    )

    var_mapping = Dict()
    blockid_mapping = Dict()
    for block in subsequent_computation
        newblock = IRTools.block!(ir)
        blockid_mapping[block.id] = newblock.id
        # copy statements
        for (var, stmnt) in block
            newvar = push!(newblock, stmnt)
            var_mapping[var] = newvar
        end
        # create new arguments
        for arg in IRTools.arguments(block)
            newarg = IRTools.argument!(newblock)
            var_mapping[arg] = newarg
        end
        # copy branches
        append!(IRTools.branches(newblock), map(copy, IRTools.branches(block)))
    end

    # rename variables
    for newblockid in values(blockid_mapping)
        newblock = IRTools.Block(ir, newblockid)
        for (var, stmnt) in newblock
            newblock[var] = apply_var_mapping(var_mapping, stmnt)
        end
        for (i, branch) in enumerate(IRTools.branches(newblock))
            branch = @set branch.condition = apply_var_mapping(var_mapping, branch.condition)
            branch = @set branch.args = map(x -> apply_var_mapping(var_mapping, x), branch.args)
            if branch.block != 0
                branch = @set branch.block = blockid_mapping[branch.block]
            end
            IRTools.branches(newblock)[i] = branch
        end
    end
    (; blockid_mapping, var_mapping)
end

apply_var_mapping(var_mapping, expr) = MacroTools.prewalk(expr) do x
    get(var_mapping, x, x)
end
apply_var_mapping(var_mapping, var::IRTools.Variable) = get(var_mapping, var, var)


function all_successors(blocks::Union{Set, AbstractVector, Tuple})
    Set(successor for block in blocks for successor in IRTools.successors(block))
end
function all_successors(b::IRTools.Block)
    successors::Set{IRTools.Block} = Set(IRTools.successors(b))
    more_successors::Set{IRTools.Block} = union(successors, all_successors(successors))
    while successors != more_successors
        successors = more_successors
        more_successors = union(successors, all_successors(successors))
    end
    more_successors
end

end