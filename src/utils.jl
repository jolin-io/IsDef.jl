using Setfield

# Type helpers
# ------------

"""
  IntrinsicFunction
"""
struct IntrinsicFunction{Function}
  IntrinsicFunction{F}() where F = error("""
    `IsDef.IntrinsicFunction{Function}` is a more detailed helper type used instead of `Core.IntrinsicFunction`.
    It has no instance.
  """)
end


"""
    istypevalue(1)

like `Base.isbits`, however accounts for surprings facts like Symbol not being bits type.

"typevalue" is understood as in the documentation about Value Types https://docs.julialang.org/en/v1/manual/types/#%22Value-types%22  
"""
istypevalue(::T) where T = istypevaluetype(T)

"""
    istypevaluetype(Int)

like `Base.isbitstype`, however accounts for surprings facts like Symbol not being bits type.

"typevalue" is understood as in the documentation about Value Types https://docs.julialang.org/en/v1/manual/types/#%22Value-types%22  
"""
istypevaluetype(::Type{Symbol}) = true
istypevaluetype(type) = isbitstype(type)

kwftype(f::Function) = kwftype(typeof(f))
kwftype(::Type{F}) where F <: Function = Core.kwftype(F)


function hassignature(::Type{Signature}; world=typemax(UInt)) where Signature <: Tuple 
  if Signature.parameters[1] <: Core.Builtin
    builtin_function = repr(Signature.parameters[1])
    error("""
      Recursed to builtin function `$builtin_function`.
      Please, overwrite `IsDef.Out` for `IsDef.Out(::Type{Tuple{typeof($builtin_function), ...}}) = ...`
      or for a previous function.

      The queried signature was:
      $Signature
    """)  
  elseif Signature.parameters[1] <: IsDef.IntrinsicFunction
    intrinsic_function = repr(Signature.parameters[1].parameters[1])
    error("""
      Recursed to intrinsic function `$intrinsic_function`.
      Please, overwrite `IsDef.Out` for `IsDef.Out(::Type{Tuple{IsDef.IntrinsicFunction{$intrinsic_function}, ...}}) = ...`
      or for a previous function.
      
      The queried signature was:
      $Signature
    """)  
  end
  result = @ccall jl_gf_invoke_lookup(Signature::Any, world::UInt)::Any
  result !== nothing
end

promote_types_or_typevalues(a::Type) = a
promote_types_or_typevalues(a) = Core.Typeof(a)

promote_types_or_typevalues(a::Type, b::Type) = promote_type(a, b)
promote_types_or_typevalues(a, b::Type) = promote_type(Core.Typeof(a), b)
promote_types_or_typevalues(a::Type, b) = promote_type(a, Core.Typeof(b))
promote_types_or_typevalues(a, b) = promote_type(Core.Typeof(a), Core.Typeof(b))

wrap_typevalue_into_Val(a::Type) = a
wrap_typevalue_into_Val(a) = Val(a)

function signature_split_first(sigtype::Type{T}) where {T<:Tuple}
  func, args... = tuple(sigtype.parameters...)
  func, Tuple{args...}
end
function signature_add_first(first::F, sigtype::Type{T}) where {F, T<:Tuple}
  Tuple{F, sigtype.parameters...}
end

signature_without_typevalues(::Type{T}) where T<:Tuple = Tuple_value_to_type(map(_without_typevalue, Tuple_type_to_value(T)))
_without_typevalue(type::Type) = type
_without_typevalue(typevalue) = Core.Typeof(typevalue)  # because we are in a signature, we know that typevalues are just those which are note Types


"""
TODO it should be `Tuple{map(Core.Typeof, tuple(T.parameters...))}(tuple(T.parameters...))`
however this destroys type information as of now, see https://discourse.julialang.org/t/tuple-constructor-forgets-types/65730
i.e. we use the simpler version as of now
"""
Tuple_type_to_value(::Type{T}) where T<:Tuple = tuple(T.parameters...)  
Tuple_value_to_type(mytuple::Tuple) = Tuple{mytuple...}

function NamedTuple_value_to_type(namedtuple::NT) where NT<:NamedTuple
  NamedTuple{keys(namedtuple), Tuple{values(namedtuple)...}}
end
function NamedTuple_type_to_value(::Type{NamedTuple{Names, NT}}) where {Names, NT<:Tuple}
  NamedTuple{Names, Tuple{map(Core.Typeof, tuple(NT.parameters...))...}}(tuple(NT.parameters...))
end

const NamedTupleEmpty = typeof(NamedTuple())



# IRTools helpers
# ---------------

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
  (IRTools.block(ir, 1), 0)
else 
  IRTools.Inner.blockidx(ir, var)
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


@doc raw"""
    shortcycle_if_notapplicable_error!(ir::IRTools.IR, var::IRTools.Variable)

Rewrites the intermediate representation `ir` right after the given `var` such that the algorithm immediately returns `NotApplicable` in case that `var === NotApplicableError`.

Examples
--------

```jldoctest
julia> using IRTools, IsDef

julia> foo(::Int) = NotApplicableError
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

julia> IsDef.shortcycle_if_notapplicable_error!(ir_g, IRTools.var(3))
1: (%1, %2)
  %3 = Main.foo(%2)
  %8 = (===)(%3, NotApplicableError)
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
function shortcycle_if_notapplicable_error!(ir::IRTools.IR, var::IRTools.Variable)
  shortcycle_after_var_if_condition!(ir, var, xcall(===, var, NotApplicableError)) do shortcycle_block, _
    IRTools.return!(shortcycle_block, GlobalRef(IsDef, :NotApplicable))
  end
end

function shortcycle_after_var_if_condition!(finish_block!::Function, ir::IRTools.IR, var::IRTools.Variable, condition)
  oldblock, i_var_oldblock = blockidx(ir, var)
  newblock = IRTools.block!(ir, oldblock.id+1)  # insert right after given block

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
  elseif isargument(ir, var)
    IRTools.pushfirst!(ir, condition)
  else
    IRTools.insertafter!(ir, var, condition)
  end
  IRTools.branch!(oldblock, newblock, unless = var_is_condition)
  finish_block!(oldblock, var_is_condition)
  return var_is_condition
end


function lift_ifelse!(ir::IRTools.IR, var_ifelse::IRTools.Variable)
  IRTools.explicitbranch!(ir)
  Core.println("var_ifelse = $var_ifelse")

  original_block = blockidx(ir, var_ifelse)[1]
  lastvar_original_block = lastvar_of_block(original_block)
  
  shortcycle_after_var_if_condition!(ir, lastvar_original_block, xcall(isbooltype, var_ifelse)) do both_block, var_isbooltype
    
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

    notboth_block = IRTools.Block(ir, both_block.id + 1)
    Core.println("notboth_blockid = $(notboth_block.id)")
    
    Core.println("""
      ir = $ir
      IRTools.branches(notboth_block) = $(IRTools.branches(notboth_block))
      conditions = $([(branch.condition, var_ifelse, branch.condition == var_ifelse) for branch in IRTools.branches(notboth_block)])
    """)
    _branch_ifnot_block = only(branches_for_condition(notboth_block, var_ifelse))
    Core.println("_branch_ifnot_block = $(_branch_ifnot_block.block)")
    
    blockid_mapping, var_mapping = copy_all_successors!(notboth_block)
    Core.println("blockid_mapping = $blockid_mapping \nvar_mapping = $var_mapping")
    
    branch_ifnot_block = only(branches_for_condition(notboth_block, var_ifelse))
    Core.println("branch_ifnot_block = $(branch_ifnot_block.block)")
    
    ifnot_block_copied = IRTools.block(ir, blockid_mapping[branch_ifnot_block.block])
    branch_if_block = only(branches_for_condition(notboth_block, nothing))

    if branch_if_block.block == 0
      # if branch only has a return, we can plainly concentrate on ifnot_block
      # add unconditional branch to ifnot block
      IRTools.branch!(both_block, ifnot_block_copied, unless = nothing)
      insert_Union_into_returns_step2_ifnotbranch(ifnot_block_copied, only(branch_if_block.args))

    else
      # if branch is its own other branch, possibly complex
      if_block_copied = IRTools.Block(ir, blockid_mapping[branch_if_block.block])
      # add unconditional branch to if block
      IRTools.branch!(both_block, if_block_copied, unless = nothing)
      
      # TODO only for debugging purposes:
      var_if_block_copied = pushfirst!(if_block_copied, xcall(identity, :if_block_copied))
      

      # change if block to account for both-semantics
      # we need to identify ifnot block even if if-blocks changed
      # we do so by adding a dummy variable
      var_ifnot_block_copied = pushfirst!(ifnot_block_copied, xcall(identity, :ifnot_block_copied))
      get_ifnot_block_copied() = blockidx(ir, var_ifnot_block_copied)[1]
      insert_Union_into_returns_step1_ifbranch(if_block_copied, get_ifnot_block_copied)
    end
  end
end

function insert_Union_into_returns_step1_ifbranch(if_block, get_ifnot_block)
  insert_Union_into_returns_step1_ifbranch(if_block, get_ifnot_block, Set{IRTools.Variable}())
end
function insert_Union_into_returns_step1_ifbranch(if_block, get_ifnot_block, visited_lastvar)
  ir = if_block.ir
  lastvar = lastvar_of_block(if_block)  # TODO is this valid way of identifying a block? maybe we should rather insert a label to the top and search for the label, in case of changes that should be stabler
  lastvar in visited_lastvar && return nothing
  push!(visited_lastvar, lastvar)  # we use mutatation because different branches may diamond-like come to the same branch down the road.
    
  for (i, branch) in enumerate(IRTools.branches(if_block))
    if branch.block != 0  # no return branch, i.e. conditional or unconditional branch 
      branch_block = IRTools.block(ir, branch.block)
      # REMOVE branch to ifnotblock
      if branch_block == get_ifnot_block()
        Core.println("BEFORE ir = $(ir)")
        Core.println("BEFORE IRTools.branches(if_block) = $(IRTools.branches(if_block))")
        deleteat!(IRTools.branches(if_block), i)
        Core.println("AFTER IRTools.branches(if_block) = $(IRTools.branches(if_block))")
        # update branch_block to subsequent branch
        # there is always such a subsequent branch, as at least an unconditional branch to if_block follows
        branch_block = IRTools.block(ir, IRTools.branches(if_block)[i].block)
      end
      insert_Union_into_returns_step1_ifbranch(branch_block, get_ifnot_block, visited_lastvar)
      
      # anything coming after an unconditional branch can never happen
      isnothing(branch.condition) && break 

    else # return branch
      ifnot_block = get_ifnot_block()
      # unconditional branch to ifnotblock INSTEAD of return
      return_value = only(branch.args)
      IRTools.branches(if_block)[i] = IRTools.branch(ifnot_block, unless = nothing)
      insert_Union_into_returns_step2_ifnotbranch(ifnot_block, return_value, visited_lastvar)
      # if we encounter multiple return, we ignore those which are never called
      break
    end
  end
end

function insert_Union_into_returns_step2_ifnotbranch(ifnot_block, T)
  insert_Union_into_returns_step2_ifnotbranch(ifnot_block, T, Set{IRTools.Variable}())
end

function insert_Union_into_returns_step2_ifnotbranch(ifnot_block, T, visited_lastvar)
  ir = ifnot_block.ir
  lastvar = lastvar_of_block(ifnot_block)
  lastvar in visited_lastvar && return nothing
  push!(visited_lastvar, lastvar)
  
  for branch in IRTools.branches(ifnot_block)
    if branch.block != 0  # branch branch
      insert_Union_into_returns_step2_ifnotbranch(IRTools.block(ir, branch.block), T, visited_lastvar)
      # anything coming after an unconditional branch can never happen
      isnothing(branch.condition) && break
  
    else # return branch
      # finally we can combine the two strengths
      newreturntype = IRTools.insertafter!(ir, lastvar, xcall(create_union, T, only(branch.args)))
      branch.args[1] = newreturntype
      # if we encounter multiple return, we ignore those which are never called
      break
    end
  end
end

function create_union(args::Vararg{Any, N}) where N   
  Union{args...}
end

function lastvar_of_block(block)
  block_defs = [(i_st_within_block, i_var) for (i_var, (i_block, i_st_within_block)) in enumerate(block.ir.defs) if i_block == block.id]
  if isempty(block_defs)
    dummy_var = push!(block, xcall(identity, :dummy_variable))
    return dummy_var
  else
    _, i_lastvar_block = maximum(block_defs)
    return IRTools.var(i_lastvar_block)
  end
end

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



function inline_all_blocks_with_arguments!(blocks)
  blocks_with_arguments = [block for block in blocks if !isempty(IRTools.arguments(block))]
  if !isempty(blocks_with_arguments)
    ir = blocks_with_arguments[1].ir
  end
  for block_with_arguments in blocks_with_arguments
    blocks_referring = [block for block in blocks for branch in IRTools.branches(block) if branch.block == block_with_arguments.id]

    for block_referring in blocks_referring
      n_statements_before = length(IRTools.BasicBlock(block_referring).stmts)
      
      i, branch = only([(i, branch) for (i, branch) in enumerate(IRTools.branches(block_referring)) if branch.block == block_with_arguments.id])
      @assert isnothing(branch.condition) "can only inline unconditional-branches-with-arguments as of now, however found conditional branch with arguments: $branch"
      
      var_mapping = Dict()
      # copy statements
      for (var, stmnt) in block_with_arguments
        newvar = push!(block_referring, stmnt)
        var_mapping[var] = newvar
      end

      # map arguments
      for (arg, newarg) in zip(IRTools.arguments(block_with_arguments), branch.args)
        var_mapping[arg] = newarg
      end

      # change variables
      n_statements_after = length(IRTools.BasicBlock(block_referring).stmts)
      for i in (n_statements_before+1):n_statements_after
        block_referring[i] = apply_var_mapping(var_mapping, block_referring[i])
      end
  
      # adapt branches
      newbranches = map(copy, IRTools.branches(block_with_arguments))
      for (i, branch) in enumerate(newbranches)
        branch = @set branch.condition = apply_var_mapping(var_mapping, branch.condition)
        branch = @set branch.args = map(x -> apply_var_mapping(var_mapping, x), branch.args)
        newbranches[i] = branch
      end
      append!(IRTools.branches(block_referring), newbranches)
      # delete the old branching-with-arguments now to be safe with block-id changes 
      deleteat!(IRTools.branches(block_referring), i)
    end
    
    IRTools.deleteblock!(ir, block_with_arguments.id)
  end
end


# TypeLevel
# ---------

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
mark_typelevel_or_typevalue(several::Union{Tuple, NamedTuple}) = map(mark_typelevel_or_typevalue, several)
mark_typelevel_or_typevalue(single::TypeLevel) = single
mark_typelevel_or_typevalue(single::Function) = TypeLevel(single) # Functions are isbits, however we treat them as types everywhere
# typevalue (like 1, :symbol, true, ...) type information would get lost when put into TypeLevel
# hence we leave them literal, just as if someone would have written a literal value into the source code
mark_typelevel_or_typevalue(single) = istypevalue(single) ? single : TypeLevel(single)

extract_type(a::TypeLevel) = a.value
extract_type(a::Union{Tuple, NamedTuple}) = map(extract_type, a)
extract_type(a) = Core.Typeof(a)

extract_type_or_typevalue(a::TypeLevel) = a.value
extract_type_or_typevalue(a::Union{Tuple, NamedTuple}) = map(extract_type_or_typevalue, a)
extract_type_or_typevalue(a) = istypevalue(a) ? a : Core.Typeof(a)
extract_type_or_typevalue(a::Function) = Core.Typeof(a)  # functions are valid typevalues surprisingly. Still it seems slightly more convenient to work on type level, as we can use Union then
extract_type_or_typevalue(a::Core.IntrinsicFunction) = IsDef.IntrinsicFunction{a}


isbooltype(::Type{Bool}) = true
isbooltype(::Type{TypeLevel{Type{Bool}}}) = true
isbooltype(other) = false