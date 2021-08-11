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

  original_block = blockidx(ir, var_ifelse)[1]
  lastvar_original_block = lastvar_of_block(original_block)
  
  shortcycle_after_var_if_condition!(ir, lastvar_original_block, xcall(isbooltype, var_ifelse)) do both_block, var_isbooltype
    
    raw"""
    think about these two examples when trying to understand the code 
    ```julia
    julia> ifelse(t, a, b) = t ? a : b
    ifelse (generic function with 1 method)

    julia> ir_ifelse = @code_ir ifelse(true, 1, 2)
    1: (%1, %2, %3, %4)
      br 2 unless %2
      return %3
    2:
      return %4
  
    julia> function ifelse2(t, a, b)
        println("before")
        if t
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
    ifnot_block = IRTools.Block(ir, only(branches_for_condition(notboth_block, var_ifelse)).block)
    branch_if_block = only(branches_for_condition(notboth_block, nothing))

    if branch_if_block.block == 0
      # if branch only has a return, we can plainly concentrate on ifnot_block
      # add unconditional branch to ifnot block
      IRTools.branch!(both_block, ifnot_block, unless = nothing)
      insert_Union_into_returns2!(ifnot_block, branch_if_block.args[1], var_isbooltype)

    else
      # if branch is its own other branch, possibly complex
      if_block = IRTools.Block(ir, branch_if_block.block)
      # add unconditional branch to if block
      IRTools.branch!(both_block, if_block, unless = nothing)

      # change if block to account for both-semantics
      # we need to identify ifnot block even if blocks changed
      # we do so by adding a dummy variable
      var_ifnot_block = pushfirst!(ifnot_block, xcall(identity, :ifnot_block))
      get_ifnot_block() = blockidx(ir, var_ifnot_block)[1]
      insert_Union_into_returns1!(if_block, get_ifnot_block, var_isbooltype)
    end
  end
end

function insert_Union_into_returns1!(if_block, get_ifnot_block, var_isbooltype)
  insert_Union_into_returns1!(if_block, get_ifnot_block, var_isbooltype, Set{IRTools.Variable}())
end
function insert_Union_into_returns1!(if_block, get_ifnot_block, var_isbooltype, visited_lastvar)
  ir = if_block.ir
  lastvar = lastvar_of_block(if_block)
  lastvar in visited_lastvar && return nothing
  visited_lastvar′ = union(visited_lastvar, [lastvar])
    
  for branch in IRTools.branches(if_block)
    if branch.block != 0
      branch_block = IRTools.block(ir, branch.block)
      # skip over ifnotblock
      branch_block != get_ifnot_block() || continue
      insert_Union_into_returns1!(branch_block, get_ifnot_block, var_isbooltype, visited_lastvar′)
      
      # anything coming after an unconditional branch can never happen
      isnothing(branch.condition) && break 

    else # return branch
      shortcycle_after_var_if_condition!(ir, lastvar, var_isbooltype) do returning_block, _
        ifnot_block = get_ifnot_block()
        # unconditional branch to ifnotblock
        IRTools.branch!(returning_block, ifnot_block, unless = nothing)
        insert_Union_into_returns2!(ifnot_block, branch.args[1], var_isbooltype, visited_lastvar′)
      end
    end
  end
end

function insert_Union_into_returns2!(ifnot_block, T, var_isbooltype)
  insert_Union_into_returns2!(ifnot_block, T, var_isbooltype, Set{IRTools.Variable}())
end

function insert_Union_into_returns2!(ifnot_block, T, var_isbooltype, visited_lastvar)
  ir = ifnot_block.ir
  lastvar = lastvar_of_block(ifnot_block)
  lastvar in visited_lastvar && return nothing
  visited_lastvar′ = union(visited_lastvar, [lastvar])
  
  for branch in IRTools.branches(ifnot_block)
    if branch.block != 0  # branch branch
      insert_Union_into_returns2!(IRTools.block(ir, branch.block), T, var_isbooltype, visited_lastvar′)
      # anything coming after an unconditional branch can never happen
      isnothing(branch.condition) && break
  
    else # return branch
      shortcycle_after_var_if_condition!(ir, lastvar, var_isbooltype) do returning_block, _
        # finally we can combine the two strengths
        newreturntype = IRTools.insertafter!(ir, lastvar, xcall(create_union, T, branch.args[1]))
        IRTools.return!(returning_block, newreturntype)
      end

      # you may wonder why this is needed, however we actually change the branches of the block in place
      # as a return indicated the last used branch of a block (and we do not wont to change the new shortcycle block we just created)
      # we simply break here
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



function copy_all_successors!(branchblock)
  ir = branchblock.ir
  subsequent_computation = collect(IRTools.Inner.successors(branchblock))

  var_mapping = Dict()
  block_mapping = Dict()
  for block in subsequent_computation
      newblock = IRTools.block!(ir)
      block_mapping[block] = newblock
      # copy statements
      for (var, stmnt) in block
          newvar = push!(newblock, stmnt)
          var_mapping[var] = newvar
      end
      # copy branches
      append!(IRTools.branches(newblock), map(copy, IRTools.branches(block)))
  end

  apply_var_mapping(expr) = MacroTools.prewalk(expr) do x
      get(var_mapping, x, x)
  end
  apply_var_mapping(var::IRTools.Variable) = get(var_mapping, var, var)

  for newblock in values(block_mapping)
      for (var, stmnt) in newblock
          newblock[var] = apply_var_mapping(stmnt)
      end
      for (i, branch) in enumerate(IRTools.branches(newblock))
          @set branch.condition = apply_var_mapping(branch.condition)
          @set branch.args = map(apply_var_mapping, branch.args)
      end
  end
  (; block_mapping, var_mapping)
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