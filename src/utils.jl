# Type helpers
# ------------

function hassignature(sigtype; world=typemax(UInt))
  if sigtype.parameters[1] <: Core.Builtin
    error("Recursed to builtin function `$(sigtype.parameters[1])`, please overwrite `IsDef.Out` for the builtin or for a previous function.")  
  end
  result = @ccall jl_gf_invoke_lookup(sigtype::Any, world::UInt)::Any
  result !== nothing
end

promote_types_or_bits(a::Type) = a
promote_types_or_bits(a) = Core.Typeof(a)

promote_types_or_bits(a::Type, b::Type) = promote_type(a, b)
promote_types_or_bits(a, b::Type) = promote_type(Core.Typeof(a), b)
promote_types_or_bits(a::Type, b) = promote_type(a, Core.Typeof(b))
promote_types_or_bits(a, b) = promote_type(Core.Typeof(a), Core.Typeof(b))

wrap_bits_into_Val(a::Type) = a
wrap_bits_into_Val(a) = Val(a)

function signature_split_first(sigtype::Type{T}) where {T<:Tuple}
  func, args... = tuple(sigtype.parameters...)
  func, Tuple{args...}
end
function signature_add_first(first::F, sigtype::Type{T}) where {F, T<:Tuple}
  Tuple{F, sigtype.parameters...}
end


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
  shortcycle_after_var_if_condition!(ir, var, IRTools.xcall(===, var, NotApplicableError)) do shortcycle_block
    IRTools.return!(shortcycle_block, GlobalRef(IsDef, :NotApplicable))
  end
end

function shortcycle_after_var_if_condition!(finish_block!::Function, ir::IRTools.IR, var::IRTools.Variable, condition)
  oldblock, i_var_oldblock = IRTools.Inner.blockidx(ir, var)
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
  var_is_notapplicable_error = IRTools.insertafter!(ir, var, condition)
  IRTools.branch!(oldblock, newblock, unless = var_is_notapplicable_error)
  finish_block!(oldblock)
  return ir
end