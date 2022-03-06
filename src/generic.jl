using IRTools: xcall, emptyargs!, arguments, block
using IRTools
using MacroTools: isexpr, MacroTools
using SimpleMatch: @match

# Generic fallback implementation
# -------------------------------


@generated function Out(::Type{sigtype_typevalues}) where {sigtype_typevalues<:Tuple}
  # we have two versions of _Out_dynamo, one with the final dynamo, and one returning just the implementation itself
  # similar to macros, only that the dynamo needs to be compiled separately
  create_expr_Out_dynamo(args...) = :($_Out_dynamo($(args...)))
  Out_implementation(sigtype_typevalues, create_expr_Out_dynamo)
end

"""
this level does generic checks which are useful for every implementation
"""
function Out_implementation(::Type{sigtype_typevalues}, _Out_dynamo=_Out_dynamo_implementation) where {sigtype_typevalues<:Tuple}
  @debug "Out sigtype_typevalues = $sigtype_typevalues"
  sigtype_notypevalues = signature_without_typevalues(sigtype_typevalues)
  @debug "Out sigtype_notypevalues = $sigtype_notypevalues"

  all(isapplicable, sigtype_notypevalues.parameters) || begin
    @debug "NOTAPPLICABLE: found NotApplicable in args, also returning NotApplicable"
    return NotApplicable
  end

  return _Out_implementation2(sigtype_typevalues, _Out_dynamo)
end


function _Out_implementation2(::Type{sigtype_typevalues}, _Out_dynamo) where {sigtype_typevalues <: Tuple{TypeLevelFunction, Vararg}}
  functype, args... = Tuple_type_to_value(sigtype_typevalues)
  func = @match(functype) do f
    f(::Type{TypeLevelFunction{F}}) where F = F.instance
  end
  return func(args...)
end

function _Out_implementation2(::Type{sigtype_typevalues}, _Out_dynamo) where {sigtype_typevalues<:Tuple}
  sigtype_notypevalues = signature_without_typevalues(sigtype_typevalues)
  @debug "Out sigtype_notypevalues = $sigtype_notypevalues"
  
  args = mark_typelevel_or_typevalue(Tuple_type_to_value(sigtype_typevalues))
  @debug "Out args = $args"
  
  return :($(_Out_dynamo(sigtype_notypevalues, args...)) |> $extract_type_or_typevalue)
end


function _extract_type_or_typevalue_ANDTHEN_Out_ANDTHEN_to_typelevel_or_typevalue(args...)
  args′ = Tuple_value_to_type(extract_type_or_typevalue(args))
  mark_typelevel_or_typevalue(Out(args′))
end

# @dynamo is similar to @generated, i.e. we get the type of the original arguments 
@dynamo function _Out_dynamo(::Type{Signature}, args...) where {Signature <: Tuple}
  _Out_dynamo_implementation(Signature, args...)
end


function _Out_dynamo_implementation(::Type{Signature}, args...) where {Signature <: Tuple}
  @debug "_Out_dynamo sigtype.parameters = $(Signature.parameters)"
  @debug "_Out_dynamo args = $args"

  # we do not recurse into functions which consist purely out of Base constructs
  # they should all get a proper interface definition
  any(!_isleaf, Signature.parameters) || begin
    function_type = Signature.parameters[1]
    error("""
      Recursed to leaf function `$function_type` (everything is from Base or Core).
      Please, overwrite `IsDef.Out` for `IsDef.Out(::Type{Tuple{$function_type, ...}}) = ...`
      or for a previous function.

      The queried signature was:
      $Signature
    """)
  end

  hassignature(Signature) || begin
    @debug "NOTAPPLICABLE: returning NotApplicable as couldn't find signature $Signature"
    return :(IsDef.NotApplicable)  # we need to return ::Expr because IRTools does not allow arbitrary values
  end

  
  ir = IR(Tuple_type_to_value(Signature)...)
  @debug "_Out_dynamo ir-start = $ir"
  isnothing(ir) && error("""
    NOTAPPLICABLE: Encountered signature type with no IR (intermediate representation), please overwrite IsDef.Out respectively.
    Signature = $(Signature)
    IR(...) = IR($(Tuple_type_to_value(Signature)...))
  """)

  # sneakyinvoke trick taken from the IRTools example https://github.com/FluxML/IRTools.jl/blob/master/examples/sneakyinvoke.jl
  IRTools.argument!(ir, at = 1)


  # all function calls which return arguments which are not used can be safely ignored, as for type inference mutation can be ignored
  keep_only_what_is_explicitly_used!(ir)

  # Change all branch.condition such that they can work with `Bool` in addition to `true` and `false`
  lift_ifelse!(ir)

  # replace functioncalls with calls to Out
  # as this introduces new branches, it has to be done after lifting ifelse
  @debug "_Out_dynamo before recurse"
  for block in iterateblocks(ir)
    @debug "_Out_dynamo block = $block"
    for (x, st) in block
      isexpr(st.expr, :call) || continue
      # TODO can we compose these functions also in here?
      @debug "_Out_dynamo recursing into st.expr = $(st.expr)"
      ir[x] = xcall(_extract_type_or_typevalue_ANDTHEN_Out_ANDTHEN_to_typelevel_or_typevalue, st.expr.args...)
      shortcycle_if_notapplicable!(ir, x)
      break # this block is now done as we shortcycled it
    end
  end

  @debug "_Out_dynamo after recurse"
  

  # Remove Internal wrapper
  # -----------------------

  # remove_internal_flags!(ir)

  
  # Extra Handling
  # --------------

  # replace new with the first argument (i.e. the type going to be constructed)
  ir = MacroTools.prewalk(ir) do x
    isexpr(x, :new) ? Expr(:call, new_out, x.args[1]) : x
  end

  @debug "_Out_dynamo ir-end = $ir"
  ir
end


"""
    _isleaf(type) -> Bool

check whether the type does not need further recurse.
If the type belongs to Core or Base it is considered a leaf.

In order to define an exception, _isleaf would need to be overwritten.
"""
function _isleaf(type)
  mod = Base.typename(type).module
  return _isleaf(mod)
end

function _isleaf(mod::Module)
  mod ∈ (Base, Core) && return true
  parent = Base.parentmodule(mod)
  mod === parent && return false
  return _isleaf(parent)
end


"""
the output type of calling the internal function new, given the target output type
"""  # if we already receive a type-level, we need to strap of one layer
new_out(type::TypeLevel{Type{Type{T}}}) where T = mark_typelevel_or_typevalue(T)
new_out(type::TypeLevel) = error("this should not happen")
# if we receive value-level, we can directly treat it as type-level
new_out(type) = mark_typelevel_or_typevalue(type)

