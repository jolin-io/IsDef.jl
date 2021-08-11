using MacroTools
using IRTools: xcall, emptyargs!, arguments, block
using IRTools


# Generic fallback implementation
# -------------------------------


function Out(sigtype_typevalues::Type{T}) where {T<:Tuple}
  Core.println("Out sigtype_typevalues = $sigtype_typevalues")
  args = mark_typelevel_or_typevalue(Tuple_type_to_value(sigtype_typevalues))
  sigtype_notypevalues = signature_without_typevalues(sigtype_typevalues)
  Core.println("Out sigtype_notypevalues = $sigtype_notypevalues")
  Core.println("Out args = $args")
  extract_type_or_typevalue(_Out_dynamo(sigtype_notypevalues, args...))
end

function _extract_type_or_typevalue_ANDTHEN_Out_ANDTHEN_to_typelevel_or_typevalue(args::Vararg{Any, N}) where N
  args′ = Tuple_value_to_type(extract_type_or_typevalue(args))
  mark_typelevel_or_typevalue(Out(args′))
end

@dynamo function _Out_dynamo(::Type{Signature}, args...) where {Signature <: Tuple} # @dynamo is similar to @generated, i.e. we get the type of the original arguments 
  Core.println("_Out_dynamo sigtype.parameters = $(Signature.parameters)")
  Core.println("_Out_dynamo args = $args")
  
  all(isapplicable, Signature.parameters) || begin
    # TODO really needed? Core.println works better within generated functions. 
    Core.println("NOTAPPLICABLE: found NotApplicable in args, also returning NotApplicable")
    return :(IsDef.NotApplicable)
  end

  hassignature(Signature) || begin
    # TODO really needed? Core.println works better within generated functions. 
    Core.println("NOTAPPLICABLE: returning NotApplicable as couldn't find signature $Signature")
    return :(IsDef.NotApplicable)
  end
  
  ir = IR(Tuple_type_to_value(Signature)...)
  Core.println("_Out_dynamo ir-start = $ir")
  isnothing(ir) && error("""
    NOTAPPLICABLE: Encountered signature type with no IR (intermediate representation), please overwrite IsDef.Out respectively.
    Signature = $(Signature)
    IR(...) = IR($(Tuple_type_to_value(Signature)...))
  """)

  # sneakyinvoke trick taken from the IRTools example https://github.com/FluxML/IRTools.jl/blob/master/examples/sneakyinvoke.jl
  IRTools.argument!(ir, at = 1)

  Core.println("_Out_dynamo before recurse")

  # replace functioncalls with calls to Out
  for block in iterateblocks(ir)
    Core.println("_Out_dynamo block = $block")
    for (x, st) in block
      isexpr(st.expr, :call) || continue
      # TODO can we compose these functions also in here?
      Core.println("_Out_dynamo recursing into st.expr = $(st.expr)")
      ir[x] = xcall(_extract_type_or_typevalue_ANDTHEN_Out_ANDTHEN_to_typelevel_or_typevalue, st.expr.args...)
      shortcycle_if_notapplicable_error!(ir, x)
      break # this block is now done as we shortcycled it
    end
  end

  Core.println("_Out_dynamo after recurse")
  
  
  # Extra Handling
  # --------------

  # replace new with the first argument (i.e. the type going to be constructed)
  ir = MacroTools.prewalk(ir) do x
    isexpr(x, :new) ? Expr(:call, new_out, x.args[1]) : x
  end

  Core.println("_Out_dynamo ir-end = $ir")
  ir
end


"""
the output type of calling the internal function new, given the target output type
"""  # if we already receive a type-level, we need to strap of one layer
new_out(type::TypeLevel{Type{Type{T}}}) where T = mark_typelevel_or_typevalue(T)
new_out(type::TypeLevel) = error("this should not happen")
# if we receive value-level, we can directly treat it as type-level
new_out(type) = mark_typelevel_or_typevalue(type)

