using MacroTools
using IRTools: xcall, emptyargs!, arguments, block
using IRTools


# Generic fallback implementation
# -------------------------------

struct TypeLevel{T}
  value::T
  # we need Core.Typeof to collect all available type information
  TypeLevel(value) = new{Core.Typeof(value)}(value)
end
TypeLevel(typelevel::TypeLevel) = typelevel

Base._collect

function Base.show(io::IO, x::TypeLevel)
  print(io, "TypeLevel($(repr(x.value)))")
end

# we directly overload Tuple as several output arguments may be either Tuple or plain value, and in case of Tuple we want to broadcast
to_typelevel_or_bits(several::Union{Tuple, NamedTuple}) = map(to_typelevel_or_bits, several)
to_typelevel_or_bits(single::TypeLevel) = single
to_typelevel_or_bits(single) = if isbits(single)
  # bitstype type information would get lost when put into TypeLevel
  # hence we leave them literal, just as if someone would have written a literal value into the source code
  single
else
  TypeLevel(single)
end

extract_type(a::TypeLevel) = a.value
extract_type(a::Union{Tuple, NamedTuple}) = map(extract_type, a)
extract_type(a) = Core.Typeof(a)

extract_type_or_bits(a::TypeLevel) = a.value
extract_type_or_bits(a::Union{Tuple, NamedTuple}) = map(extract_type_or_bits, a)
extract_type_or_bits(a) = isbits(a) ? a : Core.Typeof(a)
extract_type_or_bits(a::Function) = Core.Typeof(a)  # functions are isbits surprisingly. Still it seems slightly more convenient to work on type level, as we can use Union then

# extract_func_from_signature(sigtype::Type{T}) where {T<:Tuple} = sigtype.parameters[1]
# function extract_args_from_signature(sigtype::Type{T}) where T<:Tuple
#   func, args... = tuple(sigtype.parameters...)
#   args
# end

make_sigtype_without_bits(::Type{T}) where T<:Tuple = Tuple_value_to_type(_make_sigtype_without_bits.(Tuple_type_to_value(T)))
_make_sigtype_without_bits(element::Type) = element
_make_sigtype_without_bits(bits) = Core.Typeof(bits)  # because we are in a signature, we know that bits are just those which are note Types

function Out(sigtype_bits::Type{T}) where {T<:Tuple}
  Core.println("Out sigtype_bits = $sigtype_bits")
  functype, argstype = signature_split_first(sigtype_bits)
  args = to_typelevel_or_bits(Tuple_type_to_value(argstype))
  sigtype_nobits = make_sigtype_without_bits(sigtype_bits)
  Core.println("Out sigtype_nobits = $sigtype_nobits")
  Core.println("Out args = $args")
  extract_type_or_bits(_Out_dynamo(functype, sigtype_nobits, args...))
end

# function Out(sigtype_bits::Type{T}, kwtype_bits::Type{NT}) where {T<:Tuple, NT<:NamedTuple}
#   Core.println("Out sigtype_bits = $sigtype_bits")
#   functype, argstype = signature_split_first(sigtype_bits)
#   args = to_typelevel_or_bits(Tuple_type_to_value(argstype))
#   sigtype_nobits = make_sigtype_without_bits(sigtype_bits)
#   Core.println("Out sigtype_nobits = $sigtype_nobits")
#   Core.println("Out args = $args")
  
#   if NT <: NamedTupleEmpty
#     kwargs = to_typelevel_or_bits(NamedTuple_type_to_value(kwtype_bits))
#     # following sneakyinvoke trick taken from the IRTools example https://github.com/FluxML/IRTools.jl/blob/master/examples/sneakyinvoke.jl
#     extract_type_or_bits(_Out_dynamo(Core.kwftype(functype), sigtype_nobits, kwargs, functype, args...))
#   else
#     extract_type_or_bits(_Out_dynamo(functype, sigtype_nobits, args...))
#   end
# end

function _extract_type_or_bits_ANDTHEN_Out_ANDTHEN_to_typelevel_or_bits(args::Vararg{Any, N}) where N
  args′ = Tuple_value_to_type(extract_type_or_bits(args))
  to_typelevel_or_bits(Out(args′))
end
# function _extract_type_or_bits_ANDTHEN_Out_ANDTHEN_to_typelevel_or_bits(args::Vararg{Any, N}) where N
#   iskw = length(args) >= 3 && isa(args[2], NamedTuple) && isa(args[1], Core.kwftype(typeof(args[3])))
#   if iskw
#     args′ = args |> Base.tail |> Base.tail |> Base.tail
#     kwargs′ = args[2]
#     args′′ = Tuple_value_to_type(extract_type_or_bits(args′))
#     kwargs′′ = NamedTuple_value_to_type(extract_type_or_bits(kwargs′))
#     to_typelevel_or_bits(Out(args′′, kwargs′′)) 
#   else
#     args′ = Tuple_value_to_type(extract_type_or_bits(args))
#     to_typelevel_or_bits(Out(args′, NamedTupleEmpty))
#   end
# end

@dynamo function _Out_dynamo(::Type{func}, ::Type{sigtype}, args...) where {func, sigtype <: Tuple} # @dynamo is similar to @generated, i.e. we get the type of the original arguments 
  Core.println("_Out_dynamo sigtype.parameters = $(sigtype.parameters)")
  Core.println("_Out_dynamo args = $args")
  
  if func === Core.IntrinsicFunction
    # TODO improve this errormessage such that it is raised at the correct place
    error("""Recursed to the Core.IntrinsicFunction $(sigargs[1]),
      please overload `Out` for whatever function called this Core.IntrinsicFunction.
      It is unfortunately not possible to dispatch on a specific intrinsic function,
      as the typeof all intrinsic functions is `Core.IntrinsicFunction`
    """)
  end
  
  all(isapplicable, sigtype.parameters) || begin
    # TODO really needed? Core.println works better within generated functions. 
    Core.println("NOTAPPLICABLE: found NotApplicable in args, also returning NotApplicable")
    return :(IsDef.NotApplicable)
  end

  hassignature(sigtype) || begin
    # TODO really needed? Core.println works better within generated functions. 
    Core.println("NOTAPPLICABLE: returning NotApplicable as couldn't find signature $sigtype")
    return :(IsDef.NotApplicable)
  end
  
  ir = IR(func, Tuple_type_to_value(signature_split_first(sigtype)[2])...)
  Core.println("_Out_dynamo ir-start = $ir")
  isnothing(ir) && error("""
    NOTAPPLICABLE: Encountered signature type with no IR (intermediate representation), please overwrite IsDef.Out respectively.
    Signature = $(sigtype)
    IR(...) = IR($func, $(Tuple_type_to_value(signature_split_first(sigtype)[2])...))
  """)

  # sneakyinvoke trick taken from the IRTools example https://github.com/FluxML/IRTools.jl/blob/master/examples/sneakyinvoke.jl
  IRTools.argument!(ir, at = 2)

  Core.println("_Out_dynamo before recurse")

  # replace functioncalls with calls to Out
  for block in iterateblocks(ir)
    Core.println("_Out_dynamo block = $block")
    for (x, st) in block
      isexpr(st.expr, :call) || continue
      # creating our sigtype_bits Tuple
      # TODO can we compose these functions also in here?
      Core.println("_Out_dynamo recursing into st.expr = $(st.expr)")
      ir[x] = xcall(_extract_type_or_bits_ANDTHEN_Out_ANDTHEN_to_typelevel_or_bits, st.expr.args...)
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


Base.indexed_iterate
"""
the output type of calling the internal function new, given the target output type
"""  # if we already receive a type-level, we need to strap of one layer
new_out(type::TypeLevel{Type{Type{T}}}) where T = to_typelevel_or_bits(T)
new_out(type::TypeLevel) = error("this should not happen")
# if we receive value-level, we can directly treat it as type-level
new_out(type) = to_typelevel_or_bits(type)

