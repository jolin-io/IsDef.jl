using MacroTools

# Generic fallback implementation
# -------------------------------

struct TypeLevel{T}
  value::T
  # we need Core.Typeof to collect all available type information
  TypeLevel(value) = new{Core.Typeof(value)}(value)
end
function Base.show(io::IO, x::TypeLevel)
  print(io, "TypeLevel($(repr(x.value)))")
end

mark_as_typelevel(several::Tuple) = mark_as_typelevel.(several)
mark_as_typelevel(single::TypeLevel) = single
mark_as_typelevel(single::T) where T = if isbitstype(T)
  # bitstype type information would get lost when put into TypeLevel
  # hence we leave them literal, just as if someone would have written a literal value into the source code
  single
else
  TypeLevel(single)
end

extract_type(a::TypeLevel) = a.value
extract_type(a::Tuple) = extract_type.(a)
extract_type(a) = Core.Typeof(a)

extract_type_or_bits(a::TypeLevel) = a.value
extract_type_or_bits(a::Tuple) = extract_type_or_bits.(a)
extract_type_or_bits(a::T) where T = isbitstype(T) ? a : Core.Typeof(a)

"""
the output type of calling the internal function new, given the target output type
"""
function new_out(type::TypeLevel)
  return type

  # TODO why wasn't this a identity function?
  # if we already receive a type-level, we need to strap of one layer
  isdefined(type.value, :parameters) && length(type.value.parameters) == 1 || error("expected singleton `Type{MyType}`, but got `$(type.value)`")
  mark_as_typelevel(type.value.parameters[1])
end
# if we receive value-level, we can directly treat it as type-level
new_out(type) = mark_as_typelevel(type)

function Out(sigtype::Type{<:Tuple})
  hassignature(sigtype) || begin
    # TODO really needed? Core.println works better within generated functions. 
    Core.println("NOTAPPLICABLE: returning NotApplicable as couldn't find signature $sigtype")
    return NotApplicable
  end
  ir = IR(sigtype.parameters...)
  println(sigtype.parameters)
  println(ir)

  isnothing(ir) && error("NOTAPPLICABLE: Encountered signature type with no IR (intermediate representation), please overwrite IsDef.Out respectively. Signature = $(sigtype).")
  # replace functioncalls with calls to _Out_TypeLevel
  recurse!(ir, _Out_TypeLevel)
  # replace new with the first argument (i.e. the type going to be constructed)
  ir = MacroTools.prewalk(ir) do x
    isexpr(x, :new) ? Expr(:call, new_out, x.args[1]) : x
  end
  # wrap type arguments in our own wrapper to indicate this is already type-level
  result = IRTools.evalir(ir, mark_as_typelevel.(sigtype.parameters)...)
  extract_type(result)
end

function _Out_TypeLevel(sigargs...)
  Core.println("sigargs = $(sigargs)")
  sigtypeargs = extract_type(sigargs)
  Core.println("sigtypeargs = $(sigtypeargs)")
  if sigtypeargs[1] === Core.IntrinsicFunction
    # TODO improve this errormessage such that it is raised at the correct place
    error("""Recursed to the Core.IntrinsicFunction $(sigargs[1]),
      please overload `Out` for whatever function called this Core.IntrinsicFunction.
      It is unfortunately not possible to dispatch on a specific intrinsic function,
      as the typeof all intrinsic functions is `Core.IntrinsicFunction`
    """)
  end
  all(isapplicable, sigtypeargs) || begin
    # TODO really needed? Core.println works better within generated functions. 
    Core.println("NOTAPPLICABLE: found NotApplicable in args, also returning NotApplicable")
    return TypeLevel(NotApplicable)
  end
  sigtype = Tuple{sigtypeargs...}
  Core.println("sigtype = $sigtype")
  result = Out(sigtype)
  Core.println("out = $result")
  mark_as_typelevel(result)
end