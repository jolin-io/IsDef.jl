using MacroTools

# Generic fallback implementation
# -------------------------------

struct TypeLevel{T}
  value::T
  TypeLevel(value) = new{Core.Typeof(value)}(value)
end
function Base.show(io::IO, x::TypeLevel)
  print(io, "TypeLevel($(repr(x.value)))")
end

mark_as_typelevel(several::Tuple) = mark_as_typelevel.(several)
mark_as_typelevel(single::TypeLevel) = single
mark_as_typelevel(single) = TypeLevel(single)

typify_args(args::Vararg{<:Any, 0}) = args
typify_args(first::TypeLevel, rest...) = (first.value, typify_args(rest...)...)
typify_args(first, rest...) = (Core.Typeof(first), typify_args(rest...)...)

extract_type(a::TypeLevel) = a.value
extract_type(a::Tuple) = extract_type.(a)
extract_type(a) = Core.Typeof(a)


function new_out(type::TypeLevel)
  # if we already receive a type-level, we need to strap of one layer
  isdefined(type.value, :parameters) && length(type.value.parameters) == 1 || error("expected singleton `Type{MyType}`, but got `$(type.value)`")
  mark_as_typelevel(type.value.parameters[1])
end
# if we receive value-level, we can directly treat it as type-level
new_out(type) = mark_as_typelevel(type)

function Out(sigtype::Type{<:Tuple})
  hassignature(sigtype) || Core.println("NOTAPPLICABLE: returning NotApplicable as couldn't find signature $sigtype")
  hassignature(sigtype) || return NotApplicable
  ir = IR(sigtype.parameters...)
  println(sigtype.parameters)
  println(ir)

  isnothing(ir) && error("NOTAPPLICABLE: Encountered signature type with no IR (intermediate representation), please overwrite IsDef.Out respectively")
  # replace functioncalls with calls to _Out_TypeLevel
  recurse!(ir, _Out_TypeLevel)
  # replace new with the first argument (i.e. the type going to be constructed)
  ir = MacroTools.prewalk(ir) do x
    isexpr(x, :new) ? Expr(:call, new_out, x.args[1]) : x
  end
  # wrap type arguments in our own wrapper to indicate this is already type-level
  result = IRTools.evalir(ir, TypeLevel.(sigtype.parameters)...)
  extract_type(result)
end

function _Out_TypeLevel(sigargs...)
  Core.println("sigargs = $(sigargs)")
  sigtypeargs = typify_args(sigargs...)  # TODO getfield(tuple, 1) is an example where it would make sense to overwrite Out for argument version
  Core.println("sigtypeargs = $(sigtypeargs)")
  if sigtypeargs[1] === Core.IntrinsicFunction
    # TODO improve this errormessage such that it is raised at the correct place
    error("""Recursed to the Core.IntrinsicFunction $(sigargs[1]),
      please overload `Out` for whatever function called this Core.IntrinsicFunction.
      It is unfortunately not possible to dispatch on a specific intrinsic function,
      as the typeof all intrinsic functions is `Core.IntrinsicFunction`
    """)
  end
  all(isapplicable, sigtypeargs) || Core.println("NOTAPPLICABLE: found NotApplicable in args, also returning NotApplicable")
  all(isapplicable, sigtypeargs) || return TypeLevel(NotApplicable)
  sigtype = Tuple{sigtypeargs...}
  Core.println("sigtype = $sigtype")
  result = Out(sigtype)
  Core.println("out = $result")
  mark_as_typelevel(result)
end