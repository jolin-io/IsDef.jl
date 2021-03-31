using Cassette
using Base: @ccall
Cassette.@context IsDefCtx

isdef(f, args::Type...) = isdef(IsDefCtx(), f, args...)
function Cassette.overdub(ctx::IsDefCtx, f, args...)
  args_types = [a isa Type ? a : typeof(a) for a in args]
  isdef(f, args_types...)
end
function isdef(ctx, f, args::Type...)

  if Cassette.canrecurse(ctx, f, args...)
    Cassette.recurse(ctx, f, args...)
  else
    error("Found primite function $f($(args...)) for which `isdef` cannot be inferred automatically. Implement `IsDef.isdef` manually instead.")
  end
end 

isdef(::typeof(+), ::Type{T}...) where {T} = T
isdef(::typeof(*), ::Type{T}...) where {T} = T

isdef((a, b) -> a*a + f(a) + b, Float32, Int)


function f(a)
  2*a + a
end




f(x) = 3x*x + 2x + 1

IR(typeof(f), Int)
@code_ir f(1)

apply(f, args...; kwargs...) = f(args...; kwargs...) 

struct TypeLevel{T}
  value::T
  TypeLevel(value) = new{Core.Typeof(value)}(value)
end

typearg_args(args::Vararg{<:Any, 0}) = args
typearg_args(first::TypeLevel, rest...) = (first, typearg_args(rest...)...)
typearg_args(first, rest...) = (TypeLevel(Core.Typeof(first)), typearg_args(rest...)...)

typify_args(args::Vararg{<:Any, 0}) = args
typify_args(first::TypeLevel, rest...) = (first.value, typify_args(rest...)...)
typify_args(first, rest...) = (Core.Typeof(first), typify_args(rest...)...)


typify_args(1, 3)
typify_args(1, TypeLevel(Int))
Out(f, args)


function hassignature(sigtype; world=typemax(UInt))
  result = @ccall jl_gf_invoke_lookup(sigtype::Any, world::UInt)::Any
  result !== nothing
end

hassignature(Tuple{typeof(f), Int})
hassignature(Tuple{typeof(f), String, Int})

struct NotApplicable end

anynotapplicable(first::Type{NotApplicable}, rest...) = true
anynotapplicable(first, rest...) = anynotapplicable(rest...)
anynotapplicable(first, rest::Vararg{Any, 0}) = false

anynotapplicable(NotApplicable)
anynotapplicable(Int, NotApplicable)
anynotapplicable(1, 3, Int)

Out(f, args...) = Out(f, typeof.(args)...)
function Out(f, types::Vararg{<:Type})
  Out(apply, Core.Typeof(f), types...)
end
function Out(::typeof(apply), types::Vararg{<:Type})
  signature_type = Tuple{types...}
  Out(signature_type)
end


function Out(sigtype::Type{<:Tuple})
  hassignature(sigtype) || Core.println("NOTAPPLICABLE: returning NotApplicable as couldn't find signature $sigtype")
  hassignature(sigtype) || return NotApplicable
  ir = IR(sigtype.parameters...)
  ir === nothing && error("NOTAPPLICABLE: Encountered signature type with no IR (intermediate representation), please overwrite IsDef.Out respectively")
  recurse!(ir, _Out_TypeLevel)
  # wrap type arguments in our own wrapper to indicate this is already type-level
  return IRTools.evalir(ir, TypeLevel.(sigtype.parameters)...)
end

function _Out_TypeLevel(sigargs...)
  Core.println("sigargs = $(sigargs)")
  sigtypeargs = typify_args(sigargs...)
  Core.println("sigtypeargs = $(sigtypeargs)")
  if sigtypeargs[1] === Core.IntrinsicFunction
    # TODO improve this errormessage such that it is raised at the correct place
    error("""Recursed to the Core.IntrinsicFunction $(sigargs[1]),
      please overload `Out` for whatever function called this Core.IntrinsicFunction.
      It is unfortunately not possible to dispatch on a specific intrinsic function,
      as the typeof all intrinsic functions is `Core.IntrinsicFunction`
    """)
  end
  anynotapplicable(sigtypeargs...) && Core.println("NOTAPPLICABLE: found NotApplicable in args, also returning NotApplicable")
  anynotapplicable(sigtypeargs...) && return TypeLevel(NotApplicable)
  sigtype = Tuple{sigtypeargs...}
  Core.println("sigtype = $sigtype")
  result = Out(sigtype)
  Core.println("out = $result")
  mark_as_typelevel(result)
end

mark_as_typelevel(several::Tuple) = mark_as_typelevel.(several)
mark_as_typelevel(single::TypeLevel) = single
mark_as_typelevel(single) = TypeLevel(single)



f(x) = 3x*x + 2x
Out(f, 1)
Out(f, 2.0)
