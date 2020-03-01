module Utils
export leaftypes, Type2Union, split_unionall, apply_typedetails_and_recurse, NewType
import InteractiveUtils

function leaftypes(T)
  subtypes = InteractiveUtils.subtypes(T)
  if isempty(subtypes)
    [T]
  else
    vcat((leaftypes(S) for S in subtypes)...)
  end
end

Type2Union(T) = Union{leaftypes(T)...}
Type2Union(T::Type{Any}) = NewType  # only Any is dealed with as open world

struct NewType end


# ----------------------------------------------------------------------

split_unionall(T) = (T, [])
function split_unionall(T::UnionAll)
  S, vars = split_unionall(T.body)
  S, [T.var; vars]
end

function Base.deepcopy_internal(T::UnionAll, dict::IdDict)
  base, wheres = split_unionall(T)
  wheres_dc = [Base.deepcopy_internal(w, dict) for w in wheres]

  type = base.name.wrapper
  typeparameters = base.parameters
  typeparameters_dc = [Base.deepcopy_internal(p, dict) for p in typeparameters]
  unionall = type{typeparameters_dc...}
  for w in wheres_dc
    unionall = UnionAll(w, unionall)
  end
  unionall
end

#= currently not needed because we use deepcopy

typecopy(value) = Base.copy(value)
typecopy(tv::TypeVar) = TypeVar(tv.name, tv.lb, tv.ub)
function typecopy(T::Union)
  Union{copy(T.a), copy(T.b)}
end
function typecopy(T::Type)
  T
end
function typecopy(T::UnionAll)
  dict = IdDict()
  base, wheres = split_unionall(T)
  wheres′ = similar(wheres)
  for i in eachindex(wheres)
    w = wheres[i]
    c = typecopy(w)
    wheres′[i] = c
    dict[w] = c
  end

  type = base.name.wrapper
  typeparameters = collect(base.parameters)
  typeparameters′ = similar(typeparameters)
  for i in eachindex(typeparameters)
    p = typeparameters[i]
    if haskey(dict, p)
      typeparameters′[i] = dict[p]
    else
      c = typecopy(p)
      dict[p] = c
      typeparameters′[i] = c
    end
  end

  unionall = type{typeparameters′...}
  for w in wheres′
    unionall = UnionAll(w, unionall)
  end
  unionall
end
=#

function apply_typeparameters_and_unionall(target, original)
  original′ = deepcopy(original)
  base, wheres = split_unionall(original′)
  copiedover_parameters = target{base.parameters...}
  unionall = copiedover_parameters
  for w in wheres
    unionall = UnionAll(w, unionall)
  end
  unionall
end
const apply_typedetails = apply_typeparameters_and_unionall


# TODO could be speed up with Continuables
findall_typevariables(tv::TypeVar) = [tv]
findall_typevariables(T::Type) = findall_typevariables(T.parameters)
findall_typevariables(T::Union) = [findall_typevariables(T.a); findall_typevariables(T.b)]
findall_typevariables(T::UnionAll) = findall_typevariables(Base.unwrap_unionall(T))  # the typevariables in where are also in the parameters
findall_typevariables(parameters::Core.SimpleVector) = vcat((findall_typevariables(p) for p in parameters)...)
findall_typevariables(any) = []

function apply_typedetails_and_recurse(target, original, recurse_function)
  target_with_typedetails = apply_typedetails(target, original)

  typevariables = findall_typevariables(target_with_typedetails)
  # recurse and change inplace as we now have a copy on this level
  for tv in typevariables
    # tv.lb = recurse_function(tv.lb)  # TODO we still don't know how to deal with lowerbounds properly
    tv.ub = recurse_function(tv.ub)
  end
  target_with_typedetails
end

supertypes(::Type{Any}) = [Any]
supertypes(T::Type) = [T; supertypes(supertype(T))]

end # module
