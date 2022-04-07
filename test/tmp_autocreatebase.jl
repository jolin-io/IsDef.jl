using IsDef
using InteractiveUtils: supertypes, subtypes

# Autogenerate Base by using Base.promote_op

exclude_list = [
    :(!=)
    :(==)
    :(!==)
    :(===)
    :(!)
    :(>=)
    :(<=)
    :(<)
    :(>)
]

moduleof(T) = Base.unwrap_unionall(T).name.module
moduleof(::Core.TypeofVararg) = Core
"""
```julia
julia> subtypes(Type{Signed})
1-element Vector{Any}:
 Type{Signed}
```
"""


function nonabstract_subtypes(T)
    if myisabstracttype(T)
        all_subtypes = mysubtypes(T)
        vcat((nonabstract_subtypes(ST) for ST in all_subtypes)...)
    else
        Type[T]
    end
end

myisabstracttype(::Type{Type{T}}) where T = false
myisabstracttype(other) = isabstracttype(other)

function mysubtypes(type::UnionAll)
    base, typevars = IsDef.split_typevar(type)
    if base.name.name === :Type
        typevar = only(typevars)
        types = types_lb_ub(typevar.lb, typevar.ub)
        return [Type{T} for T in types]
    else
        return subtypes(type)
    end
end

function mysubtypes(type)
    subtypes(type)
end

subtypes_all(ub) = types_lb_ub(Union{}, ub)

function types_lb_ub(lb, ub) # lb !== Union{}
    supertys = supertypes(lb)
    return filter(supertys) do st
        st <: ub && st != ub
    end
end

function types_lb_ub(lb::Type{Union{}}, ub)
    acc! = Type[]
    subtypes_all!(ub, acc!)
    return acc!
end

function subtypes_all!(ub, acc!)
    for type in subtypes(ub)
        if type <: ub && type != ub
            push!(acc!, type)
            subtypes_all!(type, acc!)
        end
    end
end


nonabstract_subtypes(Type{<:Integer})

exprs = Expr[]
for name in names(Base)
    name ∉ exclude_list || continue
    func = getfield(Base, name)
    isa(func, Function) || continue

    for m in methods(func)
        m.module === Base || continue
        tupletype = Base.unwrap_unionall(m.sig)
        @show m.sig

        
        for p in tupletype.parameters
            if p isa TypeVar && p.lb !== Union{}
                @warn "found lowerbound $(p.lb), however we do not support lowerbounds at the moment"
            end
        end
        
        upperbounds = map(tupletype.parameters) do p
            if isa(p, Core.TypeofVararg)
                # special treatment of Vararg
                p = p.T
            end
            isa(p, TypeVar) ? p.ub : Base.rewrap_unionall(p, m.sig)
        end
        
        if Any ∈ upperbounds || Function ∈ upperbounds || Module ∈ upperbounds || Type ∈ upperbounds
            @error "found Any or Function or Module or Type in signature $(m.sig)"
            continue
        end

        types = map(upperbounds) do ub
            ubs = isa(ub, Union) ? Base.uniontypes(ub) : Type[ub]
            subtypes = map(nonabstract_subtypes, ubs)
            all_subtypes = vcat(subtypes...)
            concrete_types_in_Base = filter(all_subtypes) do st
                moduleof(st) ∈ (Core, Base)
            end
            Expr(:curly, Union, concrete_types_in_Base...)
        end

        typevariables = [Symbol(:T, i) for i in 1:length(upperbounds)]
        typebounds = map(typevariables, types) do tv, ub
            Expr(:(<:), tv, ub)
        end
        sig_parameters = map(typevariables, tupletype.parameters) do tv, p
            # special treatment of Vararg
            if isa(p, Core.TypeofVararg)
                isdefined(p, :N) ? Vararg{tv, p.N} : Vararg{tv}
            else
                tv
            end
        end
        sig = Tuple{sig_parameters...}

        args_sig_parameters = map(typevariables, tupletype.parameters) do tv, p
            # special treatment of Vararg
            tv_normal_type = Expr(:call, IsDef.promote_type_or_val, tv)
            if isa(p, Core.TypeofVararg)
                isdefined(p, :N) ? Expr(:curly, Vararg, tv_normal_type, p.N) : Expr(:curly, Vararg, tv_normal_type)
            else
                tv_normal_type
            end
        end
        args_sig = Expr(:curly, Tuple, args_sig_parameters...)

        push!(exprs, quote
            function IsDef.Out(::Type{$sig}) where {$(typebounds...)}
                Core.Compiler.return_type($apply, $args_sig)
            end
        end)
    end
end
