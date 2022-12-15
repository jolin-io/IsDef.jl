module TypeUtils

export split_typevar
export kwftype
export signature_split_first, signature_add_first
export Tuple_type_to_value, Tuple_value_to_type
export NamedTuple_type_to_value, NamedTuple_value_to_type, NamedTupleEmpty
export IntrinsicFunction
export isleaf_type

split_typevar(base) = base, TypeVar[]
function split_typevar(t::UnionAll)
    base, typevars = split_typevar(t.body)
    base, [t.var; typevars...]
end


kwftype(f::Function) = kwftype(typeof(f))
kwftype(::Type{F}) where F <: Function = Core.kwftype(F)


function signature_split_first(::Type{T}) where {T<:Tuple}
    T_unwrap = Base.unwrap_unionall(T)
    func, args... = tuple(T_unwrap.parameters...)
    Base.rewrap_unionall(func, T), Base.rewrap_unionall(Tuple{args...}, T)
end
function signature_add_first(::F, ::Type{T}) where {F, T<:Tuple}
    T_unwrap = Base.unwrap_unionall(T)
    Base.rewrap_unionall(Tuple{F, T_unwrap.parameters...}, T)
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


# TODO still needed?
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
                _isleaf(type) -> Bool

check whether the type does not need further recurse.
If the type belongs to Core or Base it is considered a leaf.

In order to define an exception, _isleaf would need to be overwritten.
"""
function isleaf_type(type)
    mod = Base.typename(type).module
    return isleaf_type(mod)
end

function isleaf_type(::Type{Core.Const})
    Core.println(Core.stderr, "WARNING: Found Core.Const and regarding it as leaf type is probably wrong")
    true
end

function isleaf_type(mod::Module)
    mod ∈ (Base, Core) && return true
    parent = Base.parentmodule(mod)
    mod === parent && return false
    return isleaf_type(parent)
end

function isleaf_type(uniontype::Union)
    isleaf_type(uniontype.a) && isleaf_type(uniontype.b)
end

end