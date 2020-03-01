using IsDef
using Test

@test Out(Base.map, typeof(x->2x), Vector{Int}) == Vector{Int}
@test isdef(Base.reduce, typeof(+), Vector{<:Number})

# works transparent with wrappers (unlike Base.``which``)
wrapper(args...; kwargs...) = original(args...; kwargs...)
original(a::Int, b::String) = true
@test isdef(wrapper, Int, String)
@test !isdef(wrapper, Float64)

# does even work on compiler level
@test Out(Out, typeof(Base.map), typeof(x->2x), Vector{Int}) == Type{Vector{Int}}

# works in a strict sense with abstracttypes
# concretely the function has to be defined for the abstracttype to count
f(a) = a + a
@test !isdef(f, Any)
@test isdef(f, Number)
@test isdef(f, Integer)
