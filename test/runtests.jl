using IsDef
using Test
using Documenter
import FunctionWrappers: FunctionWrapper

@test Out(Base.map, typeof(x->2x), Vector{Int}) == Vector{Int}
@test isdef(Base.reduce, typeof(+), Vector{<:Number})

# Caution!! Core.Compiler.return_type is actually not always as good as expected:
# this by now actually inferes correctly...
# Base.promote_op(Base.reduce, typeof(+), Vector{String}) === Union{}


# works transparent with wrappers (unlike Base.``which``)
wrapper(args...; kwargs...) = original(args...; kwargs...)
original(a::Int, b::String) = true
@test isdef(wrapper, Int, String)
@test !isdef(wrapper, Float64)


# does even work on compiler level
@test Out(Out, typeof(Base.map), typeof(x->2x), Vector{Int}) == Type{Vector{Int}}


# works in a strict open sense only with Any (would have to work for a whole newtype)
# for everything else the concrete leave-types are used
f(a) = a + a
# we decided to leave Any as is, not going to newtype, and hence Any says often yes instead of no
@test isdef(f, Any)
@test isdef(f, Number)
@test isdef(f, Integer)
@test !isdef(f, String)
@test !isdef(f, AbstractString)
@test !isdef(f, Vector{<:String})
@test !isdef(f, Vector{<:AbstractString})


@test isdef(Base.map, typeof(x -> x+4), Array{<:Number, 3})
@test !isdef(Base.map, typeof(x -> x+4), Vector{String})
@test !isdef(Base.map, typeof(x -> x+4), Vector{AbstractString})
@test !isdef(Base.map, typeof(x -> x+4), Vector{<:AbstractString})

# test documentation
doctest(IsDef)
@test Out(Base.map, typeof(string), Vector{Int}) == Vector{String}
@test Out(Base.map, typeof(isodd), Vector{Int}) == Vector{Bool}
@test Out(Base.map, FunctionWrapper{Bool, Tuple{Any}}, Vector{Int}) == Vector{Bool}
