using IsDef
using Test
using Documenter
import FunctionWrappers: FunctionWrapper

@test isempty(detect_ambiguities(IsDef))

# test apply
# ----------

@test apply(reduce, +, [2,3], init=1) == reduce(+, [2,3], init=1)


# test small anonymous functions
# ------------------------------

@test @inferred(Out(x -> 2x, Int)) == Int
@test @inferred(Out(x -> 2.3x, Int)) == Float64

# test ifelse
# -----------

function ifelse2(t, a, b)
    println("before")
    t = !t
    if t
        println("a = $a")
        a
    else
        println("b = $b")
        b
    end
end

@test @inferred(Out(ifelse2, Bool, Int, String)) == Union{Int, String}
@test @inferred(Out((a, b) -> ifelse2(false, a, b), Int, String)) == Int
@test @inferred(Out((a, b) -> ifelse2(true, a, b), Int, String)) == String


function forloop_simple(a, b)
    sum = 0
    for i in 1:a
        sum += i
    end
    return a, b
end

@test @inferred(Out(forloop_simple, Int, String)) == Tuple{Int, String}

function forloop_complex(a, b)
    h = nothing
    for i in 1:5
        if isodd(i)
            h = a
        else
            h = b
        end
    end
    h
end

@test @inferred(Out(forloop_complex, Int, String)) == Union{Int, String}


@test @inferred(Out(Main.:(:), Int, Int)) == UnitRange{Int64}
@test @inferred(Out(() -> 1:2)) == ValTypeof(1:2)
@test @inferred(Out(Tuple{typeof(Main.:(:)), ValTypeof(1), ValTypeof(2)})) == ValTypeof(1:2)

@test @inferred(Out(Base.map, typeof(x->2x), Vector{Int})) == Vector{Int}
@test @inferred(Out(Base.map, typeof(x->2.5x), Vector{Int})) == Vector{Float64}

# works transparent with wrappers (unlike Base.`which`)
wrapper(args...; kwargs...) = original(args...; kwargs...)
original(a::Int, b::String) = true

@test @inferred(Out(original, Int, String)) == ValTypeof(true)

@test Out(wrapper, Int, String) == ValTypeof(true)
@test @inferred(Out(wrapper, Int, String)) == ValTypeof(true)

@test isdef(wrapper, Int, String)
@test !isdef(wrapper, Float64)


# works in a strict open sense only with Any (would have to work for a whole newtype)
# for everything else the concrete leave-types are used
f(a) = a + a
# we decided to leave Any as is, not going to newtype, and hence Any says often yes instead of no
@test @inferred(Out(f, Any)) == NotApplicable
@test !isdef(f, Any)

@test @inferred(Out(f, Number)) == UnsureWhetherApplicable
@test !isdef(f, Number)

@test @inferred(Out(f, Integer)) == UnsureWhetherApplicable
@test !isdef(f, Integer)

@test @inferred(Out(f, String)) == NotApplicable
@test !isdef(f, String)

@test @inferred(Out(f, AbstractString)) == NotApplicable
@test !isdef(f, AbstractString)

@test @inferred(Out(f, Vector{<:String})) == UnsureWhetherApplicable
@test !isdef(f, Vector{<:String})

@test @inferred(Out(f, Vector{<:AbstractString})) == UnsureWhetherApplicable
@test !isdef(f, Vector{<:AbstractString})


@test @inferred(Out(Base.map, typeof(x -> x+4), Array{<:Number, 3})) == UnsureWhetherApplicable
@test !isdef(Base.map, typeof(x -> x+4), Array{<:Number, 3})

@test @inferred(Out(Base.map, typeof(x -> x+4), Vector{String})) == NotApplicable
@test !isdef(Base.map, typeof(x -> x+4), Vector{String})

@test @inferred(Out(Base.map, typeof(x -> x+4), Vector{AbstractString})) == NotApplicable
@test !isdef(Base.map, typeof(x -> x+4), Vector{AbstractString})

@test @inferred(Out(Base.map, typeof(x -> x+4), Vector{<:AbstractString})) == UnsureWhetherApplicable
@test !isdef(Base.map, typeof(x -> x+4), Vector{<:AbstractString})


# test types
@test @inferred(Out(Some, Int)) == Some{Int}
@test isdef(Some, Int)

# test values
@test @inferred(Out(sin, ValTypeof(1))) <: ValType{Float64}
@test isdef(sin, 1)


# test inference on isdef
# -----------------------

@test Base.promote_op((args...) -> Val(isdef(args...)), typeof(sin), Int) == Val{true}

mywrapper(args...) = myfunc(args...)
myfunc(::BigFloat) = "big"
myfunc(::Float16) = 16
myfunc(::Float32) = 32
@test Base.promote_op((args...) -> Val(isdef(args...)), typeof(mywrapper), Float64) == Val{false}
@test Base.promote_op((args...) -> Val(isdef(args...)), typeof(mywrapper), BigFloat) == Val{true}
@test Base.promote_op((args...) -> Val(isdef(args...)), typeof(mywrapper), Float16) == Val{true}
@test Base.promote_op((args...) -> Val(isdef(args...)), typeof(mywrapper), Float32) == Val{true}
@test Base.promote_op((args...) -> Val(isdef(args...)), typeof(mywrapper), AbstractFloat) == Val


# test documentation
# ------------------

@test Out(Base.map, typeof(string), Vector{Int}) == Vector{String}
@test Out(Base.map, typeof(isodd), Vector{Int}) == Vector{Bool}
@test Out(Base.map, FunctionWrapper{Bool, Tuple{Any}}, Vector{Int}) == Vector{Bool}

doctest(IsDef)
