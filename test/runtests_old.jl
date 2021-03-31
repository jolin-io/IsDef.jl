using IsDef
using Test
using Documenter
import FunctionWrappers: FunctionWrapper

@test isempty(detect_ambiguities(IsDef))

macro test_everything()
  quote

    # test apply
    # ----------

    @test apply(reduce, +, [2,3], init=1) == reduce(+, [2,3], init=1)

    # test ∨ \vee
    # -----------

    @test ∨(1, 2.0) == ∨(typeof(1), typeof(2.0)) == promote_type(typeof(1), typeof(2.0))

    # test isdef/Out
    # --------------

    @test IsDef.Out(Base.map, typeof(x->2x), Vector{Int}) == Vector{Int}
    @test isdef(Base.reduce, typeof(+), Vector{<:Number})

    # Caution!! Core.Compiler.return_type is actually not always as good as expected:
    # this by now actually inferes correctly...
    # Base.promote_op(Base.reduce, typeof(+), Vector{String}) === Union{}


    # works transparent with wrappers (unlike Base.`which`)
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


    # test types
    @test Out(Some, Int) == Some{Int}
    @test isdef(Some, Int)

    # test values
    @test Out(sin, 1) == Float64
    @test isdef(sin, 1)


    # test inference
    # --------------

    @test Base.promote_op((args...) -> Val(isdef(args...)), typeof(sin), Int) == Val{true}

    using IsDef
    using Test
    mywrapper(args...) = myfunc(args...)
    myfunc(::BigFloat) = "big"
    myfunc(::Float16) = 16
    myfunc(::Float32) = 32
    @test Base.promote_op((args...) -> Val(isdef(args...)), typeof(mywrapper), Float64) == Val{false}
    @test Base.promote_op((args...) -> Val(isdef(args...)), typeof(mywrapper), BigFloat) == Val{true}
    @test Base.promote_op((args...) -> Val(isdef(args...)), typeof(mywrapper), Float16) == Val{true}
    @test Base.promote_op((args...) -> Val(isdef(args...)), typeof(mywrapper), Float32) == Val{true}

    # TODO unfortunately type inference does not work through Union, which is surprising
    # inspecting the issue further it turns out that type-inference is extremely unstable, putting things into functions
    # instead of doing them at the REPL may influence type-inference to the worse...
    # better not to do much with type-inference

    # @test Base.promote_op((args...) -> Val(isdef(args...)), typeof(mywrapper), AbstractFloat) == Val{false}



    # test documentation
    # ------------------

    doctest(IsDef)
    @test Out(Base.map, typeof(string), Vector{Int}) == Vector{String}
    @test Out(Base.map, typeof(isodd), Vector{Int}) == Vector{Bool}
    @test Out(Base.map, FunctionWrapper{Bool, Tuple{Any}}, Vector{Int}) == Vector{Bool}

  end # quote
end # macro

@test_everything


# check that if we redefine the generated functions of the module, nothing breaks
# ----------------------------

IsDef.@redefine_generated
@test_everything
