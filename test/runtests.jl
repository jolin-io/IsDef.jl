using IsDef
using IRTools
using Test
# using Documenter
import FunctionWrappers: FunctionWrapper

# @test isempty(detect_ambiguities(IsDef))  # crashes whole Julia


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
    if t
        println("a = $a")
        a
    else
        println("b = $b")
        b
    end
end

@test Out(ifelse2, Bool, Int, String) == Union{Int, String}
@test Out(ifelse2, true, Int, String) == Int
@test Out(ifelse2, false, Int, String) == String

function gettoknowirtools(a, b)
    sum = 0
    for i in 1:a
        add = if isodd(i)
            5
        else
            9
        end
        sum += i + add
    end
    return a, b
end

function applyinnerfunc(a, b)
    sum = 0
    for i in 1:a
        sum += i
    end
    return a, b
end

using IsDef
exprs = Expr[]

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

Base.Vector

nonabstracttypes = Type[]
for name in names(Base)
    type = getfield(Base, name)
    isa(type, Type) || continue
    !isabstracttype(type) || continue
    push!(nonabstracttypes, type)
end


for name in names(Base)
    name ∉ exclude_list || continue
    func = getfield(Base, name)
    isa(func, Function) || continue

    for m in methods(func)
        m.module === Base || continue
        tupletype, typevars = IsDef.split_typevar(m.sig)
        if Any ∈ tupletype.parameters || Function ∈ tupletype.parameters
            @warn "found Any or Function in signature $(m.sig)"
        end

        if isempty(typevars)
            push!(exprs, quote
                function IsDef.Out(::Type{$tupletype})
                    Base.promote_op($apply, $(tupletype.parameters...))
                end
            end)
        
        else
            parameters_exprs = map(tupletype.parameters) do p
                if p ∈ typevars
                    Expr(:call, IsDef.promote_type_or_typevalue, p)
                else
                    p
                end
            end
            push!(exprs, quote
                function IsDef.Out(::Type{$tupletype}) where {$(typevars...)}
                    Base.promote_op($apply, $(parameters_exprs...))
                end
            end)
        end
    end
end
exprs[10]
length(exprs)
g(a::A, b::B) where A where B = a, b, A, B

IsDef.Out(::Type{Tuple{Base.Colon, A, B}}) where {A, B} = Base.promote_op(apply, Base.Colon, promote_type_or_typevalue(A), B)

Out(applyinnerfunc, Int, String)

IsDef.Out_implementation(Tuple{typeof(applyinnerfunc), Int, String})


using IRTools
ir = @code_ir applyinnerfunc(4, :hi)

IsDef.keep_only_what_is_explicitly_used!(ir)

IsDef.lift_ifelse!(ir)


ir = IsDef.Out_implementation(Tuple{typeof(applyinnerfunc), Int, String})

IRTools.evalir(ir, Tuple{}, applyinnerfunc, Bool, Int, String)

ir_original = @code_ir applyinnerfunc(1, 2)
@show ir_original

ir = @code_ir applyinnerfunc(1, 2)

IRTools.Inner.blockidx(ir, IRTools.Variable(8))
bs = IRTools.blocks(ir)


IsDef.lift_ifelse!(ir)




all_ifelse = Set{IRTools.Variable}()
block, state_block = iterate(IsDef.iterateblocks(ir))
block, state_block = iterate(IsDef.iterateblocks(ir), state_block)

branch, state_branch = iterate(IRTools.branches(block))
branch, state_branch = iterate(IRTools.branches(block), state_branch)

condition_var = branch.condition
# skip over unconditional or return branches
@show isnothing(condition_var)
# skip over already handled var
@show condition_var ∈ all_ifelse

haskey(ir, condition_var)

# it might be that the condition_var is just an argument and if so won't have an expression mapped to it
if haskey(ir, condition_var)
# skip over isbooltype conditions, they are safe as we introduce them right here
condition_statement = ir[condition_var]
using ExprParsers
@show EP.isexpr(condition_statement.expr, :call) && condition_statement.expr.args[1] === IsDef.isbooltype
end

# support Bool
IsDef.lift_ifelse!(ir, condition_var)
push!(all_ifelse, condition_var)
@debug "ifelse $(condition_var) - ir after = $ir"





# -----------


IsDef.inline_all_blocks_with_arguments!(IRTools.blocks(ir)[2:end])

IsDef.lift_ifelse!(ir)
ir



getval(t) ? a : b

@test Out(Base.map, typeof(x->2x), Vector{Int}) == Vector{Int}  # TODO continue with Out(typeof)

@test isdef(Base.reduce, typeof(+), Vector{<:Number})
Base.Generator

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
