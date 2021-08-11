struct Wrapper{T}
    value::T
end

function g()
    a = WrapType(Int)
    a.value
end

function f()
    a = Wrapper(Int)
    a.value
end

function g2()
    Wrapper(Int)
end

function g1(f)
    a = g2()
    a.value
end
Wrapper(Int)  # Wrapper{DataType}(Int64)
Base.promote_op(f)  # Type{Int64}


using IRTools
import IRTools: xcall, var
using IsDef


function showall(args...; kwargs...)
    @show args kwargs
    return args, kwargs
end

function f(a; k = 1)
    showall(a, keyword = k, key2 = "hi")
end
ir = @code_ir f([1,2])


# Experiment 1
# ------------

IsDef.shortcycle_after_var_if_condition!(ir, var(7), true) do shortcycle_block
    IRTools.return!(shortcycle_block, var(7))
end
kwfunc = IRTools.func(ir)(f, [1,2])
typeof(kwfunc)
typeof(typeof(kwfunc))

# Experiment 2
# ------------
ir[var(8)] = xcall(showall, ir[var(8)].expr.args...)
ir

args, kwargs = IRTools.func(ir)(f, [1,2])
# documented in the official docs https://docs.julialang.org/en/v1/devdocs/functions/#Keyword-arguments
args[1] === typeof(args[3]).name.mt.kwsorter
typeof(args[1]) === Core.kwftype(typeof(args[3]))


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

ir = @code_ir ifelse2(true, 1, 2); branchblock = IRTools.block(ir, 1)
IsDef.copy_all_successors!(branchblock)
ir

function prewalk!(func, e::Expr)
    e′ = func(e) 
    for (i, arg) in enumerate(e′.args)
        e′[i] = prewalk!(func, arg)
    end
    e′
end

function prewalk!(func, v::Vector)
    v′ = func(v)
    for (i, element) in enumerate(v′)
        v′[i] = prewalk!(func, element)
    end
    v′
end

function prewalk!(func, quotenode::QuoteNode)
    QuoteNode(func(quotenode.value))
end

function prewalk!(func, other)
    func(v)
end

