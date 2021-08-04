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

IRTools.meta

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