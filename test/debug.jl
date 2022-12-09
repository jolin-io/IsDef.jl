using IsDef
using IsDef: TypeLevel
using Test
using JET
using Cthulhu

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

args = (ifelse2, true, TypeLevel(Int64), TypeLevel(String))
args′ = map(IsDef.dynamointernals_innervalue_to_types, args)
tuple_type = IsDef.Tuple_value_to_type(args′)


# Inspecting inference
# ====================

Out(tuple_type)

# Inferred fails
@inferred Out(tuple_type)

# code_warntype succedes :D
@code_warntype Out(tuple_type)

# JET needs to run at the commandline
@report_opt Out(tuple_type)
# tells that
@inferred Val{IsDef._dynamointernals_innervalue_to_types_ANDTHEN_Out_ANDTHEN_dynamointernals_ensure_innervalue(!, false)}()

@report_call Out(tuple_type)

# Cthulhu tells that a recursion was detected which decreased the inference performance
@descend g()  # needs to be run on the command line

@descend_code_warntype g()