module Debug
export Out_debug, isdef_debug

using Cassette
using IsDef: IsDef, Out, isdef
using Crayons.Box
using InteractiveUtils: @code_warntype

Cassette.@context DebugCtx

function Cassette.prehook(::DebugCtx, ::typeof(Out), ::Type{input}) where input<:Tuple
    @show input
    println()
    println(DARK_GRAY_FG("IsDef._Out_implementation(input) = $(IsDef._Out_implementation(input))"))
    println()
    print(DARK_GRAY_FG("@code_warntype Out(input) = "))
    @code_warntype Out(input)
end

function Cassette.posthook(::DebugCtx, output, ::typeof(Out), ::Type{Input}) where Input<:Tuple
    @show output
end

function isdef_debug(args...; kwargs...)
    Cassette.overdub(DebugCtx(), () -> isdef(args...; kwargs...))
end

function Out_debug(args...; kwargs...)
    Cassette.overdub(DebugCtx(), () -> Out(args...; kwargs...))
end

end  # module
