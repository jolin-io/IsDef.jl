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
    # check whether code generation is used
    sourcefile = string(which(Out, Tuple{Type{input}}).file)
    if contains(sourcefile, "dynamo.jl") || contains(sourcefile, "generic.jl")  # IRTools dynamo currently changes the source pointer towards inside IRTools itself.
        # here we use code generation and print it in advance
        println(DARK_GRAY_FG("Falling back to code generation."))
        println(DARK_GRAY_FG("IsDef._Out_implementation(input) = $(IsDef._Out_implementation(input))"))
        println()
    else
        println(DARK_GRAY_FG("Using custom inference rule."))
    end
    print(DARK_GRAY_FG("@code_warntype Out(input) = "))
    @code_warntype Out(input)
end

function Cassette.posthook(::DebugCtx, output, ::typeof(Out), ::Type{Input}) where Input<:Tuple
    @show output
    Core.println()
end

function isdef_debug(args...; kwargs...)
    Cassette.overdub(DebugCtx(), () -> isdef(args...; kwargs...))
end

function Out_debug(args...; kwargs...)
    Cassette.overdub(DebugCtx(), () -> Out(args...; kwargs...))
end

end  # module
