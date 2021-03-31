
function hassignature(sigtype; world=typemax(UInt))
  if sigtype.parameters[1] <: Core.Builtin
    error("Recursed to builtin function `$(sigtype.parameters[1])`, please overwrite `IsDef.Out` for the builtin or for a previous function.")  
  end
  result = @ccall jl_gf_invoke_lookup(sigtype::Any, world::UInt)::Any
  result !== nothing
end
  