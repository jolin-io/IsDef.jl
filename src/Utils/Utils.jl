module Utils
using Reexport

include("TricksAdapted.jl")
@reexport using .TricksAdapted

include("TypeUtils.jl")
@reexport using .TypeUtils

include("ValTypes.jl")
@reexport using .ValTypes

include("CoreReturnType.jl")
@reexport using .CoreReturnType

include("IRToolsUtils.jl")
@reexport using .IRToolsUtils

end