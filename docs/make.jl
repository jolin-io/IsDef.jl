using IsDef
using Documenter

makedocs(;
    modules=[IsDef],
    authors="Stephan Sahm <stephan.sahm@gmx.de> and contributors",
    repo="https://github.com/jolin-io/IsDef.jl/blob/{commit}{path}#L{line}",
    sitename="IsDef.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://jolin-io.github.io/IsDef.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
        "Manual" => "manual.md",
        "Library" => "library.md",
    ],
)

deploydocs(;
    repo="github.com/jolin-io/IsDef.jl",
    devbranch="main",
)
