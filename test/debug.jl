using IsDef

function applyinnerfunc(a, b)
    sum = 0
    for i in 1:a
        sum += i
    end
    return a, b
end

IsDef.Out_implementation(Tuple{typeof(applyinnerfunc), Int, String})


