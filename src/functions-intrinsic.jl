# bitcast
# -------

function Out(::Type{Tuple{IsDef.IntrinsicFunction{Core.bitcast}, Type{T1}, T2}}) where {T1, T2}
    T1
end

# arraylen
# --------

function Out(::Type{Tuple{IsDef.IntrinsicFunction{Core.Intrinsics.arraylen}, T}}) where {T}
    Int
end
