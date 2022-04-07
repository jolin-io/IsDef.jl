using IsDef.Utils.TypeUtils: IntrinsicFunction
using IsDef.Utils.TypeValues: VAL

# bitcast
# -------

function Out(::Type{Tuple{IntrinsicFunction{Core.bitcast}, Type{T1}, T2}}) where {T1, T2}
    T1
end

# arraylen
# --------

function Out(::Type{Tuple{IntrinsicFunction{Core.Intrinsics.arraylen}, T}}) where {T}
    Int
end

# ===
# ---

function Out(::Type{Tuple{typeof(===), VAL{T, V}, VAL{T, V}}}) where {T, V}
    true
end
function Out(::Type{Tuple{typeof(===), T1, T2}}) where {T1, T2}
    false
end