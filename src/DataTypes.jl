module DataTypes
export NotApplicable, TypeLevelFunction, IntrinsicFunction


# we decided to use type-level because `Out` normally returns types.
# hence it is more intuitive to use type, but also more natural, as `Out` may be used within type range as `Out(f, ...) <: NotApplicable`.
struct NotApplicable
  NotApplicable() = error("Please use `NotApplicable` type instead of `NotApplicable()` instance.")
end

isapplicable(::Type{NotApplicable}) = false
isapplicable(other) = true


struct TypeLevelFunction{F} <: Function
  func::F
end

end