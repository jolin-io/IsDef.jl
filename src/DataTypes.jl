module DataTypes
export ApplicabilityProblem, NotApplicable, UnsureWhetherApplicable, TypeValueFunction, IntrinsicFunction

abstract type ApplicabilityProblem end
Base.instances(::Type{ApplicabilityProblem}) = (NotApplicable, UnsureWhetherApplicable)

# we decided to use type-level because `Out` normally returns types.
# hence it is more intuitive to use type, but also more natural, as `Out` may be used within type range as `Out(f, ...) <: NotApplicable`.
struct NotApplicable <: ApplicabilityProblem
    NotApplicable() = error("Please use `NotApplicable` type instead of `NotApplicable()` instance.")
end

struct UnsureWhetherApplicable <: ApplicabilityProblem
    UnsureWhetherApplicable() = error("Please use `UnsureWhetherApplicable` type instead of `UnsureWhetherApplicable()` instance.")
end


isapplicable(::Type{NotApplicable}) = false
isapplicable(::Type{UnsureWhetherApplicable}) = false
isapplicable(other) = true


struct TypeValueFunction{F} <: Function
    func::F
end

end