module Utils
export compare_expr

compare_expr(expr1, expr2) = expr1 == expr2
compare_expr(expr1::Type, expr2::Symbol) = Symbol(expr1) == expr2
compare_expr(expr1::Symbol, expr2::Type) = expr1 == Symbol(expr2)
function compare_expr(expr1::Symbol, expr2::Symbol)
  if startswith(string(expr1), "##") && startswith(string(expr2), "##")
    # dummy variables should compare true
    true
  else
    expr1 == expr2
  end
end
compare_expr(::LineNumberNode, ::LineNumberNode) = true

function next_expr(iter, state...)
  next = iterate(iter, state...)
  while !isnothing(next) && isa(next[1], LineNumberNode)
    next = iterate(iter, next[2])
  end
  next
end
function compare_expr(expr1::Vector, expr2::Vector)
  # this implementation skips linenumbernodes entirely,
  # i.e. there can be different amounts of linenumbernodes on each side
  next1 = next_expr(expr1)
  next2 = next_expr(expr2)
  while !isnothing(next1) && !isnothing(next1)
    value1, state1 = next1
    value2, state2 = next2
    compare_expr(value1, value2) || return false
    next1 = next_expr(expr1, state1)
    next2 = next_expr(expr2, state2)
  end
  # check whether both iterators returned nothing, i.e. both Vectors are empty now
  next1 == next2 || return false
  return true
end
function compare_expr(expr1::Expr, expr2::Expr)
  compare_expr(expr1.head, expr2.head) && compare_expr(expr1.args, expr2.args)
end

end # module
