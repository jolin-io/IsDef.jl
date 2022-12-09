using IRTools: IRTools, @dynamo, IR, xcall, emptyargs!, arguments, block
using MacroTools: MacroTools, isexpr
using SimpleMatch: @match

using IsDef.Utils.TricksAdapted: static_hasmethod, covering_method_instances
using IsDef.DataTypes: NotApplicable, isapplicable, TypeValueFunction
using IsDef.Utils.TypeUtils: Tuple_type_to_value, Tuple_value_to_type, IntrinsicFunction, signature_split_first
using IsDef.Utils.TypeValues: ValType, ValTypeof, signature_without_typevalues
using IsDef.Utils.DynamoInternals: TypeLevel, dynamointernals_ensure_innervalue, dynamointernals_innervalue_to_types
using IsDef.Utils.IRToolsUtils: detect_cycle, keep_only_what_is_explicitly_used!, shortcycle_if_notapplicable!, lift_ifelse!, iterateblocks


# the functions need to be defined so that @revise works the first time
function Out end
function Out_implementation end

function _Out_dynamo end
function _Out_dynamo_implementation end

"""
because revise does not work well for generated functions and dynamo we provide a manual way to revise the implementation
"""
function revise!()
  IsDef.@eval begin
      @generated function Out(::Type{signature_typevalues}) where {signature_typevalues <: Tuple}
        # we have two versions of _Out_dynamo, one with the final dynamo, and one returning just the implementation itself
        # similar to macros, only that the dynamo needs to be compiled separately
        Out_implementation(signature_typevalues)
      end

      # @dynamo is similar to @generated, i.e. we get the type of the original arguments
      @dynamo function _Out_dynamo(::Type{signature_typevalues}, args...) where {signature_typevalues <: Tuple}
        _Out_dynamo_implementation(signature_typevalues, args...)
      end

      # define recursion_relation so that inference works better
      # we probably have problems with true infinite cases now, but that should be still
      # better in total
      for f in (Out, _Out_dynamo)
        for m in methods(f)
          m.recursion_relation = (@nospecialize(_...)) -> true
        end
      end
  end
end

revise!()


# Out
# ===

"""
this level does generic checks which are useful for every implementation
"""
function Out_implementation(::Type{signature_typevalues}) where {signature_typevalues<:Tuple}
  # type-stable fallback
  all(isapplicable, signature_typevalues.parameters) || return NotApplicable
  return _Out_implementation(signature_typevalues)
end


"""
this level is intended to be overloaded for specific implementations
"""
function _Out_implementation end

"""
special handling of TypeLevelFunctions which already work on type-TypeLevel

these are mainly functions which are defined for internal purposes
"""
function _Out_implementation(::Type{signature_typevalues}) where {signature_typevalues <: Tuple{TypeValueFunction, Vararg}}
  functype, args... = Tuple_type_to_value(signature_typevalues)
  func = @match(functype) do f
    f(::Type{TypeValueFunction{F}}) where F = F.instance
  end
  return func(args...)
end

const TOO_GENERIC_TYPES = (Any, Function, Type, Module)

"""
fallback to
- either `Core.Compiler.return_type` for simple stuff
- or `_Out_dynamo` for a generic implementation
"""
function _Out_implementation(::Type{signature_typevalues}) where {signature_typevalues<:Tuple}
  # TODO somehow summarize which functions are automatically defined by `Core.Compiler.return_type`
  # this is helpful because we can do better than return_type by respective ValType
  signature_notypevalues = signature_without_typevalues(signature_typevalues)
  F = fieldtypes(signature_notypevalues)[1]

  if F <: Core.Builtin
    # special handling of Builtin functions, as `static_hasmethod` does not work on them
    # we simply fall back to Core.Compiler.return_type directly

    # builtin_function = repr(F)
    # error("""
    #   Recursed to builtin function `$builtin_function`.
    #   Please, overwrite `IsDef.Out` for `IsDef.Out(::Type{Tuple{typeof($builtin_function), ...}}) = ...`
    #   or for a previous function.

    #   The queried signature was:
    #   $signature_typevalues
    # """)

    return quote
      :Core_Builtin_Fallback
      $_Core_return_type($apply, $signature_notypevalues)
    end

  elseif F <: IsDef.IntrinsicFunction
    # special handling of IntrinsicFunctions, because
    # 1) `static_hasmethod` does not work on them
    # 2) they don't have a proper type

    intrinsic_function = F.parameters[1]
    # error("""
    #   Recursed to intrinsic function `$intrinsic_function`.
    #   Please, overwrite `IsDef.Out` for `IsDef.Out(::Type{Tuple{IsDef.IntrinsicFunction{$intrinsic_function}, ...}}) = ...`
    #   or for a previous function.

    #   The queried signature was:
    #   $signature_typevalues
    # """)

    ftype, argstype = signature_split_first(signature_notypevalues)
    return quote
      :Intrinsic_Function_Fallback
      $_Core_return_type($intrinsic_function, $argstype)
    end

  elseif all(_isleaf, fieldtypes(signature_notypevalues))
    # functions which consists purely out of Base/Core stuff are handled by falling back to Core.Compiler.return_type
    # we need to make sure, that this is not too general, hence we check whether the to be called methods are too generic
    mts = covering_method_instances(signature_notypevalues)
    length(mts) > 1 && error(
      "not sure what this is about. Found several matching method instances. Maybe an ambiguity error. methods = $mts"
    )
    if !isempty(mts)
      method_instance = only(mts)

      found_generic_base_function = any(t -> t ∈ TOO_GENERIC_TYPES, fieldtypes(method_instance.def.sig))
      found_generic_base_function && Core.println(Core.stderr, """
        WARNING: Recursed to a generic leaf function (everything is from Base or Core, and found at least one too generic type $TOO_GENERIC_TYPES).

        Please, overwrite `IsDef.Out` for the general case, e.g. with a fallback to Core.Compiler.return_type like so:
        ```julia
        IsDef.Out(::Type{T}) where T <: $(method_instance.def.sig) = IsDef.Core_return_type(IsDef.apply, T)
        ```

        Alternatively you can overwrite the given concrete case
        ```julia
        IsDef.Out(::Type{$signature_typevalues}) = ...
        ```
        Or overwrite another function called before this one in the callstack.

        It is always recommended not to use Core_return_type if you know the return type by other means.
        """)
    else
      Core.println(Core.stderr, "Warning: found empty method_instances for signature $signature_notypevalues.")
      # in the case that the method table is empty, no method is available yet, hence static_hasmethod will return false, but retrigger
      # compilation if the method is redefined. We simply fallback to Core.Compiler.return_type in this case, even though we do not know
      # whether we use it on a generic method.
      # Don't know any better behaviour.
    end

    return quote
      :all_types_are_from_BaseCore_fallback
      $static_hasmethod($signature_notypevalues) || return $NotApplicable
      $_Core_return_type($apply, $signature_notypevalues)
    end

  else
    # fall back to _Out_dynamo if method is too generic
    # we need to generate static_hasmethod call for it to actually retrigger compilation in case the hasmethod changed
    dynamo_args = dynamointernals_ensure_innervalue(Tuple_type_to_value(signature_typevalues))
    return quote
      $static_hasmethod($signature_notypevalues) || return $NotApplicable
      $_Out_dynamo($signature_typevalues, $(dynamo_args...)) |> $dynamointernals_innervalue_to_types
    end
  end
end


# _Out_dynamo
# ===========

# we use signature_typevalues for dispatch, because it has more details
# and should less often lead to recursion-inference-problems
function _Out_dynamo_implementation(::Type{signature_typevalues}, args::Vararg{<:Any, N}) where {signature_typevalues <: Tuple, N}
  signature_notypevalues = signature_without_typevalues(signature_typevalues)
  ir = IR(Tuple_type_to_value(signature_notypevalues)...)
  isnothing(ir) && error("""
    NOTAPPLICABLE: Encountered signature type with no IR (intermediate representation), please overwrite IsDef.Out respectively.
    signature_typevalues = $(signature_typevalues)
    signature_notypevalues = $(signature_notypevalues)
  """)

  # sneakyinvoke trick taken from the IRTools example https://github.com/FluxML/IRTools.jl/blob/master/examples/sneakyinvoke.jl
  IRTools.argument!(ir, at = 1)

  # all function calls which return arguments which are not used can be safely ignored, as for type inference mutation can be ignored
  keep_only_what_is_explicitly_used!(ir)

  if detect_cycle(ir)
    ir_fallback = empty(ir)
    for arg in IRTools.arguments(ir)
      IRTools.argument!(ir_fallback)
    end
    return_type = xcall(_Core_return_type, apply, signature_typevalues)
    # we still need to wrap return_type into our internal wrappers
    IRTools.return!(ir_fallback, xcall(dynamointernals_ensure_innervalue, return_type))
    return ir_fallback
  end

  # Change all branch.condition such that they can work with `Bool` in addition to `true` and `false`
  # TODO with new signature_typevalues (was a signature_notypevalues before),
  # we can actually already take out branches immediately, so that the code simplifies in all cases
  lift_ifelse!(ir)

  # replace functioncalls with calls to Out
  # as this introduces new branches, it has to be done after lifting ifelse
  for block in iterateblocks(ir)
    for (x, st) in block
      isexpr(st.expr, :call) || continue
      # TODO can we compose these functions also in here?
      ir[x] = xcall(_dynamointernals_innervalue_to_types_ANDTHEN_Out_ANDTHEN_dynamointernals_ensure_innervalue, st.expr.args...)
      # this is needed because a return value may be used as a Bool, however Bool have special handling as they can define branching conditions
      # things will fail in such situations, if we do not short-cycle on a NotApplicable
      shortcycle_if_notapplicable!(ir, x)
      break # this block is now done as we shortcycled it
    end
  end


  # Extra Handling
  # --------------

  # replace new with the first argument (i.e. the type going to be constructed)
  ir = MacroTools.prewalk(ir) do x
    isexpr(x, :new) ? Expr(:call, new_out, x.args[1]) : x
  end

  return ir
end


function _dynamointernals_innervalue_to_types_ANDTHEN_Out_ANDTHEN_dynamointernals_ensure_innervalue(args::Vararg{<:Any, N}) where N
  args′ = Tuple_value_to_type(map(dynamointernals_innervalue_to_types, args))
  out = Out(args′)
  result = dynamointernals_ensure_innervalue(out)
  result
end


"""
    _isleaf(type) -> Bool

check whether the type does not need further recurse.
If the type belongs to Core or Base it is considered a leaf.

In order to define an exception, _isleaf would need to be overwritten.
"""
function _isleaf(type)
  mod = Base.typename(type).module
  return _isleaf(mod)
end

function _isleaf(::Type{Core.Const})
  Core.println(Core.stderr, "WARNING: Found Core.Const and regarding it as leaf type is probably wrong")
  true
end

function _isleaf(mod::Module)
  mod ∈ (Base, Core) && return true
  parent = Base.parentmodule(mod)
  mod === parent && return false
  return _isleaf(parent)
end

function _isleaf(uniontype::Union)
  _isleaf(uniontype.a) && _isleaf(uniontype.b)
end

"""
the output type of calling the internal function new, given the target output type
"""  # if we already receive a type-level, we need to strap of one layer
new_out(type::TypeLevel{Type{Type{T}}}) where T = TypeLevel(T)
new_out(type::TypeLevel) = error("this should not happen")
# if we receive value-level, we can directly treat it as type-level
new_out(type::TypeLevel{ValType{ParentType, T}}) where {ParentType, T} = dynamointernals_ensure_innervalue(T)
new_out(type) = dynamointernals_ensure_innervalue(type)