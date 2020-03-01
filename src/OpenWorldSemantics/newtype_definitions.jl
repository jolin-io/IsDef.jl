# some known abstracttypes from Core and  Base
# ----------------------------------

@create_newtype Any
@create_newtype Signed Integer Real Number
@create_newtype AbstractFloat
@create_newtype AbstractChar AbstractString
@create_newtype AbstractDict
@create_newtype DenseArray AbstractArray AbstractVector AbstractMatrix Base.AbstractZeroDimArray
@create_newtype AbstractUnitRange OrdinalRange AbstractRange
@create_newtype AbstractSet
@create_newtype Function
@create_newtype(
  Base.AbstractCartesianIndex, Base.AbstractChannel, Base.AbstractCmd,
  Base.AbstractDisplay, Base.AbstractIrrational, Base.AbstractLock,
  Base.AbstractPipe)
