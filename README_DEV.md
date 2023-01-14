## Revise

`IsDef` makes heavy use of generated functions, which are not well supported by Revise.

Use `IsDef.revise!()` to redefine all generated functions in the running julia session.

## Debug Logging

Use `isdef_debug` and `Out_debug` to get a nice log of the typeinference steps.