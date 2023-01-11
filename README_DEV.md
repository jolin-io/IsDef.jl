## Revise

`IsDef` makes heavy use of generated functions, which are not well supported by Revise.

Use `IsDef.revise!()` to redefine all generated functions in the running julia session.