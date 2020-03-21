# Performance tips

## General

- When trying to improve performance, use one of the profilers to check that
  your change actually improves things.

- Use strict records instead of tuples, especially for `Map` keys and `Set`
  elements, to avoid laziness and especially the `<` instance for tuples.

- Avoid chains of `MaybeError` and `Maybe` (e.g.
  `MaybeError String (Maybe ...)`); define a local three-way type instead. This
  allows the next optimization:

- Think about which guards/patterns are the most common cases and put those
  first.

- Avoid unnecessary pointfree notation (e.g., do not write `const ((===) key)`
  but write `\_ k -> k == key`.

- Use boxed records (`:: T = ! { ... }`) where appropriate. Normally, records
  are unboxed when they are a strict argument of a function, meaning that each
  field gets a space on the stack. With large records of which most fields are
  not used by the function, this should be avoided. `IWorld` is an example of a
  type that should be boxed.

## Shares

- When defining a share it is good sense to use a CAF (define with `=:`) so
  that it is only evaluated once. Otherwise its identity gets recomputed, which
  can be heavy for complicated SDS trees.
