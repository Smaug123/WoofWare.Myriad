Implement this plan with each stage on its own branch, stacked as necessary on previous branches, so that a reviewer can review each branch in isolation.

## Stage 1: Constructor plan and property-test oracle

**Dependencies**: None

**Implements**: DESIGN.md § Functional core and generator shell; § Guarantees

**Correctness oracle**:

- Property: planned assignments contain every input field exactly once and in the same order.
- Property: planned parameters are exactly the required fields, with their types and relative order preserved.
- Property: every optional field maps to `UseNone`, and every required field maps to its unique parameter.
- Property: parameter names are the unique consecutive sequence `arg_0` through `arg_(n-1)`.
- Property: planning is deterministic and respects consistent field renaming.
- The generator intentionally covers all-required, all-optional, and mixed shapes by construction, and asserts the observed distribution.

---

## Stage 2: F# syntax adapter and AST renderer

**Dependencies**: Stage 1

**Implements**: DESIGN.md § Meaning of optional; § Functional core and generator shell

**Correctness oracle**:

- Property: top-level `option` types, including nested options, are classified as optional; non-option, `voption`, and nullable types are required.
- Property: parsed valid record shapes produce exactly the expected constructor plan, and AST construction succeeds for each shape.
- Focused examples assert the all-optional unit argument and applied generic return type.

---

## Stage 3: Attribute and Myriad integration

**Dependencies**: Stage 2

**Implements**: DESIGN.md § Public API; § Deliberate limits

**Correctness oracle**:

- The consumer fixture compiles generated constructors for mixed, all-required, all-optional, nested-option, generic, internal, tupled, and function-typed records, including awkward or casing-distinct labels.
- Property: for arbitrary required values, the compiled mixed-record constructor returns the corresponding direct record literal with every optional field equal to `None`.
- Misapplying the attribute to a non-record, nesting it inside a module, and using a private record representation produce focused generator errors.

---

## Stage 4: Package surface and documentation

**Dependencies**: Stage 3

**Implements**: DESIGN.md § Public API; § Meaning of optional

**Correctness oracle**:

- Both public API surface tests pass with the new attribute and generator recorded.
- XML documentation coverage passes.
- The README example agrees with the compiled consumer fixture.
- Fantomas, the focused tests, and the full solution test suite pass without warnings.
