# Simple Low-level Language
*Silly Little Language*

This is a learning project for compiler architecture and assembly language. Currently, it outputs Arm32 assembly. A lot of inspiration is taken from Rust (and a bit from TypeScript).

## High-level Architecture

### AST

First, the input text is transformed into an AST. This step is performed by the `pest` library, and can be found under [src/parser/program.pest](./src/parser/program.pest).

Then, this AST is converted into MIR (mid-level intermediate representation). This conversion can be found under [src/parser/mod.rs](./src/parser/mod.rs). This conversion is mostly one-to-one, although a small amount of syntax sugar is removed (for example, `let a: u32 = 1;` is split into `let a: u32;` and `a = 1;`).

### MIR

The MIR (mid-level intermediate representation) is where most of the compilation work is done. You can see a high-level view of what happens in [src/mir/mod.rs](./src/mir/mod.rs) (under `visit_mir`). Initially, the MIR looks like the AST, and is gradually transformed to look like IR (intermediate representation).

This transformation includes things like type checking, constant evaluation, expression simplification, if/loop flattening, and expression splitting (reducing a complex expression to a series of primitive operations).

After these transformations, the MIR is lowered into IR (like before, this lowering is mostly one-to-one). IR can't represent everything that MIR can, but it can represent everything that transformed MIR can. You can find this in [src/ir/mod.rs](./src/ir/mod.rs).

### IR

The IR (intermediate representation) is the final stage before assembly. Currently, there aren't any transformations that occur on IR. The main difference between MIR and IR is that IR doesn't include complex expressions, and only contains the minimum required type information (i.e., size and alignment). You can find the IR definitions in [src/ir/mod.rs](./src/ir/mod.rs).

### Arm32

Finally, IR is converted into Arm32. There's a helper in [src/ir/alloc.rs](./src/ir/alloc.rs) that's used to determine where to allocate variables on the stack. Then, the conversion is performed in [src/ir/arm32.rs](./src/ir/arm32.rs).