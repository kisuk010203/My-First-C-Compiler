# My First `C` Compiler
## Material
**Writing a C Compiler(Build a real programming language from scratch)** - Nora Sandler
We will do this in <span style="color:#B94700">_Rust_</span>

## Part 0 : A High-Level Overview
`Compiler` : Program that translates code from one programming language to another

`Assembler` : Program that translates assembly into `object files`

`Linker` : Combines all object files to make one final `executable` program.

`Preprocessor` : Does preprocess before the compile occurs : Strip comments, execute directives, expand macros, etc.

```mermaid
graph TD;
    E(Another object file) --> G(( ))
    A(C source Code)-->|Preprocessor| B(Preprocessed C source code)
    B --> |Compiler|C(Assembly Code)
    C --> |Assembler|D(Object file)
    D --> G
    F(Yet another object file) --> G
    G --> |Linker| H(Executable)
```
We are not going to implement the preprocessor, assembler, and the linker.

## Part 1 : The Basics
### Design Workflow
```mermaid
graph TD;
    A(( ))-->|program.c|B(Lexer)
    B-->|Token List|C(Parser)
    C-->|AST|D(Assembly Gen)
    D-->|Assembly|E(Code emission)
    E-->|program.s|F(( ))
```