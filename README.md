# Chry Compiler

A toy **statically typed compiler** written in Rust.  
The project is designed as both a learning exercise in compiler construction and a functioning backend pipeline capable of lowering high-level constructs to **x86-64 assembly**.

---

## Features

- **Statically typed language** with:
  - Explicit types (`int`, `bool`, etc.)
  - Functions with typed parameters and return types
  - Variables, assignments, control flow (`if`, `while`), and expressions
- **Multiple Intermediate Representations (IRs):**
  - **HIR** – High-level IR, close to typed AST
  - **SSA** – Static Single Assignment form with phi insertion
  - **LIR** – Low-level IR with virtual registers
  - **x86-64 assembly** output

---

## Compiler Pipeline

The compiler is built as a series of lowering and analysis passes:


1. **Scanning**
   - Basic Scanner for matching keywords
   - Produces token stream for parsing
2. **Parsing**
   - Recursive descent parser for expressions and statements
   - Produces a typed AST

3. **Type Checking**
   - Variable Definition checking and proper statemnets and expression checking"
  
4. **HIR (High-level IR)**
   - Structured instructions (`StoreVar`, `Return`, `If`, etc.)
   - Maintains type annotations

5. **SSA (Static Single Assignment)**
   - Variable renaming into SSA form
   - Phi insertion via dominance frontiers (Cytron et al. 1989 algorithm)
   - Dominator tree construction (Lengauer-Tarjan)

6. **LIR (Low-level IR)**
   - SSA is lowered into a flat sequence of virtual register operations
   - Phi nodes are eliminated into moves in predecessors
   - Supports live range analysis

7. **Register Allocation**
   - Virtual registers → physical registers
   - Linear scan allocator (with future possibility of graph coloring)
   - Optional register coalescing

8. **x86-64 Backend**
   - LIR lowered to assembly
   - Handles type sizes (`byte`, `word`, `dword`, `qword`)
   - Generates NASM-style output

---

## Analyses and Passes

- Control Flow Graph (CFG) construction  
- Dominator tree and frontier computation  
- SSA renaming in reverse postorder  
- Live range analysis for register allocation  
- SSA → LIR phi elimination strategies (option A: same vreg, option B: fresh vregs)  

---

## Project Structure

chry_compiler/

hir/ # High-level IR definitions and lowering

ssa/ # SSA representation, dominators, phi insertion

lir/ # Low-level IR, virtual registers, lowering

types/ # Core type system, tokens, labels

parser/ # Scanner and parser for source language

tests/ # Integration tests for HIR, SSA, LIR, backend

main.rs # Entry point


---

## Example

Source:

fun main() {
var b: int = 1;
var b2: int = b = 2;
}


Lowered HIR:

LABEL "L0":
t0 = CONST Int(1) : Int
STORE "b" = t0 : Int
t2 = CONST Int(2) : Int
STORE "b" = t2 : Int
t1 = MOVE t2 : Int
STORE "b2" = t2 : Int
RETURN void


SSA and LIR add phis, renamings, and registerized form before finally emitting assembly.

---

## Testing

The compiler includes:
- HIR lowering tests
- SSA renaming and phi insertion tests
- LIR lowering and live range tests
- Backend integration tests to validate x86-64 output

---

## References

- Cytron et al. *Efficiently Computing Static Single Assignment Form and the Control Dependence Graph* (1989)
- Muchnick, *Advanced Compiler Design and Implementation*
- Appel, *Modern Compiler Implementation in ML*

---

## Roadmap

- [ ] Improve register coalescing
- [ ] More advanced optimizations (constant folding, CSE)
- [ ] Full function calls and stack frame management
- [ ] Better error messages and diagnostics
- [ ] Extend type system with arrays, structs, generics
