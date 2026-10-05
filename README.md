# bluec — compiler front-end for a Blue-inspired imperative language

A compiler front-end written in **Haskell** for a statically scoped, statically typed imperative
language whose concrete syntax is derived from *Blue*. `bluec` takes a source program through
lexing, LALR parsing, static semantic analysis with a formally specified type system, and generation
of **three-address code (TAC)**.

Group project for the *Languages and Compilers* course of the M.Sc. in Computer Science, University
of Udine (2025). The full design report (in Italian) is in
[`progetto_parte_2_parziale/relazione/tesina.pdf`](progetto_parte_2_parziale/relazione/tesina.pdf).

## The language

```
var
    base0: Array<Array<Integer; 3>; 4> := [[0,0,0], [0,0,0], [0,0,0], [0,0,0]];
    a: Array<&Array<Array<Integer; 3>; 4>; 3> := [&base0, &base0, &base0];
    b: Array<Array<Array<Integer; 3>; 4>; 3> := [base0, base0, base0];

routines
    main is
    var
        idx : Integer := 3;
        res1, res2 : Integer := 0, 0;
    do
        (*a[2])[3][0] := 1;
        res1 := b[2][3][0];
        res2 := b[2][idx][0];
        return;
    end main
```

- **Types:** `Integer`, `Real`, `Char`, `Boolean` and `String`, multi-dimensional arrays
  `Array<T; n>`, and pointers `&T` with dereference `*`.
- **Declarations:** constants and variables, always initialised. Multiple assignment such as
  `a, b := 1, 2` is allowed.
- **Routines:** procedures and functions with multiple return values and four parameter-passing modes
  (by value, by reference, by value-result and by constant). Routines cannot return references, which
  rules out dangling pointers.
- **Control flow:** `if`/`else` and a general `loop … exit on … end loop`, which expresses both
  `while-do` and `do-while`.
- **Exceptions:** `try … catch … end try` and `throw error`.
- **Comments:** single-line (`--`) and multi-line (`/* … */`).

## Pipeline

| Stage | Location | What it does |
|---|---|---|
| Grammar | `src/grammar.cf` | BNFC grammar used to bootstrap the lexer and parser |
| Lexer | `src/LexGrammar.x` (Alex) | Tokens annotated with source positions |
| Parser | `src/ParGrammar.y` (Happy) | LALR(1) grammar with all conflicts resolved |
| AST | `src/AbsSyn.hs`, `src/Types.hs` | Hand-written AST parameterised by its annotation type, with source spans |
| Static semantics | `src/StatSem/` | Type checking of expressions, statements and declarations; environments and scoping; implicit casts made explicit (least common supertype); type propagation; checks for partial routines, assignments to constants, returned references and a valid `main`; errors with positions |
| TAC generation | `src/TAC/` | Typed TAC with temporaries and labels; array address calculation, short-circuit booleans, the parameter-passing conventions, and exception handlers |
| Pretty-printers | `src/PrettyPrinter.hs`, `src/TAC/Printer.hs` | Normalised source code and readable TAC |

Excerpt of the TAC generated for the program above (`tests/testCase5.blue`). Identifiers carry their declaration position, and constant indices are folded into byte offsets:

```
main@21:5:
      idx@23:9 =(int) 3
      t7 =(addr) a@16:5[8]
      t6 =(addr) *t7
      t6[36] =(int) 1
      t11 =(int) idx@23:9 +(int) 8
      ...
      res2@24:9 =(int) b@17:5[t11]
      return
```

### Grammar design notes

- Top-level routines are introduced by the `routines` keyword, so that a routine name cannot be
  confused with the start of a variable initialiser.
- Statements and declarations end with `;` to remove LALR(1) ambiguities. Without it, for example,
  `b (` could start either a call or a parenthesised left-expression, and `*` could be either a
  dereference or a multiplication.
- The remaining shift/reduce conflict between left- and right-expressions is resolved by always
  shifting, because left-expressions are a subset of right-expressions.

## Build and run

The `Dockerfile` (also used by the VS Code dev container) provides GHC 9.10, BNFC 2.9.5, Alex and
Happy.

```bash
cd progetto_parte_2_parziale
make                       # builds ./bluec
./bluec tests/testCase5.blue   # pretty-printed source + generated TAC
./bluec -s file.blue           # TAC only
make demo                  # runs all programs in tests/ and saves each output to tests/DemoN_output.txt
```

`tests/` contains 28 commented programs. Some must compile, and others must be rejected with a
specific error, for example assignments to constants, type mismatches or a missing `main`.

## Part 1

`progetto_parte_1/` is the first assignment of the course, written in C with BNFC, Flex and Bison. It
is a parser and pretty-printer for an XML-like configuration language with file imports, section
inheritance and cross-references between fields, which are resolved after parsing.

## Authors

Leonardo Danelutti, Lorenzo Della Giustina and Francesco Bortuzzo.
