# a'no:wal semantics

This document specifies what a'no:wal programs mean: how each construct is typed and how it
runs. Syntax is specified in `grammar.md`.

**Conventions**
- Code examples are a'no:wal unless marked otherwise. `%%` starts a comment.
- *compile error*: reported before the program runs, and the program does not run.
- *runtime error*: aborts the running program with a message and trace (section 9).
- **(proposed)** and **(on hold)** mark parts that are not decided yet. Everything else is
  decided.

---

## 1. Pipeline

A program passes through the stages below in order. Each stage reads columns of the node
store and adds a column of its own, or builds a new store when it changes the shape of the
tree.

| # | Stage | Output |
|---|---|---|
| 1 | Lex (strings NFC-normalised) | token array |
| 2 | Type-name pre-pass | declared type names |
| 3 | Parse (Pratt + recursive descent, error recovery) | node store, `span` |
| 4 | Parent links | `parent` |
| 5 | Placeholder rewrite (`_` -> `otala`) | new store |
| 6 | Name resolution, `use`, argument fill-in | `binder`, `fills` |
| 7 | Assignment analysis | `assigned`, `captured` |
| 8 | Type checking, one SCC at a time in dependency order | `type` |
| 9 | Elaboration: overloads, `some` insertion, `check<T>` insertion, `cast` selection | new store |
| 10 | Desugaring to core: loops, `katsa`, `if`/`?:`, blocks, IIFEs, `::`, comparison chains | core store |
| 11 | Closure conversion: capture lists, boxes | `captures` |
| 12 | Evaluation (CEK machine with a frame stack) | effects, value |

Errors from stages 3-9 are collected and reported together, then compilation stops. Types
are erased after stage 9; they remain only in the `type` column, for tooling.

---

## 2. Values

| Type | Values | Runtime class (for `Dyn`) |
|---|---|---|
| `int` | signed integers up to 64 bits; overflow is a runtime error | `IntV` |
| `kl` | 64-bit IEEE-754 doubles | `KlV` |
| `lihwa` | `true`, `false` | `BoolV` |
| `ahsli` | NFC-normalised Unicode text, indexed by grapheme | `StrV` |
| `void` | no value (section 5.6) | `VoidV` |
| `range` | `(start, end, step)` over `int`, lazy | `ConV(range)` |
| `[a]` | arrays | `ArrV` |
| `<l: t, ...>` | objects (records) | `ObjV` |
| user type `t` | a constructor and its fields | `ConV(t)` |
| `(t, ...) -> t` | closures | `FunV` |
| `Term` | a handle on code, `(store_id, node_idx)` | `TermV` |
| `Dyn` | any of the above, checked where used (section 7.3) | none |

How `int` and `kl` are stored (31-bit immediates, boxed 64-bit values) is invisible to
programs; see sections 6.4 and 8.

### 2.1 Values are immutable, variables are not

No value is ever modified: arrays, objects, strings and constructors stay as they were built.
A *variable* can be reassigned with `=`, which makes it refer to a different value.

```
xs: [1, 2, 3]
ys: xs
xs = xs + [4]      %% xs now refers to a new array
print(ys)          %% [1, 2, 3]
```

### 2.2 Equality

`==` and `!=` compare structurally, on every type except functions:
- arrays element by element, objects field by field
- constructors by tag, then fields
- strings byte by byte (both are NFC already)

Both operands must have the same type, so `1 == 1.0` is a compile error.

### 2.3 Ordering

`< > <= >=` compare `int` and `kl` (mixed is allowed) and `ahsli`. Strings compare by code
point, with no locale rules.

Comparisons chain: `0 <= i < n` means `0 <= i && i < n` (section 3.1).

---

## 3. Core language

Desugaring (stage 10) reduces every surface form to this core:

```
e ::= x                                 variable: local (depth, slot) or global name
    | lit                               int, kl, ahsli, lihwa, void
    | fn(x1, ..., xn) e                 n-ary function (not curried)
    | e0(e1, ..., en)                   n-ary call
    | let x = e1 in e2
    | letrec f1 = fn..., ..., fk = fn... in e     right sides must be functions
    | con_k(e1, ..., en)                constructor k
    | case e { k1(x...) -> e1 | ... | _ -> e }    one level, jumps on the tag
    | <l1 = e1, ..., ln = en>           object literal
    | e.l                               field access
    | prim_op(e1, ..., en)              primitive (arithmetic, I/O, arrays, strings...)
    | set x = e                         reassignment, result void
    | exit(target, e)                   ahket / break
    | check<T>(e)                       runtime class check on a Dyn
    | quote(store, node)                Term handle, no copy
    | splice(store, node, [e1, ..., en])  copy a template, fill $ holes with Terms
```

### 3.1 Surface to core

| Surface | Core |
|---|---|
| `{ s1; s2; ...; e }` | chained `let`s; value `e`, or `void` without a trailing expression |
| `x: e` / `T x: e` / `atalo x: e` | `let x = e in ...` (an annotation becomes a check) |
| `x = e` | `set x = e` |
| `otala f(...) {...}` | `letrec f = fn(...) ... in ...` |
| `f :: (...) {...}` | `let f = fn(...) ... in ...` (non-recursive: inner `f` is the previous binding) |
| `otala(...) {...}` | `fn(...) ...` |
| `otala(x: e1, y: e2) {...}` | `let x = e1 in let y = e2 in ...` |
| `otala go(x: e1) {...}` | `letrec go = fn(x) ... in go(e1)` |
| `if (c) {a} then (d) {b} else {e}` | `case c { true -> a \| _ -> case d { true -> b \| _ -> e } }` |
| `c ? a : b` | `if (c) {a} else {b}` |
| `if` without `else` | missing branch is `void` |
| `a < b <= c` | `let t = b in a < t && t <= c` (`t` fresh; `b` evaluated once) |
| `a && b` | `case a { true -> b \| _ -> false }` |
| `a \|\| b` | `case a { true -> true \| _ -> b }` |
| `for` loops | `letrec loop = fn(...) ...` (section 6.7) |
| `katsa` | decision tree of one-level `case`s (Maranget 2008) |
| `nil`, `some(e)`, user constructors | `con` |
| `_`, `_0`, ... placeholders | `fn` |
| operators, `.sz`, `e[i]`, `cast<T>` | the `prim` chosen by type (section 5.4) |
| `ahket e` / `break e` | `exit(target, e)` |
| `` `{...} `` | `quote`, or `splice` if it contains `$` |
| `print`, `cc`, ... | `prim` |
| `type` aliases | nothing; they exist only in types |

### 3.2 Top level and modules

A file is a module. Its top level runs in two steps:
1. Every top-level `otala` function is bound in one `letrec`. Functions are hoisted, so
   their order doesn't matter.
2. The remaining statements run in source order.

Each top-level name is defined once. In the REPL, a new definition shadows the old one.

Using a top-level value before its definition:
- directly is a compile error;
- through a function called too early is a runtime error, "`x` used before it's defined".

```
otala show() { print(limit) }
show()              %% runtime error: `limit` used before it's defined
limit: 10
```

Exports and imports:
- `export` marks a function or type as visible to other files.
- `use m.f` binds `f` to `m`'s exported value.
- Files are loaded and checked in `use` order. A `use` cycle is a compile error.
- The prelude is `use`d implicitly.

---

## 4. Names and scoping

Scoping is lexical: a name refers to the nearest enclosing binding.

- `x: e` creates a new binding, which may shadow an outer one.
- `x = e` reassigns the nearest existing `x`. Reassigning an unbound name is a compile error.
- `otala f` is visible in its own body, so it can recurse. `f ::` is not; inside its body,
  `f` is the previous binding.
- Types have their own namespace. Values and constructors share one. Constructor names are
  unique across all types, and a declared type name can't be used as a variable name.
- Locals are addressed `(0, slot)` in the current frame or `(1, slot)` in the current
  closure's captures. Globals and imports are looked up by name.

### 4.1 Argument fill-in

When a direct call to a *named* function leaves out trailing arguments, each missing
parameter is looked up **by name** in the scope of the call, and the call is rewritten to
pass it.

```
otala area(w, h) { w * h }
h: 4
area(3)             %% area(3, h) = 12
```

- No binding with that name is a compile error: "f needs n arguments, got m, and no `p` is
  in scope to fill in."
- Calls through a function *value* get no fill-in; too few arguments is an arity error.
- Under `eval`, the same lookup happens at runtime against the environment record.

---

## 5. Static semantics (types)

### 5.1 Types

```
t ::= a                         type variable
    | con name [t...]           int, kl, lihwa, ahsli, void, range, array, option, user types
    | (t1, ..., tn) -> t        n-ary function
    | <l1: t1, ..., ln: tn | r> object; r is a row variable (open) or empty (closed)
    | Dyn
s ::= forall a... r... . t      type scheme (after generalisation)
```

Surface types map onto these:

| Surface | Type |
|---|---|
| `int?` | `option<int>` |
| `[int]` | `con array [int]` |
| `<x: int>` in an annotation | `<x: int \| r>`, fresh `r` (open) |
| object literal `<x: 1>` | `<x: int>` (closed) |
| alias | its expansion |
| undeclared type name | type variable, scoped to the enclosing closure |

### 5.2 Core rules (Hindley-Milner with rows)

Rules read `premises ⟹ conclusion`. `Γ` is the typing environment, `inst` instantiates a
scheme with fresh variables, and `gen` generalises over the variables not free in `Γ`.

```
Var      Γ(x) = σ                                     ⟹  Γ ⊢ x : inst(σ)
Lit      lit has base type b                          ⟹  Γ ⊢ lit : b
Abs      Γ, x1:τ1, …, xn:τn ⊢ e : τ                   ⟹  Γ ⊢ fn(x1…xn) e : (τ1…τn) -> τ
App      Γ ⊢ e0 : (τ1…τn) -> τ    Γ ⊢ ei : τi         ⟹  Γ ⊢ e0(e1…en) : τ       (exactly n)
Let      Γ ⊢ e1 : τ1    Γ, x : S ⊢ e2 : τ             ⟹  Γ ⊢ let x = e1 in e2 : τ
           where S = gen(Γ, τ1) if x is never reassigned, else S = τ1 (monomorphic)
LetRec   Γ, f1:α1…fk:αk ⊢ fni : αi   (fresh αi, monomorphic inside the group)
         Γ, f1:gen(α1)…fk:gen(αk) ⊢ e : τ             ⟹  Γ ⊢ letrec … in e : τ
Con      k : ∀ᾱ. (τ1…τn) -> t ᾱ    Γ ⊢ ei : τi[ᾱ:=β̄]  ⟹  Γ ⊢ con_k(e…) : t β̄
Case     Γ ⊢ e : t β̄    each arm: Γ, fields ⊢ ei : τ  ⟹  Γ ⊢ case e {…} : τ
Obj      Γ ⊢ ei : τi                                  ⟹  Γ ⊢ <l1=e1…> : <l1:τ1… | ∅>
Field    Γ ⊢ e : <l: τ | ρ>                           ⟹  Γ ⊢ e.l : τ
Set      Γ(x) = τ (monomorphic)    Γ ⊢ e : τ          ⟹  Γ ⊢ set x = e : void
Exit     Γ ⊢ e : τ_target                             ⟹  Γ ⊢ exit(target, e) : α   (fresh α)
Check    Γ ⊢ e : Dyn                                  ⟹  Γ ⊢ check<T>(e) : T
```

- `τ_target` is the return type of the enclosing `otala` for `ahket`, and the loop's state
  type for `break`. A bare `ahket` or `break` passes `void` or the current loop state.
- A variable that is ever reassigned stays monomorphic.
- Unification is standard, with an occurs check, extended for rows (a row variable unifies
  with the missing fields) and arity (function types unify only if their parameter counts
  are equal).
- Definitions are checked one SCC of the call graph at a time, in dependency order.
  Independent SCCs may be checked in parallel.

### 5.3 Option insertion

When an expression of type `T` is checked against an *expected* type `T?` (from an
annotation, parameter, return type or field), it is wrapped in `some`. At most one `some`
is inserted. Without an expected type nothing is inserted.

```
int? n: 5               %% n = some(5)
m: c ? 5 : nil          %% compile error: no expected type
int? k: c ? 5 : nil     %% ok
```

### 5.4 Overloaded operators and built-ins

An overloaded operator gets the type `a -> b -> c` (prefix: `a -> c`) plus the list of
allowed combinations below. Once its definition is inferred, `(a, b)` is looked up in the
list, `c` is set, and the operator is replaced by the matching `prim`.

| Operator | Allowed -> result | If unresolved |
|---|---|---|
| `+ - * /` | int,int -> int; kl,kl -> kl; int,kl -> kl; kl,int -> kl | int |
| `+` | also ahsli,ahsli -> ahsli; [a],[a] -> [a] | int |
| `%` | {int,kl} x {int,kl} -> int | int |
| `& \| ^ << >>`, `~` | int -> int | int |
| `< > <= >=` | int/kl, mixed -> lihwa; ahsli,ahsli -> lihwa | int |
| `== !=` | t,t -> lihwa, for any non-function t | int |
| `!` | lihwa -> lihwa | n/a |
| `e.sz` | ahsli, [a], range -> int | error |
| `e[i]` | [a],int -> a; ahsli,int -> ahsli | error |
| `e[a..b]` | [a],range -> [a]; ahsli,range -> ahsli | error |
| `for (x of e)` | [a] gives x:a; range gives x:int; ahsli gives x:ahsli | error |
| `contains`, `index_of` | ahsli or [a] | error |

`print(x)` accepts any type except functions and `void`. `cc(e1, ..., en)` requires every
argument to be `ahsli`. Neither can be used as a value.

### 5.5 Casts

`cast<T>(e)` selects a conversion from the type of `e` and `T`:

| From -> to | Result |
|---|---|
| int -> kl | exact up to 2^53, rounded above |
| kl -> int | truncated toward 0; NaN, infinity or out of range is a runtime error |
| lihwa -> int | 0 or 1 |
| any non-function -> ahsli | its printed form (`str`) |
| ahsli -> int, kl, lihwa | parsed; failure is a runtime error |
| Dyn -> T | `check<T>` |
| T -> T | unchanged |
| other | compile error |

`cast<T?>(e)` returns `nil` where `cast<T>` would raise a runtime error. A source type that
can't be inferred is a compile error.

```
cast<int>(3.9)          %% 3
cast<int?>("abc")       %% nil
```

### 5.6 `void`

`void` may appear only:
- as a function's result type
- as the type of an expression in statement position
- as the value of a block, `if`, `?:` or `for` whose own value is `void`

Any other use is a compile error, "x was void, we expected a value.": binding it, passing
it, using it as an operand, element or field, comparing it, or converting it. A type
variable can become `void` only in a function-result position, so `map(xs, print)` is a
compile error.

### 5.7 Annotations, aliases, type variables

- Annotations are optional. Where present they are checked by unification.
- A type variable `a` belongs to the closure whose annotations mention it first, nested
  functions included. `otala<a> f(...)` declares a fresh `a` that shadows any outer one.
- Aliases are transparent and can't be recursive.

### 5.8 Static checks besides typing

- `katsa` must be exhaustive; the error lists the missing constructors. Unreachable arms
  produce a warning.
- Warnings are also produced for an unused binding, a variable that shadows a constructor
  (a local `kes: 3` turns a later `kes =>` arm into a catch-all), and a statement that is
  only a placeholder function, such as `_ + 1`.
- Placeholder numbering errors (`_0 + _2`, mixed `_` and `_n`) are compile errors.
- Every declaration needs an initialiser.
- At top level, defining a name twice or a clash between `use`s is a compile error.

---

## 6. Dynamic semantics (evaluation)

### 6.1 Strategy

Evaluation is call-by-value, in a fixed left-to-right order: arguments in order, object
fields in source order, a `let`'s value before its body, binary operands left then right.
`&&` and `||` short-circuit. Effects happen when their expression is evaluated.

### 6.2 Machine state (CEK with a frame stack)

The machine state is `(C, F, Cl, S)`:

| Part | Meaning |
|---|---|
| `C` | control: the core node being evaluated, or a value being returned |
| `F` | current frame: argument and `let` slots |
| `Cl` | current closure: its captured values |
| `S` | frame stack, the continuation: one entry per unfinished step |

Each entry on `S` holds:
- the next action on a value, e.g. "evaluate argument 2", "bind `x` and run the `let` body",
  "select the `case` arm"
- the saved `F` and `Cl`
- for a call, a **return marker** naming the running function, used by `exit` and traces

### 6.3 Key transitions

| Node | Step |
|---|---|
| `x` at `(0, k)` | `F[k]`; read through the box if boxed |
| `x` at `(1, k)` | `Cl.captured[k]`; read through the box if boxed |
| global `x` | the global slot; empty is a runtime error, "`x` used before it's defined" |
| `fn(...) e` | allocate closure `{node, arity, captured...}`; each captured variable is copied by *value*, or by *box pointer* if it is ever reassigned |
| `e0(e1...en)` | evaluate `e0`, then the arguments left to right; check arity; push a return marker; new frame `[v1...vn]`; `Cl` = the closure; run the body |
| returning a value | pop the top entry of `S` and resume its action |
| `let x = e1 in e2` | evaluate `e1`; store it in `x`'s slot (in a new box if `x` is captured *and* reassigned); evaluate `e2` |
| `letrec` | allocate the closures with empty self/sibling slots, then fill them (cycles allowed) |
| `con_k(...)` | evaluate the fields and allocate; a constructor without fields is an immediate |
| `case e {...}` | evaluate `e`; jump on its tag; bind its fields into the arm's slots |
| `set x = e` | evaluate `e`; write it to `x`'s slot or box; result `void` |
| `exit(target, e)` | evaluate `e`; pop to the most recent return marker for `target`; return the value there. No marker is a runtime error, "`ahket` from a function that already returned" |
| `check<T>(e)` | evaluate `e`; compare its runtime class with `T`. Scalars check the class only; arrays, objects and constructors also check their contents; functions are wrapped so each call checks arguments and result. A mismatch is a runtime error |
| `prim` | evaluate the arguments, run the primitive |

A call in tail position reuses the current entry of `S`, so loops run in constant stack
space.

### 6.4 Numbers

- An `int` is either small (31-bit, unboxed) or big (64-bit, boxed), and is always small
  when it fits. Programs can't tell the two apart.
- `int` arithmetic promotes to big on 31-bit overflow. Overflow past 64 bits is a runtime
  error.
- `int` division truncates toward zero. Division by zero is a runtime error.
- In mixed `int`/`kl` arithmetic the `int` is converted to `kl` first.
- `a % b` is `truncate(fmod(a, b))`, an `int` with the sign of `a`. `% 0` and NaN or infinite
  operands are runtime errors.
- A `kl` is a 64-bit IEEE double, always boxed on the heap. `kl` arithmetic follows IEEE:
  `1.0 / 0.0` is infinity, with no error.
- `>>` is an arithmetic shift. A shift count below 0 or above 63, or a result past 64 bits,
  is a runtime error.

### 6.5 Strings

- Strings are immutable.
- `s[i]` and `s.sz` count grapheme clusters, so `"kátsaˀ".sz` is 6. Both are O(1) for
  strings of one-byte characters, and O(1) after first use otherwise (via a cached offset
  table).
- `s[i]` out of range is a runtime error.
- `+` and `cc` concatenate; the result is re-normalised.

### 6.6 Reassignment

`set x = e` changes what `x` refers to, and every reader of `x` sees the new value:
- code in the same function reads the slot
- closures that captured `x` read the shared box
- globals are read by name

```
n: 0
inc :: () { n = n + 1 }
inc(); inc()
print(n)            %% 2
```

Values stored earlier are unaffected (section 2.1).

### 6.7 Loops and ranges

```
for (i of a..b by s) with (acc: init) body
=> letrec loop = fn(i, acc) if (i < b) loop(i + s, body) else acc in loop(a, init)
     (`>` when s < 0; `<=`/`>=` for `..=`)
for (ever) with (acc: init) body   =>  letrec loop = fn(acc) loop(body) in loop(init)
for (x of xs) ...                  =>  the same, over indices 0..xs.sz
```

- The default step is `1` if `a <= b`, else `-1`. A step pointing away from the end gives an
  empty range. `by 0` is an error: a compile error for a literal, a runtime error otherwise.
- With `with`, the loop's value is its final state; without, it is `void`. With several
  state variables the body and the loop produce an object `<a: ..., b: ...>`.
- `break` exits with the current state, `break e` with `e`. `ahket` leaves the enclosing
  function.
- The body may reassign outer variables.

```
sum: for (i of 1..=10) with (acc: 0) { acc + i }     %% 55
```

### 6.8 Pattern matching

`katsa` compiles to a decision tree that tests each value at most once. Arms are tried in
source order. Literal patterns compare with `==`; a binding pattern binds the value.

```
katsa (shape) {
  circle(r) => 3.14 * r * r,
  rect(w, h) => w * h,
}
```

---

## 7. Metacircularity

### 7.1 Quote and unquote

- `` `{ ... } `` evaluates to a `Term` handle on the parsed nodes, without copying them.
- `$x` and `$(e)` inside a quote are evaluated **when the quote is evaluated**. They must
  produce `Term`s, which are spliced into a copy of the template. Copies live in a runtime
  store separate from the parser's, so they can be freed.
- There is no hygiene: free names in quoted code resolve when that code is evaluated.

### 7.2 `eval`

- `eval(t)` evaluates `t` in the **caller's environment** and returns a `Dyn`. Free names
  resolve against an environment record built at the call site (every visible local, name ->
  slot or box), then against globals.
- Locals visible at an `eval` call site are boxed, since `t` may reassign them.
- Code run by `eval` is not type-checked. Arity, including argument fill-in, is checked at
  runtime.

### 7.3 `Dyn`

- `Dyn` unifies with any type. Where a `Dyn` meets a concrete type `T`, stage 9 inserts
  `check<T>`.
- An overloaded operator whose operands are all `Dyn` selects its primitive at runtime and
  returns `Dyn`.
- `cast<T?>(d)` tests a `Dyn`'s class without aborting.

### 7.4 Term API **(on hold)**

Only the handle, `tag` and `child` exist so far.

---

## 8. Memory

Heap words are 32 bits, in OCaml's 32-bit layout:
- low bit 1: an immediate (small `int`, `lihwa`, `void`, constructors without fields)
- low bit 0: a pointer to a block

Blocks have an OCaml-style header. Constructor blocks carry an extra word for the type id.
Big `int`s and all `kl`s are boxed.

The garbage collector is a Cheney copying collector. A value is live if it is reachable from
a root. The roots are:
- globals
- the machine state: `C`, `F`, `Cl`, every frame on `S`, and the value being returned
- environment records held by `eval`
- the host's term stores

**(on hold):** when a collection runs, how the heap grows, generational and parallel
collection.

---

## 9. Errors

| Stage | Behaviour |
|---|---|
| Parse | recover at the next `;`, newline or `}`; at most one error per statement; report all; don't run |
| Names, types | one error per definition, all reported together; don't run. Format: `file.t:line:col`, newline, `x was __, we expected __.` |
| Runtime | abort with the message and a trace: one line per return marker on `S`, innermost first, giving function name and position. Exit code 2 |

Runtime errors:
- `int` overflow past 64 bits
- `/ 0`, `% 0`, `%` on NaN or infinity
- a shift count out of range, or a shift result past 64 bits
- `by 0` with a non-literal step
- an index out of range
- a failed `cast`
- a `check<T>` mismatch
- a `void` used as a value (reachable only through `eval`)
- an arity mismatch under `eval`
- a value used before its definition
- `ahket` from a function that already returned
- `fail(msg)`, a failed `assert`
