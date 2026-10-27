# a'no:wal garbage collector

a'no:wal is a small typed functional language with an interpreter written in OCaml. This
document describes how its runtime reclaims memory, and why it is built this way. The exact
heap layout and the rules are specified in `semantics.md` section 8.

---

## 1. The heap

- One array of 32-bit words, kept outside OCaml's own heap. OCaml's GC never scans it.
- The layout follows OCaml's 32-bit layout. A word with low bit 1 is an immediate value
  (small integers, booleans, constructors without fields). A word with low bit 0 points to a
  block.
- A block is a header word (size, 2 GC bits, tag) followed by its fields. Some tags mark raw
  data (floats, large integers, string bytes), which the collector never scans.

Two properties of the language shape the design:
- **Data is immutable.** Arrays, objects and bindings never change, so a new block can only
  point to blocks that already exist. Existing blocks are written in a few places only:
  mutable variables captured by a closure (stored in a one-field "box"), recursive closures
  that are patched to point to themselves, a per-string cache, and built-in functions filling
  in a result.
- **Most data dies young.** Floats are boxed, adding to an array copies it, and updating an
  object builds a new one. The program allocates a lot, and most of it is garbage almost
  immediately.

---

## 2. Three collections

| Collection | What it does | When | Runs on |
|---|---|---|---|
| **Minor** | copies the live part of the nursery; blocks that survive twice move to old space | nursery full | main thread, program paused |
| **Major** | marks and sweeps old space | old space past a threshold | separate thread, alongside the program |
| **Full** | copies every space, removing fragmentation | rarely | main thread, program paused |

The heap is split roughly like this:

```
nursery | survivor A : survivor B | old
  10    |     10     :     10     |  70
```

**Why generational.** Most blocks die young. A minor collection only looks at the nursery and
copies what is still live, so its cost follows the live data, not the garbage. Long-lived data
moves to old space and stops being copied.

**Why the minor collection pauses the program.** It moves blocks. If the program kept running,
it could follow a pointer to a block that has just moved. Preventing that would need a check on
every heap read, paid by every step of the interpreter. Collecting a small nursery is quick, so
pausing costs little.

**Why the major collection runs alongside the program.** Old space is large, so collecting it
takes a long time. Marking moves nothing, so the program can keep running during it; only
writes need watching (section 4).

**Why mark-sweep for old space.** A copying collector needs an empty space as large as the one
it copies from, so half its memory is always idle. Marking needs no second space, so about 90%
of the heap is usable. Mark-sweep leaves holes behind; the rare full collection closes them.

**The nursery has a fixed maximum size**, so minor pauses stay short as the heap grows.

---

## 3. Where a new block goes

| Size | Space |
|---|---|
| small (up to 5% of the nursery) | nursery |
| medium (up to 9% of a survivor space) | the occupied survivor space, marked to move to old space at the next minor collection; old space if there is no room |
| large | old space |

Large blocks would otherwise be copied by every minor collection they survive.

---

## 4. The write barrier

A write barrier is a short check that runs whenever the program writes a pointer into an
existing block. It protects two assumptions:

1. **Old blocks don't point to young ones.** The minor collection skips old space; that is
   what makes it fast. A write can still create such a pointer:
   ```
   xs: [0]
   f: otala() { xs }      %% f captures xs, and xs is reassigned: xs lives in a box
   ...                    %% the box survives long enough to reach old space
   xs = [1, 2, 3]         %% a new (young) array written into an old box
   ```
   Without the barrier, the minor collection never sees the pointer and frees the array. The
   barrier records the box, and the minor collection treats recorded blocks as extra starting
   points.
2. **A finished block never points to an unvisited one.** While marking runs, the program can
   move the last pointer to a block from a part the marker hasn't reached into a part it has
   already finished. The marker never sees the block, and the sweep frees it.

**Choice: snapshot at the beginning (Yuasa's barrier).** When a field is overwritten, the
barrier marks the value being *replaced*. Everything reachable when marking started survives
it. OCaml's own write barrier (`caml_modify`) works the same way, for the same reasons.

**Why it fits here.** The interpreter already keeps its call frames on a separate frame stack:
one array holding every running call's values, which the minor collection already scans. The
snapshot at the start of marking is a scan of that existing structure. Frame slots change on
almost every step, and with this barrier they need no check at all.

**Why not the alternatives:**
- **Dijkstra's barrier** marks the *new* value instead. Since frame slots have no barrier, the
  marker would have to pause the program at the end and rescan every frame.
- **Steele's barrier** sends the written block back to be scanned again. The marker can keep
  revisiting busy blocks, which makes it harder to know when marking finishes.

The cost: a block that dies while marking is running survives until the next cycle.

---

## 5. GC bits in the header

The header's 2 GC bits mean different things in young and old blocks:

| Bits | Young block | Old block |
|---|---|---|
| 00 | survived 0 times | not reached |
| 01 | survived once | reached, not scanned |
| 10 | survived twice | scanned |
| 11 | moved (only during a minor collection) | free space |

Reading them with the wrong meaning would silently corrupt the heap, so:
- **A block's generation comes from its address, never its header.** Each space is a fixed
  address range.
- In the OCaml code, young and old pointers have different types, created only by that address
  check, so reading an old meaning from a young block doesn't compile.
- Every move rewrites the bits, and a debug mode checks all of them after each collection.

---

## 6. Roots

A block is live if it can be reached from a root:
- global variables;
- the interpreter's state: the frame stack, the current closure and the value being returned;
- the environments built for `eval` (runtime evaluation of code);
- for the minor collection, the blocks recorded by the write barrier.

**The interpreter never keeps a heap pointer in an OCaml variable across an allocation.** The
collector moves blocks and can't see OCaml variables, so such a pointer would go stale. Two
rules follow:
- **Built-in functions allocate first, then fill.** They work out the total size, allocate
  once, and fill in the fields without allocating again.
- **Built-in functions that call other functions** (`map`, `fold`, ...) keep their state in a
  frame on the frame stack, not in an OCaml loop. The function they call may allocate and
  trigger a collection. State in a frame is seen and updated by the collector; state in an
  OCaml variable is not.

---

## 7. Quoted code

The language can treat code as data. A quoted piece of code lives in a node store in OCaml
memory, and the heap value only holds the store's number. OCaml's GC frees unused stores, but it
can't see numbers inside the heap array. So each major collection records which store numbers
are still in use; afterwards the others are released, and OCaml reclaims them.

---

## 8. Why not reference counting

A recursive function's closure points to itself, and a box can point back to the closure that
holds it. Reference counting never frees such cycles. A tracing collector handles them without
extra work.
