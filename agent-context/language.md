---
title: Pisemble language reference for agents
description: Syntax, supported AArch64 instructions and addressing modes, raw opcode workarounds, register renaming and the helper macro libraries in lib/
ms.date: 2026-10-17
---

## How a Pisemble file works

A Pisemble source file starts with `#lang pisemble`. That is plain Racket plus the assembler from `expander.rkt` and a reader extension from `reader.rkt`. Assembly only happens inside an `(aarch64 "out-file" [link files] ...)` form. Running the file with `racket file.rkt` assembles and writes the output. See [linker.md](linker.md) for that form.

Because it is Racket, you can use any Racket code to generate assembly: `define` for constants, `for` loops that emit blocks, functions that return blocks, and `syntax-parse` macros. Tables such as sines and palettes are normally computed in Racket at assembly time.

## Reader syntax

| Syntax       | Meaning                                                                                   |
|--------------|-------------------------------------------------------------------------------------------|
| `{ ... }`    | An assembly block. Each source line inside it becomes one `arm-line`                      |
| `@expr`      | Immediate operand. `@` reads as `#:immediate`; the next datum is any Racket expression    |
| `$FF`        | Hex number. Underscores are not allowed (the reader stops at the first non-hex character) |
| `%1010_0001` | Binary number. Underscores are allowed as separators                                      |
| `;`          | Comment to end of line                                                                    |
| `£`          | Reads as `#:indirect`, which nothing in the expander currently uses                       |

Inside a block, a line ends at a newline that is outside parentheses. A parenthesised Racket expression on its own line is evaluated as Racket, so `(PUSH x0)`, `(for ...)` and `(write-value-32 ...)` work as lines. Blocks nest: a Racket expression inside a block can contain further `{ }` blocks.

## Lines, labels and constants

* `:name` defines a label at the current location. It can share a line with an instruction (`:hang wfe`) or a data expression (`:fb (write-value-32 0)`).
* `name:` references the only label called `name`. It errors if there are two or more.
* `name+` references the nearest `:name` at or after this point, and `name-` the nearest at or before it. Use these for repeated local labels such as `:loop` or `:wait`.
* Labels whose first character after `:` is upper case are global linker symbols. Read [linker.md](linker.md) before naming anything with a capital.
* `name = expr` inside a block defines a Racket constant (`mbox-status = $18`).
* `/= n` aligns the location to a multiple of `n` bytes, padding with zeros.
* Labels must be inside a `{ }` block.

## Data directives

* `(write-value b)` emits one byte
* `(write-values list)` emits a (flattened) list of bytes
* `(write-value-16 n)`, `(write-value-32 n)`, `(write-value-64 n)` emit little-endian values
* `(write-ascii "s")` and `(write-ascii-nullterm "s")` emit strings
* `(reserve n)` emits `n` zero bytes
* `(data b ...)` emits bytes

## Registers

* `x0` to `x30`, `w0` to `w30`, `sp`, `xzr`, `wzr`
* A `w` register selects the 32 bit form of instructions that have one.
* `mov sp xN` and `mov xN sp` are rewritten to `add` automatically.

## Supported instructions

The opcode list in `expander.rkt` (the `opcode` syntax class and the `create-opcode-metadata` table) is the source of truth. As of this writing:

* Arithmetic: `add`/`sub` (register, or unsigned 12 bit immediate), `adds`/`subs` (same forms, set flags), `mul`, `madd`/`msub` (`madd x0 x1 x2 x3`), `mneg`, `smulh`/`umulh` (64 bit only), `udiv`, `sdiv`, `neg`, `cmp` and `cmn` (register, or unsigned 12 bit immediate), `csel xD xN xM cond`, `csinc xD xN xM cond`, `cset xD cond` (the inverse of the condition, so `cset x0 eq` sets x0 to 1 when the Z flag is set), `csinv`, `csneg`, `cinc`, `cneg` (`csel`-family, for example `cinc x0 x1 eq`), `adc`, `sbc` (register form, no shift), `smull`, `umull` (64 bit product of two 32 bit registers, for example `smull x0 w1 w2`). `cset`, `cinc` and `cneg` reject `al` (and `nv`, which the condition syntax does not accept), as ARM does not encode them: `cset x0 al` fails at assembly time
* Logic: `and`, `orr`, `eor`, `ands` with a register or a bitmask immediate (`and x0 x1 @$FF`), `bic`, `bics`, `orn` (register forms, with an optional shift), `mvn` (`mvn x0 x1`, or with a shift, `mvn x0 x1 LSL @2`), and `tst` (register form only). A bitmask immediate must be a run of ones, rotated and repeated across the register (for example `@$FF`, `@$00FF00FF`, `@$5555555555555555`). All zeros, all ones, and values that do not repeat (for example `@$1234`) fail at assembly time. `and w0 w1 @-2` is accepted for 32 bit forms
* Shifts: `lsl`, `lsr`, `asr`, `ror` with an immediate amount (`lsl x0 x1 @4`) or a register amount (`lsl x0 x1 x2`)
* Shifted register operands: `add`, `adds`, `sub`, `subs`, `and`, `ands`, `orr`, `eor`, `bic`, `bics`, `orn` take a final `LSL`, `LSR` or `ASR` with an immediate amount (`add x0 x1 x2 LSL @4`). The amount must be 0 to 63 for x registers and 0 to 31 for w registers. `ROR` is accepted only on the logical forms (`and`, `ands`, `orr`, `eor`, `bic`, `bics`, `orn`), for example `and x0 x1 x2 ROR @4`. `add`, `adds`, `sub`, `subs` with `ROR` are rejected, as ARM does not encode them
* Extended register operands: `add`, `adds`, `sub`, `subs` take `UXTB`, `UXTH`, `UXTW`, `UXTX`, `SXTB`, `SXTH`, `SXTW` or `SXTX` with an optional amount 0 to 4 (`add x0 x1 w2 SXTW @2`). Keywords are uppercase. The extend `UXTB`, `UXTH`, `UXTW`, `SXTB`, `SXTH` and `SXTW` take a W `Rm`. `UXTX` and `SXTX` take an X `Rm`. The width of `Rm` must match, so `add x0 x1 w2 UXTX` fails at assembly time* Bitfield: `ubfm`, `sbfm` (two immediates: `ubfm x0 x1 @4 @7`), and the aliases `uxtb`/`uxth` (32 bit registers only), `ubfx`/`sbfx` (`ubfx x0 x1 @lsb @width`), `bfi` (`bfi x0 x1 @lsb @width`, the lsb and width must fit the register), `extr` (`extr x0 x1 x2 @lsb`)
* Moves: `mov` (register, or 16 bit immediate), `movz`, `movn`, `movk` (with optional `LSL @16/32/48`)
* Bit operations: `clz`, `rbit`, `rev16`, and `rev32` (64 bit only)
* Conditional compare: `ccmp x0 x1 @nzcv cond` (register), or `ccmp x0 @imm @nzcv cond` (5 bit immediate). `nzcv` is 0 to 15 and the immediate is 0 to 31; out of range values fail at assembly time
* Sign extend: `sxtb`, `sxth`, `sxtw`
* Branches: `b`, `bl`, `b.eq` ... `b.al`, `cbz`, `cbnz`, `br xN`, `blr xN`, `ret` (no operand, same as `ret x30`) or `ret xN`
* Address: `adr xN label:`
* Loads and stores: `ldr`, `str`, `ldrb`, `strb`, `ldrh`, `strh`, `ldp`, `stp`, `stur`, `ldur`, `ldurb`, `ldurh`, and `ldr xN label:` (literal)
* Sign extending loads: `ldrsw` (x destination only), `ldrsb` and `ldrsh` (x or w destination), with `[xN @off]` or `[xN]`. The unscaled forms `ldursb`, `ldursh` (x or w destination) and `ldursw` (x destination only) take a signed 9 bit offset with no scaling
* Exclusive and ordered: `ldxr`, `ldaxr`, `ldar`, `stlr` (`ldxr x0 [x1]`), and `stxr`, `stlxr` (`stxr w0 x1 [x2]`, the status register is W). The base is always an X register and has no offset
* Prefetch: `prfm @op [xN @off]`, where `@op` is the prefetch operation and the offset is unsigned and a multiple of 8
* Exceptions and barriers: `svc`, `hvc`, `smc`, `brk`, `hlt` (16 bit immediate, `@n`), `dmb`, `dsb`, `isb` (barrier option, `@n`, for example `dsb @15` is `dsb sy`)
* System: `mrs`, `msr`, `nop`, `wfe`, `wfi`, `sev`, `eret`
* Single precision floating point (`s` registers): see [Floating point](#floating-point-single-precision)

Branch and literal reach: `b`/`bl` ±128MB; `b.cond`, `cbz`, `cbnz`, `adr` and `ldr` literal ±1MB.

### Addressing modes

| Form                 | Mode                          | Notes                                                     |
|----------------------|-------------------------------|-----------------------------------------------------------|
| `ldr x0 [x1 @16]`    | Unsigned offset               | Offset must be a non-negative multiple of the access size |
| `ldr x0 [x1]`        | Same, offset 0                |                                                           |
| `ldr x0 [x1] @8`     | Post-index (base += 8 after)  | Signed 9 bit                                              |
| `str x0 [sp @-16] !` | Pre-index (base += -16 first) | Signed 9 bit                                              |
| `ldp x0 x1 [sp @16]` | Signed offset pair            | Scaled by the register size                               |
| `ldp x0 x1 [sp] @16` | Post-index pair               | Signed 7 bit, scaled                                      |
| `stp x0 x1 [sp @-16] !` | Pre-index pair             | Signed 7 bit, scaled                                      |
| `ldr x0 [x1 x2]`     | Register offset               | The optional `LSL @n` must be 0, or log2 of the access size |
| `ldur x0 [x1 @-8]`   | Unscaled signed offset        | Signed 9 bit, not scaled, so any byte offset works        |
| `ldrsw x0 [x1 @8]`   | Sign extending offset         | Scaled by the access size (4, 1 or 2 bytes)               |
| `ldursw x0 [x1 @-8]` | Unscaled sign extending offset | Signed 9 bit, not scaled                                 |
| `ldr x0 label:`      | PC-relative literal           |                                                           |

Parentheses and brackets are interchangeable: `(x1 @8)` is the same as `[x1 @8]`.

### Not supported, and what to do instead

* Logical immediates that are not a repeating bitmask (`and x0 x1 @$1234`): load the mask into a register, then use the register form.
* Register-offset addressing with an `UXTW`/`SXTW`/`SXTX` extend: `add` the offset into a temporary register first. Plain `[x1 x2]` and `[x1 x2 LSL @n]` are supported.
* Shifted register operands with `ROR` on `add`, `adds`, `sub`, `subs` (not encodable in ARM), and SIMD or double precision register-offset loads: none are available.
* Negative unsigned offsets (`ldr x0 [x1 @-8]`): use `ldur` (signed 9 bit offset, no scaling), or adjust the base, or use pre-index or post-index.
* `mov x0 @-1` or `mov` with a value above 16 bits: use `load-immediate` (below).
* `mov` with `LSL`: use `movz` or `movk`.
* `tbz`, `smulh`/`umulh` with 32 bit registers, double precision floating point and SIMD: none are available. Express them with the supported instructions, or emit a raw opcode.

### Floating point (single precision)

Registers are `s0` to `s31`. Only 32 bit floats are supported, so `d` registers and `v` registers are not available.

| Form                              | Encoding                                  |
|-----------------------------------|-------------------------------------------|
| `fadd sd sn sm`, `fsub`, `fmul`, `fdiv` | three-register arithmetic           |
| `fabs sd sn`, `fneg`, `fsqrt`     | one-register-source operations            |
| `fmov sd sn`                      | register copy                             |
| `fmov sd wn`, `fmov wd sn`        | move bits between an integer and float register |
| `fmov sd @1.5`                    | immediate. Only values the 8 bit FP immediate can encode (`1.0`, `1.5`, `2.25`, `-0.5`, and so on). `0.0` is not encodable |
| `fcmp sn sm`, `fcmp sn @0`        | compare. Only `@0` is accepted as the immediate form |
| `scvtf sd xn`, `scvtf sd wn`      | signed integer to float                   |
| `fcvtzs wd sn`, `fcvtzs xd sn`    | float to signed integer, rounding toward zero |
| `ldr sd [xn @16]`, `str sd [xn]`  | 32 bit float load and store, unsigned offset |
| `ldr sd [xn xm]`, `ldr sd [xn xm LSL @2]`, `str sd [xn xm]` | 32 bit float register-offset load and store. The optional `LSL @2` sets the scaled bit; the index must be an X register |

Note: on QEMU raspi3b the platform runs at EL2, and the FP arithmetic, `fmov` immediate, `fcvtzs`, `fneg` and `str`/`ldr` used in a check ran there with no FP enable. Check the trap bits before using FP from EL1 or EL0.

### Raw opcodes for system instructions

`mrs`/`msr` only know `mpidr_el1`, `vbar_el1`, `esr_el1`, `elr_el1`, `sctlr_el1`, `hcr_el2`, `scr_el3`, `spsr_el3`, `elr_el3`, `daifset` and `daifclr`, and not every combination works. Emit everything else as a 32 bit word with `(write-value-32 opcode)`. The low 5 bits are the register number, so `(+ opcode 5)` targets `x5`.

| Instruction          | Opcode      |
|----------------------|-------------|
| `mrs x0, CurrentEL`  | `$d5384240` |
| `mrs x0, cntpct_el0` | `$d53be020` |
| `mrs x0, cntfrq_el0` | `$d53be000` |
| `dsb sy`             | `$d5033f9f` |
| `isb`                | `$d5033fdf` |
| `dc civac, x0`       | `$d50b7e20` |
| `dc cvac, x0`        | `$d50b7a20` |
| `dc ivac, x0`        | `$d5087620` |
| `msr vbar_el1, x0`   | `$d518c000` |
| `msr tcr_el1, x0`    | `$d5182040` |
| `msr mair_el1, x0`   | `$d518a200` |
| `msr ttbr0_el1, x0`  | `$d5182000` |
| `mrs x0, sctlr_el1`  | `$d5381000` |
| `msr sctlr_el1, x0`  | `$d5181000` |
| `mrs x0, esr_el1`    | `$d5385200` |
| `mrs x0, elr_el1`    | `$d5384020` |
| `mrs x0, far_el1`    | `$d5386000` |
| `tlbi vmalle1`       | `$d508871f` |

QEMU starts the kernel at EL2. The EL2 version of an EL1 register differs only in the op1 field, so `lib/platform.rkt` and `emu_clean.rkt` (pisemble-wolf3d repo) define `(el-op base)`, which adds `4 << 16`. For example, `(write-value-32 (el-op $d518c000))` is `msr vbar_el2, x0`, and `(el-op $d508871f)` is `tlbi alle2`.

## Register renaming

The `register` syntax class expands identifiers before checking them, so Racket rename transformers work anywhere a register is expected: in instructions and in any macro that uses the class.

* `(regs ([name reg] ...) body ...)` from `lib/stack.rkt` binds names for the body.
* `subr` takes the same alias list (below).
* An alias carries its width. `[tmp x13]` cannot be used where a `w` register is needed, so code often falls back to `w13` for byte and word accesses. Whether two aliases (`[tmp x13] [tmp-w w13]`) can name the same register has not been tested.

## Helper libraries in lib/

These are ordinary `#lang pisemble` modules. Require them by relative path.

### stack.rkt

* `(PUSH r ...)` and `(POP r ...)` use 16 bytes of stack per register (`str r [sp @-16] !`), so `sp` stays 16 byte aligned. Pop in reverse order.
* `(preserve [r ...] body)` pushes, runs the body, pops.
* `(subr name ([alias reg] ...) [saved-reg ...] { body })` defines `:name`, pushes `x30` and the saved registers, runs the body with the aliases, pops and emits `ret x30`. Call it with `bl name:`. Arguments and results are passed in registers by convention. An empty alias list `()` and an empty saved list `[]` are both fine.

### struct.rkt

* `(load-immediate reg value)` loads any constant up to 64 bits using `mov` and `movk`.
* `(/struct name ([field size] ...))` defines field offsets and accessor macros.

### expr.rkt

Conditions are written `(x0 lt x1)` or `(x0 ge @10)` using the condition names `eq ne cs cc mi pl vs vc hi ls ge lt gt le al`, and can be combined with `(/and c ...)` and `(/or c ...)`.

* `(/if cond then else)`, `(/when cond then)`, `(/cond ([cond expr] ... [else expr]))`
* `(/whilenz reg body)`, `(/whilez reg body)`, `(/while cond body)`, `(/for init cond update body)`

> [!WARNING]
> All the loop macros test at the bottom, so the body always runs at least once. `(/whilenz x3 ...)` with `x3 = 0` loops about 2^64 times.

The macros generate labels like `:then-start_0` and branch to them with `+` and `-` references. The counter lives in the module being compiled, so the same generated name can appear in code expanded from another module. Relative references usually still pick the right label, but deeply nested control flow expanded from another module's macro is a place to look if a branch goes somewhere odd.

### periph.rkt

* `PERIPH-BASE` is `$3F000000` for the Pi 3.
* `(periph-addresses)` emits 8 byte aligned data labels such as `:AUXB` and `:GPFSEL1`. They are capitalised, so they are global; include them once per image.
* `(init-uart)` sets up the mini UART. It needs `periph-addresses` in the same image.
* `(create-send-char)` defines `send-char` (byte in `w0`). It loads `AUXB:` so it also needs `periph-addresses`.
* `(debug-str "text" newline?)` sends a string using the UART protocol in [qemu-and-debugging.md](qemu-and-debugging.md). It needs a `send-char` routine in the same program.
* `debug-reg` (send a register as a number) and `debug-str-reg` (a string followed by a register) are not in `periph.rkt`. Copies live in `lib/platform.rkt` and in `emu_clean.rkt` in the pisemble-wolf3d repo.

## Writing macros that emit assembly

The pattern used throughout the repo:

```racket
(define-syntax-parser fill-blocks
  [(_ n:expr)
   #'{
      (load-immediate x4 n)
    :fill
      str x2 [x0] @8
      sub x4 x4 @1
      cbnz x4 fill-
     }])
```

* Use the `register`, `label`, `label-targ`, `immediate` and `condition` syntax classes exported for syntax by `expander.rkt` to validate arguments.
* Use relative label references (`fill-`) inside macros, since the macro may be expanded several times.
* A plain function that returns a block (`(define (init-uart) { ... })`) also works when no syntax checking is needed.
