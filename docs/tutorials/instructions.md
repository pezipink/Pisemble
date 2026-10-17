---
title: AArch64 Instruction Families in Pisemble
description: Learn the instruction families that Pisemble supports, read each line with confidence, and see worked examples of the less common instructions such as madd, csel, bitfield moves, clz, and pair loads.
ms.date: 2026-10-17
ms.topic: tutorial
---

## What you will learn

* How to read a Pisemble instruction line: the opcode, the destination, the sources, and the immediates.
* The main instruction families and what each one is for.
* Basic examples from each family.
* Detailed examples of the less common instructions, with a short explanation of what each bit pattern does.
* The forms that Pisemble does not accept, and what to use instead.

You will write the code yourself. The snippets here show one instruction or a small group of them. They are not complete programs.

## Before you start

Read these project notes first:

* [language.md](../../agent-context/language.md) for the supported instruction list, the addressing table, and the unsupported forms. This tutorial follows that file.
* [bouncing.rkt](../../demo/bouncing.rkt) for a working program that uses several of these instructions. Section 8 walks through part of it.

Every snippet in this tutorial assembled with the current Pisemble expander. The encodings for the uncommon instructions match the expected values in [instruction-test.rkt](../../instruction-test.rkt). Behaviour on real hardware is a separate question, and the notes say to check it under QEMU before you trust it.

## 1. How to read an instruction line

Pisemble writes an instruction as the opcode followed by its operands, in the same order as the ARM reference manual, with the destination first:

```racket
add x0 x1 x2        ; x0 = x1 + x2
```

Three operand shapes come up again and again:

* **Registers**: `x0` to `x30` are 64-bit, `w0` to `w30` are the low 32 bits of the same register. `sp` is the stack pointer and `xzr` / `wzr` read as zero and discard writes.
* **Immediates**: marked with `@`. `@10` is decimal, `@$FF` is hex, and `@%1010` is binary.
* **Shifts and extends on a register operand**: `LSL @4` after a register shifts it before the operation, as in `add x0 x1 x2 LSL @4`.

Two rules matter most:

* A write to a `w` register clears the top 32 bits of the matching `x` register. `add w0 w1 w2` leaves the top half of `x0` at zero.
* The destination comes first, even for stores. `str x0 [sp @8]` stores `x0` at `sp + 8`.

The rest of this tutorial uses these shapes without repeating the explanation.

## 2. The instruction families

| Family | Instructions | What they do |
| ------ | ------------ | ------------ |
| Move | `mov`, `movz`, `movk`, `movn` | Put a constant in a register, one 16-bit piece at a time |
| Arithmetic | `add`, `sub`, `adds`, `subs`, `neg`, `adc`, `sbc` | Add and subtract, with optional flag setting |
| Multiply and divide | `mul`, `madd`, `msub`, `mneg`, `smull`, `umull`, `smulh`, `umulh`, `udiv`, `sdiv` | Products, multiply-accumulate, widening and high halves, and division |
| Compare and test | `cmp`, `cmn`, `tst`, `ccmp` | Set the flags without keeping a result, or chain two conditions |
| Logical | `and`, `ands`, `orr`, `eor`, `bic`, `bics`, `orn` | Bitwise operations, including clear and or-not |
| Shifts and rotates | `lsl`, `lsr`, `asr`, `ror`, `extr` | Move bits left and right, and rotate |
| Bitfields and extends | `ubfm`, `sbfm`, `ubfx`, `sbfx`, `bfi`, `uxtb`, `uxth`, `sxtb`, `sxth`, `sxtw` | Pull out, place, and widen bitfields |
| Bit counting and reversal | `clz`, `rbit`, `rev16` | Count leading zeros and reorder bits and bytes |
| Conditional select | `csel`, `csinc`, `csinv`, `csneg`, `cset`, `cinc`, `cneg` | Choose a value by condition without a branch |
| Branches | `b`, `bl`, `br`, `blr`, `ret`, `b.eq` and the other `b.cond`, `cbz`, `cbnz` | Change the program counter |
| Loads and stores | `ldr`, `str`, `ldrb`, `strb`, `ldrh`, `strh`, `ldur`, `ldurb`, `ldurh`, `stur`, `ldrsb`, `ldrsh`, `ldrsw`, `ldp`, `stp`, `adr`, `prfm` | Move data between registers and memory, and hint the cache |
| Exclusive and ordered access | `ldxr`, `stxr`, `ldaxr`, `stlxr`, `ldar`, `stlr` | Read-modify-write loops and acquire and release ordering |
| Floating point | `fmov`, `fadd`, `fsub`, `fmul`, `fdiv`, `fneg`, `fabs`, `fsqrt`, `fcmp`, `scvtf`, `fcvtzs` | Single precision (`s` registers) only |
| System and barriers | `mrs`, `msr`, `nop`, `wfe`, `wfi`, `sev`, `dmb`, `dsb`, `isb`, `svc`, `hvc`, `brk`, `hlt`, `eret` | Special registers, waiting, barriers, and exceptions |

## 3. Moving constants

A 64-bit constant does not fit in one instruction. You build it in pieces: `movz` sets one 16-bit piece and clears the rest, and `movk` sets another piece and keeps the others.

```racket
movz x0 @$BEEF          ; x0 = $000000000000BEEF
movk x0 @$DEAD LSL @16  ; x0 = $00000000DEADBEEF
```

`movn` writes the inverse of its immediate. `movn x0 @0` gives `x0 = -1`, which is all ones. Pisemble does not accept `mov x0 @-1`, so use `movn` for negative constants.

```racket
mov x0 x1       ; register to register
mov x0 @10      ; small constant, the assembler picks the encoding
```

Logical immediates such as `and x0 x1 @$FFFFFFFF00000000` are a special case, covered in section 5.

## 4. Arithmetic, multiply, and divide

Basic arithmetic is what you expect:

```racket
add x0 x1 @4         ; x0 = x1 + 4
add x0 x1 x2 LSL @4  ; x0 = x1 + (x2 << 4)
sub x0 x1 x2         ; x0 = x1 - x2
neg x0 x1            ; x0 = -x1
```

The `s` forms also set the condition flags, so you can branch on the result. `adds` and `subs` set them the same way as `cmp`, which section 6 covers.

### Multiply-accumulate: `madd`, `msub`, `mneg`

`madd` multiplies two registers and adds a third, all in one instruction:

```racket
madd x0 x1 x2 x3   ; x0 = x3 + x1 * x2
msub x0 x1 x2 x3   ; x0 = x3 - x1 * x2
mneg x0 x1 x2      ; x0 = -(x1 * x2)
```

A common use is computing an address inside a table. If each row of a 2D array has `stride` bytes, the address of element `(row, col)` is `base + row * stride + col`. With `x1` holding `row`, `x2` holding `stride`, and `x3` holding `base`:

```racket
madd x0 x1 x2 x3   ; x0 = base + row * stride
add x0 x0 x4       ; x0 = base + row * stride + col
```

`madd` does the multiply and the add in one step, so the loop has one instruction fewer than the separate `mul` and `add` version.

The `w` form, `madd w0 w1 w2 w3`, works on 32-bit values and writes `w0`, which zeroes the top half of `x0`.

### The high half of a product: `smulh` and `umulh`

`mul` gives the low 64 bits of a product. If you need the high 64 bits, use `smulh` for signed values or `umulh` for unsigned values:

```racket
umulh x0 x1 x2   ; x0 = high 64 bits of (x1 * x2), unsigned
mul   x3 x1 x2   ; x3 = low 64 bits of the same product
```

Together, `umulh` and `mul` give you the full 128-bit product. This is useful for fixed-point arithmetic, where you multiply two 64-bit values and then keep the middle bits. Pisemble only supports the 64-bit forms of `smulh` and `umulh`.

### Division: `udiv` and `sdiv`

`udiv` divides unsigned values and `sdiv` divides signed values. Both round toward zero and leave the remainder out.

```racket
udiv x0 x1 x2   ; x0 = x1 / x2, unsigned
```

The remainder is not computed for you. To get `x1 mod x2`, multiply the quotient back and subtract:

```racket
udiv x0 x1 x2   ; x0 = quotient
mul  x0 x0 x2   ; x0 = quotient * divisor
sub  x0 x1 x0   ; x0 = x1 - quotient * divisor, which is the remainder
```

The ARM manual says that dividing by zero gives a result of zero and does not raise an exception. Check this under QEMU before you depend on it. The bouncing demo in section 8 divides only by a value it has already checked.

## 5. Logical operations and bitmask immediates

`and`, `orr`, and `eor` (exclusive or) work on registers, and each has an immediate form:

```racket
and x0 x1 x2                 ; register form
and x0 x1 @$FFFFFFFF00000000 ; immediate form
eor x0 x1 x2
orr x0 x1 x2
```

Not every value fits in the immediate field. The 64-bit value must be a run of ones, possibly rotated, that repeats across the register at some element size of 2, 4, 8, 16, 32, or 64 bits. These assemble:

```racket
and x0 x1 @$FFFFFFFF00000000     ; upper 32 bits set
and x0 x1 @$0F0F0F0F0F0F0F0F     ; 4-bit pattern repeated
and x0 x1 @$00FF00FF00FF00FF     ; 16-bit pattern repeated
```

These do not assemble, because the value does not repeat across all 64 bits:

```racket
and x0 x1 @$00FF00FF    ; fails: the pattern is not repeated to 64 bits
```

The rule is strict, so when you need a mask that does not fit, load it into a register first with `movz`, `movk`, or a literal load, and use the register form.

### Testing bits: `tst` and `ands`

`tst x1 x2` is `ands xzr x1 x2`. It sets the flags from `x1 AND x2` and throws the result away. Use it to ask whether any bit in a mask is set:

```racket
ands xzr x1 @8   ; Z is set if bit 3 of x1 is clear
```

Use `ands` with a real destination when you need the masked value as well as the flags.

## 6. Compare and conditional select

`cmp x1 x2` is `subs xzr x1 x2`. It sets the flags for `x1 - x2` and keeps no result. `cmn x1 @1` is `adds xzr x1 @1`, so `cmn x1 @1` sets Z exactly when `x1` is -1.

The conditions are the usual ones: `eq`, `ne`, `gt`, `ge`, `lt`, `le` for signed values, and `hi`, `hs`, `lo`, `ls` for unsigned values. Pick the signed or unsigned set to match your data.

### `csel`: pick one of two values

`csel xD xN xM cond` writes `xN` to `xD` if the condition holds, and `xM` otherwise. There is no branch to take or skip.

```racket
cmp  x1 x2
csel x0 x1 x2 gt   ; x0 = max(x1, x2) for signed values
```

The absolute value follows the same pattern:

```racket
neg  x2 x1          ; x2 = -x1
cmp  x1 @0
csel x0 x1 x2 ge   ; x0 = |x1|
```

### `csinc` and `cset`

`csinc xD xN xM cond` writes `xN` if the condition holds, and `xM + 1` otherwise. This lets you build a value from a condition:

```racket
cmp   x1 x2
csinc x0 x1 x2 eq   ; x0 = x1 if equal, x2 + 1 if not
```

`cset` is the common case where you want 0 or 1. `cset x0 eq` writes 1 to `x0` when the condition holds and 0 when it does not:

```racket
cmp  x1 @0
cset x0 eq          ; x0 = 1 if x1 was zero, else 0
```


## 7. Bitfields, extends, and bit counting

### Bitfields: `ubfm`, `ubfx`, `sbfm`, `sbfx`

The bitfield family copies a run of bits. Pisemble has the raw forms `ubfm` and `sbfm`, which take the ARM manual's `immr` and `imms` values, and the friendlier `ubfx` and `sbfx`, which take the lowest bit and the width.

```racket
ubfx x0 x1 @8 @8     ; x0 = bits 15..8 of x1, zero extended
sbfx w0 w1 @4 @4     ; w0 = bits 7..4 of w1, sign extended
```

`ubfx x0 x1 @8 @8` means: start at bit 8 and take 8 bits. It is the same encoding as `ubfm x0 x1 @8 @15`. The second number in `ubfm` is the top bit, which is easy to mix up with the width in `ubfx`.

`ubfm` with `@4 @7` extracts bits 7 to 4 into the bottom of the destination, which is the same field as `ubfx x0 x1 @4 @4`.

Use `sbfx` when the field is signed. It fills the bits above the field with the field's top bit, so a 4-bit value of `%1111` becomes -1.

### Extends: `uxtb`, `sxtb`, `sxth`, `sxtw`

These widen a narrow value in place:

```racket
uxtb w0 w1   ; zero extend the low byte of w1 into w0
sxtb x0 x1   ; sign extend the low byte of x1 into x0
sxth x0 x1   ; sign extend the low halfword of x1 into x0
sxtw x0 x1   ; sign extend the low word of x1 into x0
```

`uxtb w0 w1` encodes the same as `ubfm w0 w1 @0 @7`.

### Counting bits: `clz`, `rbit`, `rev16`

`clz` counts leading zeros. Use it to find the highest set bit, which gives you `floor(log2(x))` for a nonzero value:

```racket
clz  x0 x1          ; x0 = number of leading zeros in x1
mov  x2 @63
sub  x0 x2 x0       ; x0 = index of the highest set bit of x1
```

`clz` of zero is 64, so check for zero first if your input can be zero.

Pisemble does not have a count-trailing-zeros instruction, so reverse the bits and then count leading zeros:

```racket
rbit x0 x1          ; reverse all 64 bits
clz  x0 x0          ; leading zeros of the reversed value = trailing zeros of x1
```

`rev16` swaps the two bytes inside each 16-bit half of a register. It is the usual way to convert between big endian and little endian data within 16-bit units. Pisemble does not accept `rev`, so there is no single instruction for a full byte swap.

## 8. Worked example: the bouncing ball

[bouncing.rkt](../../demo/bouncing.rkt) puts several of these instructions together. The `bounce-axis` subroutine returns a coordinate that moves in a straight line and reflects at each end of its range. Here is the core of it, with the setup lines and the comments removed:

```racket
mul position frame speed      ; position = frame * speed
add position position phase   ; position += phase
lsl period range @1           ; period = 2 * range
udiv x0 position period       ; x0 = position / period
mul x0 x0 period              ; x0 = (position / period) * period
sub position position x0      ; position = position mod period
cmp position range
b.ls in-first-half+           ; if position <= range, it is on the way out
sub position period position  ; on the way back: reflect the position
:in-first-half
add x0 margin position        ; return margin + position
```

The steps follow the families from earlier:

* `mul` and `add` build the linear position. This is the same pattern as `madd` in section 4.
* `udiv` and `mul` together compute `position mod period`. The subtraction at the end gives the remainder, as in section 4.
* `cmp` and `b.ls` branch on an unsigned comparison. The `ls` condition is unsigned, which is correct here because `position` is never negative.
* The reflection uses `sub` with the registers in a different order, which is one instruction and no branch.

If you want to try a variation, change `speed` or `phase` and think about what `udiv` gives you when `position` is smaller than `period`.

## 9. Loads and stores

### Basic loads and stores

```racket
ldr  x0 [x1 @8]       ; load 64 bits from x1 + 8
str  x0 [x1 @8]       ; store 64 bits to x1 + 8
ldrb w0 [x1 @3]       ; load one byte, zero extended
strh w0 [x1 @2]       ; store the low halfword
```

The offset in `ldr` and `str` is scaled by the access size, so `@8` on an `ldr` is encoded as 1. You write the byte offset, and the assembler does the scaling.

### Unscaled offsets: `ldur` and `stur`

The scaled form cannot take a negative offset. Use the unscaled forms when the offset is negative or not a multiple of the access size:

```racket
ldur x0 [x1 @-8]      ; load 64 bits from x1 - 8
stur x0 [x1 @-8]      ; store to x1 - 8
ldurb w0 [x1 @-1]     ; load one byte from x1 - 1
```

### Sign-extending loads: `ldrsb`, `ldrsh`, `ldrsw`

These load a narrow value and sign extend it:

```racket
ldrsb w0 [x1 @3]      ; load a byte and sign extend it into w0
ldrsh x0 [x1 @4]      ; load a halfword and sign extend it into x0
ldrsw x0 [x1 @8]      ; load a word and sign extend it into x0
```

`ldrsb` and `ldrsh` accept either a `w` or an `x` destination. `ldrsw` takes only an `x` destination, and Pisemble rejects a `w` one. The offsets are byte offsets.

### Pair loads and stores: `ldp`, `stp`

`ldp` and `stp` move two 64-bit values in one instruction. They are the usual way to save and restore registers around a subroutine call. Pisemble has three addressing forms:

```racket
ldp x0 x1 [sp @16]      ; offset form: load from sp + 16
stp x19 x20 [sp @-16] ! ; pre-index: sp = sp - 16, then store at the new sp
ldp x19 x20 [sp] @16    ; post-index: load from sp, then sp = sp + 16
```

The pre-index form with `!` is the standard way to push a pair, and the post-index form is the matching pop. `ldp x2 x3 [x1]` with no offset is not accepted, so write `[x1 @0]` if you want a zero offset.

### Literal loads and `adr`

`ldr` can load a value from a label placed in the code. `adr` gives the address of a label as a register value.

```racket
ldr x0 big:      ; load the 64 bit value stored at label big
adr x1 big:      ; x1 = address of label big

:big
  (write-value-64 $0123456789ABCDEF)
```

The label must be within 1MB of the instruction.

## 10. Branches

`b` and `bl` reach ±128MB. `b.cond`, `cbz`, and `cbnz` reach ±1MB. Use `cbz` and `cbnz` for a single register test, and `b.cond` after a `cmp`:

```racket
cbz  x0 done+        ; branch if x0 is zero
cmp  x1 x2
b.lt less+           ; branch if x1 < x2, signed
```

`br` jumps to an address in a register and `blr` calls one. `ret` returns through `x30` by default.

## 11. Floating point

Pisemble supports single precision only, using the `s` registers. The arithmetic follows the usual pattern:

```racket
fmov s1 @2.5        ; the value must be one the hardware can encode
scvtf s0 x1         ; convert a signed integer to float
fmul s0 s0 s1       ; s0 = s0 * s1
fcvtzs x2 s0        ; convert float to integer, rounding toward zero
fsqrt s0 s0         ; square root
fcmp s0 @0          ; compare with zero; fcmp s0 s1 compares two registers
```

`fmov s1 @2.5` works, but a value the hardware cannot encode fails at assembly time. Floating point needs the FPU enabled at the exception level you run at. The notes say it runs under QEMU at EL2, but check the trap bits before you use it at EL1 or EL0.

## 12. System instructions, barriers, and exceptions

The instructions in this family are for the kernel, not everyday arithmetic.

```racket
mrs x0 mpidr_el1      ; read a system register
dsb @15               ; data synchronization barrier, full system
dmb @15               ; data memory barrier, full system
isb @15               ; instruction synchronization barrier, full system
wfe                   ; wait for an event
sev                   ; send an event to other cores
```

Barriers need an option. `isb` on its own is rejected, so write `isb @15`, the full-system form. Use `dsb` after writing to memory that another core or a device reads, and `isb` after changing code.

`svc`, `hvc`, `brk`, and `hlt` raise an exception or stop the core. `brk @$1234` is encoded as expected, and `hlt @0` is accepted. What happens next depends on the exception vectors you have installed, so only use these once you have set them up. `eret` returns from an exception and needs the matching saved state. Check their behaviour under QEMU before you rely on them.

## 13. Uncommon instructions in detail

This section goes deeper on instructions the earlier sections touched only briefly. Every snippet here assembles. Where the result depends on how the hardware or QEMU behaves at run time, the text says so.

### Conditional compare: `ccmp`

`ccmp` compares only when an earlier condition holds. When that condition fails, it sets the flags to a value you choose. This lets you test two conditions and then use one flag result:

```racket
cmp  x0 @5
ccmp x1 @7 @0 eq   ; if x0 == 5, compare x1 with 7; otherwise force the flags to "not equal"
cset x2 eq         ; x2 = 1 only when x0 == 5 and x1 == 7
```

The third operand is the value of the N, Z, C, and V flags to use when the earlier condition fails. `@0` clears all four, so `eq` is false.

### Exclusive access: `ldxr` and `stxr`

These two form a read-modify-write loop. `ldxr` loads a value and marks the address. `stxr` stores only if the mark is still there, and writes its status register: 0 for success, 1 for failure.

```racket
adr  x0 counter:     ; x0 = address of the counter
:retry
ldxr x1 [x0]         ; load the value and mark the address
add  x1 x1 @1
stxr w2 x1 [x0]      ; w2 = 0 if the store succeeded, 1 if the mark was lost
cbnz w2 retry-       ; try again after a lost mark

:counter
  (write-value-64 $0)
```

The base register takes no offset, so `ldxr x0 [x1 @8]` is rejected. Compute the address into a register first. A failed `stxr` does not write, so the loop reads the value again and retries.

> [!WARNING]
> This loop assembles, but I have not run it on QEMU or hardware. Test it on one core before you use it with several, and see the warning in the multicore tutorial.

### Acquire and release: `ldar` and `stlr`

`ldar` is an acquire load. Later memory accesses cannot move before it. `stlr` is a release store. Earlier memory accesses cannot move after it.

```racket
ldar x0 [x1]         ; acquire load
stlr x0 [x1]         ; release store
```

Both take a base register and no offset. The exclusive versions, `ldaxr` and `stlxr`, combine the ordering with the mark from `ldxr` and `stxr`.

### Add and subtract with carry: `adds`, `adc`, `subs`, `sbc`

A 64-bit add can overflow into a carry. To add two 128-bit numbers, add the low halves with `adds`, which sets the carry flag, then add the high halves with `adc`, which adds the carry in:

```racket
adds x0 x1 x2        ; low halves, sets the carry flag
adc  x3 x4 x5        ; high halves plus the carry
```

Here `x4:x1` is one 128-bit number, with `x4` as the high half, and `x5:x2` is the other. The result is `x3:x0`. `subs` and `sbc` do the same for subtraction, using the borrow in place of the carry.

### Bit clear and or-not: `bic`, `orn`

```racket
bic x0 x1 x2         ; x0 = x1 AND NOT x2, clears the bits that are set in x2
orn x0 x1 x2         ; x0 = x1 OR NOT x2
bic x0 x1 x2 LSL @4  ; the second operand can be shifted
```

`bics` is the flag-setting form, like `ands`. Using `orn` with `xzr` gives a bitwise NOT, which is the replacement for `mvn` in the table in section 14.

### Conditional increment, invert, and negate: `cinc`, `csinv`, `csneg`, `cneg`

These are the conditional-select family, with a result that depends on the condition:

```racket
cmp   x1 x2
cinc  x3 x4 hi       ; x3 = x4 + 1 if x1 > x2 unsigned, otherwise x3 = x4
csinv x3 x4 x5 lt    ; x3 = x4 if x1 < x2 signed, otherwise x3 = NOT x5
csneg x3 x4 x5 lt    ; x3 = x4 if x1 < x2 signed, otherwise x3 = -x5
cneg  x0 x1 lt       ; x0 = -x1 if x1 < x2 signed, otherwise x0 = x1
```

### Long multiplies: `smull` and `umull`

These take two 32-bit inputs and produce a 64-bit result, so the product cannot overflow:

```racket
smull x0 w1 w2       ; x0 = signed w1 * signed w2
umull x0 w1 w2       ; x0 = unsigned w1 * unsigned w2
```

The inputs are `w` registers and the output is an `x` register.

### Extract and rotate: `extr`

`extr` takes 64 bits starting at a chosen bit of the 128-bit value `Xn:Xm`, where `Xn` is the high half:

```racket
extr x0 x1 x2 @8     ; x0 = 64 bits starting at bit 8 of x1:x2
extr x0 x1 x1 @8     ; same register twice: rotate x1 right by 8
ror x0 x1 @8         ; the same rotate, written with the ror name
```

`ror x0 x1 @8` is an alias for `extr x0 x1 x1 @8`, so both encode to `0x93C12020`. The `w` form takes 0 to 31 and the `x` form takes 0 to 63. A larger amount is rejected with a range error.

### Insert a bitfield: `bfi`

```racket
bfi x0 x1 @4 @8      ; copy the low 8 bits of x1 into bits 11 to 4 of x0
```

The other bits of `x0` are unchanged. The second and third numbers are the lowest bit and the width, the same order as `ubfx`.

### Unscaled halfword loads: `ldurh`

```racket
ldurh w0 [x1 @-2]    ; load a halfword from x1 - 2
```

Like `ldur`, the offset is a plain byte offset and may be negative.

### Prefetch: `prfm`

```racket
prfm @0 [x1 @8]      ; hint: bring the cache line at x1 + 8 closer to the core
```

`prfm` changes timing only, never results. The first operand is the 5-bit hint field, and `@0` is the value the ARM manual names `PLDL1KEEP`. The offset must be a multiple of 8, so `prfm @0 [x1 @3]` is rejected.

## 14. Forms Pisemble does not accept

Check [language.md](../../agent-context/language.md) for the full list. These came up while writing this tutorial:

| You might write | Accepted form | Why |
| --------------- | ------------- | --- |
| `mov x0 @-1` | `movn x0 @0` | Negative immediates for `mov` are not supported |
| `isb` | `isb @15` | Barriers need an option |
| `ldp x2 x3 [x1]` | `ldp x2 x3 [x1 @0]` | Pair forms need an explicit offset |
| `mvn x0 x1` | `orn x0 xzr x1` | `mvn` is not supported. `orn` with `xzr` is the bitwise NOT |
| `tbz x0 @3 done+` | `and x1 x0 @8` then `cbz x1 done+` | `tbz` is not supported. Mask the bit, then branch on zero |
| `rev x0 x1` | `rev16 x0 x1` | Only the 16-bit byte swap is supported |
| `ldxr x0 [x1 @8]` | `ldxr x0 [x1]` | Exclusive forms take no offset |
| `ldrsw w0 [x1 @8]` | `ldrsw x0 [x1 @8]` | `ldrsw` needs an `x` destination |

## Exercises

1. Write `madd` and `mul`/`add` versions of the array address calculation in section 4. Count the instructions and compare them.
2. Use `csel` to write a signed minimum of three registers without any branch.
3. Write `cset` for "x1 is between 10 and 20 inclusive". Use two compares and think about which conditions you need.
4. Write a short fragment that counts the trailing zeros of `x1` using `rbit` and `clz`. Test it with 0, 1, and `$8000000000000000` under QEMU.
5. Use `ubfx` to pull out the red, green, and blue fields of a 0x00RRGGBB colour. Then use `lsl` and `orr` to rebuild the colour from its parts. Check your answer with `ubfx` on each channel.
6. Change `bounce-axis` so that it returns a triangle wave with a 2 pixel margin at each end. Which instruction in the bouncing demo would you change, and why?
7. Build a 64-bit constant with `movz` and three `movk` instructions. Check each step with the register dump in your debugger.
