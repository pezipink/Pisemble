---
title: Using interrupts on the Raspberry Pi 3 with Pisemble
description: Learn how AArch64 interrupts reach EL2, how to build a vector table, route and acknowledge the BCM system timer, and write an IRQ handler that returns with eret. The example runs under QEMU.
ms.date: 2026-10-17
ms.topic: tutorial
---

## What you will learn

* What an interrupt does to the CPU and why a handler must return with `eret`.
* How the BCM2837 routes the system timer to the core IRQ line.
* How to build the 16-entry exception vector table and point VBAR_EL2 at it.
* Why HCR_EL2.IMO must be set for IRQs to reach EL2 under QEMU.
* How to unmask IRQs, sleep with `wfi`, and count interrupts in a handler.
* How to check a hung or silent interrupt with the QEMU monitor.

The tutorial uses one complete program, [timer-irq.rkt](../../examples/timer-irq.rkt). Each section explains one part of it. Build it, run it, then change it.

## Before you start

Read these project notes first:

* [language.md](../../agent-context/language.md) for the Pisemble syntax, including the raw opcode rules and the `PUSH`/`POP` macros.
* [qemu-and-debugging.md](../../agent-context/qemu-and-debugging.md) for running QEMU and the monitor commands used in the last section.
* [mmu.md](mmu.md) for the EL2 system register conventions. This tutorial uses the same `el-op` pattern.
* [periph.rkt](../../lib/periph.rkt) for the interrupt controller and system timer addresses.
* [stack.rkt](../../lib/stack.rkt) for `PUSH` and `POP`.

The example runs at EL2. QEMU `-kernel` enters the image at EL2, so the vector table is VBAR_EL2 and the routing bit is in HCR_EL2. If your code runs at EL1, use `vbar_el1` and `hcr_el2` is not involved.

## Interrupts in one paragraph

An interrupt is a signal from a device that makes the CPU stop what it is doing. The CPU saves the program counter in ELR and the processor state in SPSR, then jumps to a vector in the table pointed to by VBAR. The handler does its work and returns with `eret`, which restores the saved PC and state. The interrupted code continues as if nothing happened, so the handler must save every register it changes and restore them before it returns.

On the Raspberry Pi 3 the path for the system timer is:

1. The system timer compares its free-running counter with a compare register. When they match, it sets match bit 1 in `TIMER_CS`.
2. The BCM interrupt controller sees the timer as IRQ 1 in `IRQ_PENDING_1`, if IRQ 1 is enabled in `ENABLE_IRQS_1`.
3. The controller raises a GPU interrupt. The core sees it as bit 8 in `CORE0_IRQ_SOURCE` (`$40000060`).
4. The core takes an IRQ exception if PSTATE allows it and the vector table sends it to a handler.

The handler must clear the match bit in `TIMER_CS` (write 1 to bit 1). If it does not, the line stays high and the handler runs again as soon as it returns.

## Step 1: Build the vector table

VBAR_EL2 holds the address of a table with 16 entries of 128 bytes (`$80`) each. The table must be aligned to at least 2KB (`$800`). Each entry is the branch the CPU takes for one kind of exception. The entries are grouped by the exception level and stack pointer of the code that was interrupted:

| Index | Offset | Source | Type |
| ----- | ------ | ------ | ---- |
| 0 | `$000` | current EL, SP0 | synchronous |
| 1 | `$080` | current EL, SP0 | IRQ |
| 4 | `$200` | current EL, SPx | synchronous |
| 5 | `$280` | current EL, SPx | IRQ |
| 8 to 15 | `$400` to `$780` | lower EL | all types |

At EL2 the core runs on SP_EL2 (SPSel is 1), so an IRQ taken while running EL2 code uses index 5. The example also points index 1 at the handler, in case the code clears SPSel.

The table in the example is written out one entry per line:

```text
/= $800
:vectors
/= $80
b bad:      ; 0 current EL SP0, sync
/= $80
b irq:      ; 1 current EL SP0, IRQ
...
/= $80
b irq:      ; 5 current EL SPx, IRQ
...
```

`/= $80` aligns the next line to a 128-byte boundary. Keep the `/=` and the branch on separate lines, because the directive does not parse with a second instruction on the same line. Every entry except 1 and 5 goes to `bad`, which prints a message and halts. An unexpected exception then shows up as text on the UART, not as a silent hang.

The table is installed at the start of `:main`:

```text
adr x0 vectors:
(write-value-32 (el-op $d518c000))  ; msr vbar_el2, x0
```

`$d518c000` is `msr vbar_el1, x0`. `el-op` adds `4 << 16` to change the op1 field from EL1 to EL2, which gives `msr vbar_el2, x0`. This is the same pattern as in [platform.rkt](../../lib/platform.rkt).

## Step 2: Route IRQs to EL2

Under QEMU, an IRQ taken at EL2 is masked until HCR_EL2.IMO (bit 4) is set. With IMO clear, QEMU treats the IRQ as an EL1 interrupt and does not deliver it while the core runs at EL2. The example saw exactly this: the timer status and the core source register both showed the interrupt pending, but the handler never ran and the PC stayed in the main loop.

Set the bit without disturbing the rest of the register, because RW and the other control bits must stay as they are:

```text
mrs x0 hcr_el2
orr x0 x0 @16
msr hcr_el2 x0
```

`@16` is bit 4, which is a valid logical immediate. Do this before you unmask IRQs.

## Step 3: Configure the system timer

The system timer runs at 1 MHz. Compare register 1 (`TIMER_C1`) is at `TIMER_CS + $10`. Match 1 fires when the low 32 bits of the counter (`TIMER_CLO`) equal C1.

```text
ldr x1 TIMER_CS:
ldr w0 [x1 @4]          ; CLO
ldr x2 interval:        ; 250000 = 250 ms
add w0 w0 w2
str w0 [x1 @$10]        ; C1 = CLO + interval
mov w0 @2
str w0 [x1]             ; clear any stale match 1
```

Then enable IRQ 1 in the interrupt controller. `ENABLE_IRQS_1` bit 1 is the system timer match 1:

```text
ldr x1 ENABLE_IRQS_1:
mov w0 @2
str w0 [x1]
```

Writes to `ENABLE_IRQS_1` only set enable bits. To turn one off, write to `DISABLE_IRQS_1` at `$B21C`.

## Step 4: Unmask IRQs

PSTATE has four mask bits: D (debug), A (SError), I (IRQ) and F (FIQ). An IRQ is taken only when I is clear. `msr daifclr` clears bits in DAIF. The immediate selects the bits: `@2` clears I.

```text
msr daifclr @2
```

Unmask last. Once I is clear, an interrupt pending from an earlier step can fire immediately.

## Step 5: Write the handler

The handler runs with interrupts masked. It must do the following, in order:

1. Save every register it uses.
2. Clear the source. Write 1 to bit 1 of `TIMER_CS`.
3. Rearm the compare register for the next tick.
4. Do the work. The example increments a 64-bit counter.
5. Restore the saved registers.
6. Return with `eret`.

```text
:irq
  (PUSH x0)
  (PUSH x1)
  (PUSH x2)
  ldr x1 TIMER_CS:
  mov w0 @2
  str w0 [x1]                 ; write 1 to clear match 1
  ldr w0 [x1 @4]
  ldr x2 interval:
  add w0 w0 w2
  str w0 [x1 @$10]            ; next match at CLO + interval
  ldr x0 ticks:
  add x0 x0 @1
  adr x1 ticks:
  str x0 [x1]
  (POP x2)
  (POP x1)
  (POP x0)
  eret
```

Notes on the handler:

* `PUSH` and `POP` take one register per call. The `(PUSH x0)` form is needed for the macro to expand, and the registers must be popped in the reverse order.
* The source is cleared before the next compare is written. Clearing it first means the line drops before the handler finishes, so the next match can set it again.
* Only use `x0` to `x2` here. Anything the handler touches must be saved, or the interrupted code will see wrong values after `eret`.

## Step 6: Sleep in the main loop

The main loop waits for an interrupt and prints when the counter changes:

```text
:wait-irq
  wfi
  ldr x1 ticks:
  adr x2 printed:
  ldr x3 [x2]
  cmp x1 x3
  b.eq wait-irq-
  str x1 [x2]
  (debug-str "tick" #t)
  b wait-irq-
```

`wfi` stops the core until an interrupt is pending. It wakes even when the interrupt is masked, so the loop is safe whether or not IRQs are unmasked. The loop compares the counter to the last value it printed, rather than to a flag that the handler clears, so it cannot miss a tick.

## Build, run and check

Build from the `examples` folder:

```powershell
cd examples
racket timer-irq.rkt
```

This writes `timer-irq.img` beside the source. Run it headless, with the UART going to a file. Keep both `-serial` options: the two-option form is the one that produced output in testing.

```powershell
& "C:\Program Files\qemu\qemu-system-aarch64.exe" -M raspi3b -kernel timer-irq.img -display none -serial null -serial file:uart.bin
```

`uart.bin` should contain `timer irq demo` followed by one `tick` line about every 250 ms. In an 8-second run the example printed 39 ticks. Stop QEMU when you have enough output. The UART file stays locked while QEMU runs, so read it with a shared open, or stop QEMU first.

If the output stops after `timer irq demo`, open the monitor. Add `-monitor tcp:127.0.0.1:45454,server,nowait` to the command line and connect with a TCP client. These checks separate the three likely failures:

| Command | Good value | Meaning if wrong |
| ------- | ---------- | ---------------- |
| `xp /1wx 0x3f003000` | bit 1 set after a match, then clear after the handler | Bit 1 stuck at 1: the handler is not clearing it, or never runs. |
| `xp /1wx 0x3f00b210` | `0x2` | IRQ 1 not enabled in the controller. |
| `xp /1wx 0x40000060` | bit 8 set while a timer match is pending | Core does not see the GPU interrupt. Check routing at `$4000000C`. |
| `info registers` | PSTATE with I (bit 7) clear, EL2h | IRQs are still masked, or the core is at a different EL. |
| `x /2i <vectors + $280>` | `b` to the IRQ handler | The vector table is not where VBAR points. |

`xp` reads physical memory. `info registers` shows the PC, so `x /4i $pc` tells you whether the core is in the main loop or inside the handler.

If `bad` fires, the UART prints `unexpected exception`. The `bad` label does not record which vector was taken, so use `info registers` to read ELR_EL2 and ESR_EL2 and find the faulting instruction.

## Changing the example

Try these changes one at a time:

* Change `interval` to `1000000` for one tick per second.
* Count ticks in a second variable and print the count every ten ticks.
* Set `daifset @2` in the main loop for a second, and check that the count does not change while IRQs are masked.

## Limits under QEMU

The example tick rate was about 4 per second in a 10-second wall-clock run. QEMU does not run in real time, so the rate depends on the host. Measure timing with the system timer counter, not with instruction counts.

The example uses the BCM system timer, not the ARM local timer, and it runs with the MMU off. Both choices are deliberate: the system timer is the source the Pi 3 hardware documents for this path, and QEMU models it.

Nothing here has been run on hardware. The register addresses and the IRQ 1 bit come from the BCM2837 documentation, and QEMU uses the same addresses. The routing bit (HCR_EL2.IMO) is the part most likely to differ, so check it first if the handler does not run on a real board.
