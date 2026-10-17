---
title: Running Code on Multiple Cores on the Raspberry Pi 3 with Pisemble
description: Learn how the four Cortex-A53 cores start, how to identify and park them, how to give each core its own stack, how to wake a parked core through the local mailbox, and how to share memory between cores safely.
ms.date: 2026-10-17
ms.topic: tutorial
---

## What you will learn

* How the Raspberry Pi 3 has four cores, and how each core knows which one it is.
* How to park the cores you do not want running yet.
* How to turn on SMP coherency, which the cache needs before cores can share data.
* How to give each core its own stack.
* How to wake a parked core with the local mailbox and send it an entry address.
* How to share data between cores, and which barriers stop them seeing stale values.

You will write the code yourself. This tutorial gives you the concepts, the register addresses, and short fragments. It does not give you a complete program.

## Before you start

Read these project notes first:

* [language.md](../../agent-context/language.md) for the Pisemble syntax used in the snippets.
* [qemu-and-debugging.md](../../agent-context/qemu-and-debugging.md) for how QEMU starts the image, and the note that all four cores begin at `$80000`.
* [linker.md](../../agent-context/linker.md) for alignment and global labels.

The core park code in `emu_clean.rkt` (pisemble-wolf3d repo, around lines 238 to 256) and [platform.rkt](../../demo/platform.rkt) (around lines 323 to 329) is a working reference. Read it after you have worked through this tutorial, and compare your version with it.

## The cores

The BCM2837 has four Cortex-A53 cores. They share RAM and the caches, but each core has its own registers, its own program counter, and its own stack. When you power on, the boot code runs on one core, and the other three wait. Your program has to decide what the other cores do.

Two things make this hard:

* All four cores can run the same code at the same time. Without a rule, they will fight over the same stack and the same data.
* A core that is not doing anything should sleep, not spin. Sleeping saves power, and it is easier to reason about.

## Finding out which core you are

The `mpidr_el1` register (Multiprocessor Affinity Register) tells a core its identity. On the Pi 3, bits 1 and 0 hold the core number, from 0 to 3. Every core runs the same code from the start, so the first thing your code does is read this register.

```racket
  mrs x0 mpidr_el1
  mov x1 @$3
  and x0 x0 x1                ; x0 = core number
```

The mask `$3` keeps only the low two bits. The `and` form with registers is the one `emu_clean.rkt` uses.

## Parking the other cores

Parking a core means keeping it in a safe loop until you want it to do work. The simplest park loop uses `wfe` (wait for event), which sleeps until something sends an event with `sev` or until an interrupt arrives.

```racket
  cbz x0 main:                ; core 0 carries on
:hang
  wfe
  b hang-
```

`main:` refers to your main routine, which is a unique global label in this example. The `hang` loop is the parking loop. The `cbz` is the decision: if the core number is zero, branch away to `main`. Every other core falls through to `hang`.

> [!NOTE]
> The notes say QEMU starts all four cores at `$80000` at EL2. Because of this, every core runs your park check first. Do not put any code before the check that touches shared memory or the UART.

## Turning on SMP coherency

The Cortex-A53 has a control bit, SMPEN, in `cpuectlr_el1`. It joins the core to the cluster so that caches are kept coherent between cores. Without SMPEN, a value one core writes to cacheable memory may not be seen by another core.

Set bit 6 with a read, an `orr`, and a write. The project writes this as raw opcodes, because `mrs` and `msr` do not know `cpuectlr_el1`.

```racket
  (write-value-32 $d539f220)  ; mrs x0, cpuectlr_el1
  (write-value-32 $b27a0000)  ; orr x0, x0, #(1<<6)
  (write-value-32 $d519f220)  ; msr cpuectlr_el1, x0
```

`CPUECTLR_EL1` is a per-core register. Each core sets its own copy. Put this sequence in the entry path of every core, before the caches are enabled. The Pi armstub normally sets this bit already, so repeating it is harmless.

## Giving each core its own stack

Each core needs its own stack. Otherwise two cores that call functions will overwrite each other's return addresses. Pick a region of RAM that your image does not use, and give each core a slice of it. A simple layout is one top address for core 0, and each further core starts 64 KB lower.

These constants go at the top of your file:

```racket
(define STACK-TOP $400000)
(define STACK-SIZE $10000)
```

The stack setup takes the core number from the earlier step, subtracts `STACK-SIZE` once for each core, and then sets `sp`. Use a loop rather than a shift, since this tutorial has not confirmed that Pisemble has `lsl` for registers.

```racket
  mrs x0 mpidr_el1
  mov x1 @$3
  and x0 x0 x1                ; x0 = core number
  load-immediate x1 STACK-TOP
:stack-loop
  cbz x0 stack-done+
  load-immediate x2 STACK-SIZE
  sub x1 x1 x2
  sub x0 x0 @1
  b stack-loop-
:stack-done
  mov sp x1
```

Run this on every core before you call any function. Core 0 keeps `STACK-TOP` and each other core gets a lower address.

> [!IMPORTANT]
> `mov sp x1` is rewritten to `add` by Pisemble. Use it as written. Do not try to write `sp` with a `mov` from an immediate.

## The local mailbox

Cores 1 to 3 can be woken by a message from core 0 through the local mailbox. The local peripherals start at `$40000000`, and each core has four mailboxes. Mailbox 3 is the one most bare-metal code uses for wake-up, because the firmware and QEMU both use it for the same job.

Each core has two register addresses for each mailbox:

* The write-set register ORs the value you write into the mailbox. Writing zero changes nothing.
* The read-clear register returns the mailbox value and then clears it to zero.

| Core | Mailbox 3 write-set | Mailbox 3 read-clear |
| ---- | ------------------- | -------------------- |
| 0    | `$4000008C`         | `$400000CC`          |
| 1    | `$4000009C`         | `$400000DC`          |
| 2    | `$400000AC`         | `$400000EC`          |
| 3    | `$400000BC`         | `$400000FC`          |

The pattern is: write-set at `$40000080 + 16 * core + 12`, and read-clear at `$400000C0 + 16 * core + 12`. Define two Racket helpers so you do not type the numbers by hand:

```racket
(define LOCAL-BASE $40000000)
(define (mbox3-set core) (+ LOCAL-BASE $8C (* 16 core)))
(define (mbox3-clr core) (+ LOCAL-BASE $CC (* 16 core)))
```

> [!TIP]
> Check these addresses against the BCM2837 datasheet before you rely on them. They come from the QEMU local-peripheral model and the Linux driver. The tutorial's wake code assumes they are correct.

## The wake protocol

Core 0 sends an entry address to a parked core in three steps. The parked core does the reverse.

Core 0 does this:

1. Read-clear the target's mailbox, so any old value is removed. The write-set operation can only OR bits in, so a stale value would corrupt the new one.
2. Write the entry address to the target's write-set register.
3. Run a barrier so the earlier writes are visible to the other core, then send an event with `sev`.

```racket
  (load-immediate x1 (mbox3-clr 1))
  ldr w2 [x1]                 ; read-clear: remove any old value
  adr x2 core1-entry:
  (load-immediate x1 (mbox3-set 1))
  str w2 [x1]                 ; write-set: post the entry address
  dsb @15
  sev
```

The parked core does this:

```racket
:park-loop
  wfe
  (load-immediate x1 (mbox3-clr 1))
  ldr w0 [x1]                 ; read-clear: get the entry address, or zero
  cbz w0 park-loop-
  br x0
```

`wfe` can return for no reason, which is why the loop checks the mailbox and goes back to sleep when the value is zero. `br x0` jumps to the entry address. Because the address is 32 bits, it fits in `w0`, which the `ldr` zero extends.

Core 1's entry code should then set SMPEN, set up its stack, and call its own work routine. Use the core-number check and stack setup from the earlier steps.

> [!NOTE]
> The snippets above are for core 1. Make a copy of the park loop for each core, or drive them from a single loop that uses `mpidr_el1` to choose the mailbox. Writing the address-for-core helpers with `mbox3-set` and `mbox3-clr` makes this a one-line change.

## Sharing data between cores

Two cores that touch the same memory need an order. One core must write the data and then announce it, and the other must check the announcement before it reads the data. Barriers provide that order.

* `dmb` (data memory barrier) makes earlier memory accesses visible to other cores before later ones. Use it between a data write and a flag write.
* `dsb` (data synchronization barrier) waits until earlier accesses have completed. Use it before `sev` and before the start of a DMA transfer.

```racket
  str w1 [x2]                 ; write the data
  dmb @15
  str w3 [x4]                 ; set the ready flag
```

The reader checks the flag, then reads the data:

```racket
:wait-ready
  ldr w3 [x4]
  cbz w3 wait-ready-
  dmb @15
  ldr w1 [x2]
```

This is a single-writer, single-reader handoff. It is simple and it works for one piece of data passed one way. It does not protect against two cores writing the same location at the same time.

> [!WARNING]
> Pisemble accepts the exclusive load and store instructions (`ldxr` and `stxr`), and their forms assemble. `language.md` does not list them, and I have not run them on QEMU or hardware. Do not build a lock on them until you have tested them on one core and then on several. Until then, design your shared data so that one core writes each location, or so that the handoff above is enough.

## QEMU compared with hardware

QEMU's `raspi3b` machine has its own way of starting secondary cores. When it loads a 64-bit kernel, it runs a spin-table stub. The secondaries wait in that stub, reading slots at `0xd8`, `0xe0`, `0xe8`, and `0xf0`, until the slot for their core is non-zero. Then they branch to the address in the slot.

This does not match the note that all four cores start at `$80000`. The two descriptions can both be true only if the stub is not used with the `-kernel` path in this project. Check which one happens by printing the core number and the value of `mpidr_el1` from each core, before you rely on either.

On real hardware, the Pi firmware and armstub control the first steps and the spin tables. A kernel loaded as `kernel8.img` should not assume a particular state for the secondaries until you have checked it.

> [!WARNING]
> The park loop, the mailbox wake, and the spin-table behaviour in this tutorial are not yet confirmed in this project under QEMU. The addresses come from QEMU's local peripheral model and the Linux driver. Test each step on one secondary core before you move on to all three.

## Exercises

1. Print the core number from every core over the UART. Make each core print its number once, then park.
2. Park cores 1 to 3 with the park loop. Confirm with the UART that core 0 can still print while the others sleep.
3. Give each core its own stack, then call a routine that writes the core number to a different word on each stack. Read the words back on core 0.
4. Wake core 1 with the mailbox. Have core 1 write a value to a shared word, then set a ready flag. Core 0 waits for the flag and prints the value.
5. Write the same handoff for core 2 and core 3, using the helpers in this tutorial and a loop over the core number.
