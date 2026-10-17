---
title: Using DMA on the Raspberry Pi 3 with Pisemble
description: Learn how the BCM2837 DMA controller moves memory, how control blocks describe a transfer, and how to start, wait for, and check a transfer from Pisemble code.
ms.date: 2026-10-17
ms.topic: tutorial
---

## What you will learn

* What a DMA controller does and why you would use one.
* How channels, control blocks, and the ENABLE register fit together.
* What each field of a control block and the channel registers mean.
* How to start a copy, wait for it to finish, and check for errors.
* How the cache affects DMA, and what you need to clean before and after a transfer.

You will write the code yourself. This tutorial gives you the concepts, the register layout, and short fragments. It does not give you a complete program.

## Before you start

Read these project notes first:

* [language.md](../../agent-context/language.md) for the Pisemble syntax used in the snippets.
* [qemu-and-debugging.md](../../agent-context/qemu-and-debugging.md) for how the project runs under QEMU.
* [periph.rkt](../../lib/periph.rkt) for `PERIPH-BASE` and the way peripheral registers are written.

The snippets assume `PERIPH-BASE` is defined as in `lib/periph.rkt`. They also assume that the virtual and physical addresses match, so the address `adr` gives you is the one the DMA controller uses. Check the MMU setup in `emu_clean.rkt` (pisemble-wolf3d repo) before you rely on that.

## What DMA is

DMA (direct memory access) lets a controller copy data between memory locations while the CPU does something else. Without DMA, the CPU runs a load and a store for every word. With DMA, the CPU writes a short description of the work into RAM, starts the controller, and later checks whether the work is done.

The uses are the same ones you would find on any small system:

* Copying or clearing a large buffer, such as a frame buffer.
* Moving data to or from a peripheral, such as audio or a display.
* Filling memory with a pattern.

The project notes say DMA is not used anywhere yet, so this is a good place to start.

## The parts of the controller

The BCM2837 DMA controller has several parts that work together.

* A channel is a set of registers at a fixed address. Each channel runs one chain of control blocks at a time.
* A control block (CB) is a 32-byte structure in RAM that describes one transfer. It names the source, the destination, the length, and the next control block.
* A chain is a list of control blocks linked by their `NEXTCONBK` field. A value of zero ends the chain.
* The ENABLE register is global. A channel only runs when its bit in ENABLE is set.

> [!NOTE]
> The GPU and the firmware use some DMA channels. Channel 0 is often taken. Choose a channel that is free, for example by reading the DMA channel mask with the mailbox property tag mentioned in the project notes. Channel 15 has its own address and is not part of the normal channel block.

## Where the registers live

The ARM core sees the peripherals starting at `$3F000000`, so the DMA block starts at `$3F007000`. Each channel is 256 bytes (`$100`) apart. Addresses of RAM buffers inside a control block are physical addresses, the same values `adr` gives you. A control block that points at a peripheral register must use the bus view of that register, which starts at `$7E000000` instead of `$3F000000`.

| Address (ARM view)     | Name       | Purpose                                                                 |
| ---------------------- | ---------- | ----------------------------------------------------------------------- |
| `$3F007000 + n * $100` | CS         | Control and status for channel `n`, at offset `$00`                     |
| `$3F007000 + n * $100` | ADDR       | Address of the control block the channel is running, at offset `$04`   |
| `$3F007000 + n * $100` | TI         | Copy of the control block `TI` field, at offset `$08`                   |
| `$3F007FE0`            | INT_STATUS | Interrupt status for each channel                                       |
| `$3F007FF0`            | ENABLE     | One bit per channel                                                     |

The channel also has copies of the other control block fields at offsets `$0C` to `$20`. The channel loads them from the control block when it starts each block, so you normally write only the control block and leave these alone.

> [!IMPORTANT]
> `ADDR` reads zero when the whole chain is finished. The Linux DMA driver relies on this, and you can use it as your completion test.

## The control block

A control block is eight 32-bit words, which is 32 bytes. The address of every control block must be a multiple of 32.

| Offset | Field     | Meaning                                                                  |
| ------ | --------- | ------------------------------------------------------------------------ |
| `0`    | TI        | Transfer information: flags for interrupts, increments, and widths       |
| `4`    | SOURCE_AD | Address to read from                                                     |
| `8`    | DEST_AD   | Address to write to                                                      |
| `12`   | TXFR_LEN  | Number of bytes to move in normal mode                                   |
| `16`   | STRIDE    | Gaps added to the source and destination after each row in 2D mode       |
| `20`   | NEXTCONBK | Address of the next control block, or zero to end the chain              |
| `24`   | reserved  | Set to zero                                                              |
| `28`   | reserved  | Set to zero                                                              |

### The TI bits

These are the bits you will use most often.

| Bit | Name        | Meaning when set                                                                           |
| --- | ----------- | ------------------------------------------------------------------------------------------ |
| 0   | INTEN       | Raise an interrupt when this block ends                                                    |
| 1   | TDMODE      | 2D mode: `TXFR_LEN` holds the row length and row count, and `STRIDE` holds the gaps        |
| 3   | WAIT_RESP   | Wait for each write to be acknowledged before going on                                     |
| 4   | DEST_INC    | Move the destination address forward after each access                                     |
| 5   | DEST_WIDTH  | Write 128 bits at a time (QEMU does not support this)                                      |
| 6   | DEST_DREQ   | Pace destination writes with a peripheral request                                          |
| 7   | DEST_IGNORE | Discard writes                                                                             |
| 8   | SRC_INC     | Move the source address forward after each access                                          |
| 9   | SRC_WIDTH   | Read 128 bits at a time (QEMU does not support this)                                       |
| 10  | SRC_DREQ    | Pace source reads with a peripheral request                                                |
| 11  | SRC_IGNORE  | Do not read, and use zero instead                                                          |

Bits 12 to 15 hold the burst length, and bits 16 to 20 hold the peripheral request number (`PERMAP`). Both matter only for paced transfers. A memory-to-memory copy leaves them at zero.

For a memory-to-memory copy you set `DEST_INC` and `SRC_INC`, and you usually set `WAIT_RESP` and `INTEN` as well. The Linux driver does the same thing for its memcpy path.

### The CS bits

Your code reads and writes CS at offset `$00` of the channel.

| Bit | Name   | Meaning                                                                |
| --- | ------ | ---------------------------------------------------------------------- |
| 0   | ACTIVE | Write 1 to start the channel                                           |
| 1   | END    | Set when a control block finishes. Write 1 to clear it                 |
| 2   | INT    | Interrupt pending. Write 1 to clear it                                 |
| 8   | ERR    | An error happened. Read the DEBUG register at offset `$20` for details |
| 30  | ABORT  | Stop the current block (QEMU treats this as a no-op)                   |
| 31  | RESET  | Reset the channel. This bit clears itself                              |

## Setting up a copy

A transfer takes six steps. Each step is a fragment you will write yourself.

1. Build a control block in RAM, 32-byte aligned.
2. Clean the source buffer out of the cache so DMA reads the current data.
3. Point the channel's `ADDR` at the control block.
4. Set the channel's bit in ENABLE.
5. Write ACTIVE to CS to start the channel.
6. Wait for `ADDR` to read zero, then clear END and check ERR.

### Constants

These constants go at the top of your file. They assume `PERIPH-BASE` comes from `lib/periph.rkt`.

```racket
(define DMA-BASE (+ PERIPH-BASE $7000))
(define DMA-ENABLE (+ DMA-BASE $FF0))
(define (dma-chan n) (+ DMA-BASE (* n $100)))
(define DMA-CH 5)
(define TI-MEMCPY (bitwise-ior $8 $10 $100))
```

`TI-MEMCPY` sets `WAIT_RESP` (`$8`), `DEST_INC` (`$10`), and `SRC_INC` (`$100`).

### Building the control block

The control block lives in the data section. Aligning it to 32 bytes with `/=` gives the DMA controller the alignment it needs.

```racket
/= 32
:dma-cb
  (write-value-64 0)
  (write-value-64 0)
  (write-value-64 0)
  (write-value-64 0)
```

Fill in the fields at run time, once the buffer addresses are known:

```racket
  adr x0 dma-cb:
  load-immediate x1 TI-MEMCPY
  str w1 [x0]                 ; TI
  adr x1 src-buf:
  str w1 [x0 @4]              ; SOURCE_AD
  adr x1 dst-buf:
  str w1 [x0 @8]              ; DEST_AD
  mov w1 @256
  str w1 [x0 @12]             ; TXFR_LEN, 256 bytes
```

`NEXTCONBK` is already zero, so this is a one-block chain. `TXFR_LEN` must be a multiple of 4. QEMU rejects other lengths and sets ERR.

### Cleaning the cache

The DMA controller reads and writes RAM directly, so it does not see data that is still sitting in the CPU cache. Clean the source before the transfer, and clean and invalidate the destination after it. On the Cortex-A53, a cache line is 64 bytes.

```racket
  adr x0 src-buf:
  mov x1 @4                   ; 256 bytes is four lines
:clean-src
  (write-value-32 $d50b7e20)  ; dc civac, x0
  add x0 x0 @64
  sub x1 x1 @1
  cbnz x1 clean-src-
  dsb @15
```

`$d50b7e20` is the raw opcode for `dc civac, x0`, from the table in [language.md](../../agent-context/language.md).

### Starting the channel

Point the channel at the control block, set its ENABLE bit, and write ACTIVE. Put a barrier before the start so the control block writes are visible to the DMA controller.

```racket
  dsb @15
  (load-immediate x0 (dma-chan DMA-CH))
  adr x1 dma-cb:
  str w1 [x0 @4]              ; ADDR
  load-immediate x2 DMA-ENABLE
  load-immediate x1 (arithmetic-shift 1 DMA-CH)
  str w1 [x2]                 ; ENABLE
  mov w1 @1
  str w1 [x0]                 ; CS, ACTIVE
```

### Waiting for the copy

Test `ADDR` at the bottom of the loop, as the project's loops do.

```racket
  (load-immediate x0 (dma-chan DMA-CH))
:dma-wait
  ldr w1 [x0 @4]              ; zero when the chain is finished
  cbnz w1 dma-wait-
```

After the loop, clear END, check ERR, and clean and invalidate the destination before the CPU reads it:

```racket
  mov w1 @$2
  str w1 [x0]                 ; CS, write 1 to clear END
  ldr w1 [x0]                 ; CS, check ERR at bit 8
```

Then run the same `dc civac` loop as before, but on `dst-buf:`.

> [!TIP]
> Start with a copy of 256 bytes into a buffer you have filled with a known pattern. Print the first and last words with `debug-str` over the UART. If the destination is wrong, check the cache loops before you check the control block.

## Chaining blocks

`NEXTCONBK` makes the channel continue to another control block without the CPU stepping in. Each block has its own TI, addresses, and length. The channel finishes the chain when it reads a `NEXTCONBK` of zero.

Build two control blocks, put the address of the second one in the `NEXTCONBK` field of the first, and leave the second one's field at zero. Start the chain as before. Set INTEN on the first block if you want an interrupt after it, and use END to see when each block has finished.

## 2D transfers

With TDMODE set, `TXFR_LEN` splits into two parts. The low 16 bits are the row length in bytes, and the bits above that hold the number of rows minus one. `STRIDE` holds two 16-bit values: the upper half is added to the destination after each row, and the lower half is added to the source. Use 2D mode to copy a rectangle out of a larger image or into one.

## QEMU compared with hardware

The project notes and the QEMU model (`hw/dma/bcm2835_dma.c`) give these behaviours on QEMU:

* Writing ACTIVE runs the whole chain at once, so the wait loop exits immediately.
* 2D mode works.
* ABORT does nothing.
* 128-bit widths set ERR.
* A transfer length that is not a multiple of 4 sets ERR.

Real hardware takes time, and the timing differs. The wait loop is therefore the only part of this tutorial that you can trust to behave the same way on both. Test timing and cache behaviour on real hardware before you rely on either.

> [!WARNING]
> The register offsets and bit positions in this tutorial come from the QEMU DMA model and the Linux DMA driver. They have not yet been checked line by line against the BCM2837 datasheet in the repository root, `BCM2837-PI3.pdf`. Check them there before you use them in production code.

## Exercises

1. Copy 256 bytes with one control block. Fill the source with a known pattern, and check the destination after the wait.
2. Fill a buffer with one 32-bit value. Set `SRC_IGNORE` with `SRC_INC` clear, and write the value from the source word.
3. Chain two control blocks that copy two halves of a buffer. Use END to confirm that both blocks ran.
4. Write a routine that takes a source, a destination, and a length in `x0`, `x1`, and `x2`, and returns when the copy is complete.
5. Use 2D mode to copy a 16 by 16 rectangle out of a 320 pixel wide image into a frame buffer with a different stride.
