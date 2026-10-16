---
title: Using the VideoCore GPU on the Raspberry Pi 3 with Pisemble
description: Learn how the ARM core talks to the VideoCore through the mailbox property interface, how a property message is laid out, and how to read a clock rate, the DMA channel mask, and set up a frame buffer from Pisemble code.
ms.date: 2026-10-17
ms.topic: tutorial
---

## What you will learn

* What the VideoCore is and which services the firmware gives it.
* How the ARM core reaches the VideoCore through the mailbox registers.
* How a property message is laid out: its header, its tags, and its end tag.
* How to send a message with `send-vcore-msg` and read the reply.
* How the cache affects messages, and why message buffers must be aligned.
* Worked examples: the ARM clock rate, the DMA channel mask, and the frame buffer.

You will write the code yourself. This tutorial gives you the concepts, the message layout, and short fragments. It does not give you a complete program.

## Before you start

Read these project notes first:

* [language.md](../../agent-context/language.md) for the Pisemble syntax used in the snippets.
* [qemu-and-debugging.md](../../agent-context/qemu-and-debugging.md) for how the project runs under QEMU and how to read the UART output.
* [dma.md](./dma.md) for the DMA tutorial, which also uses the mailbox to find free channels.

The snippets use `send-vcore-msg` and `debug-str-reg` from [platform.rkt](../../demo/platform.rkt), so a demo that follows them starts with `(require "platform.rkt")`, as the other demos do.

> [!NOTE]
> The clock, board, and DMA results in this tutorial come from a QEMU run of the fragments below. Real hardware, and other firmware versions, may report different values.

## What the VideoCore is

The Raspberry Pi 3 has two processors that share RAM. The ARM cores run your code. The VideoCore GPU runs the firmware that starts the board. The firmware also owns the clocks, the power domains, the DMA channels that the ARM core may use, the board identity, and the frame buffer that the display shows.

In this project you do not run code on the VideoCore. You ask it for information and services by sending it messages. The ARM core writes a message into RAM and hands its address to the mailbox. The VideoCore reads the message and writes its reply into the same memory. This is the mailbox property interface, and it is the only way the project talks to the GPU.

## The mailbox registers

The mailbox is a small block of registers. The ARM core sees it at `$3F00B880`, which is the BCM2837 peripheral base plus `$00B880`. In the project this is written as `(+ PERIPH-BASE $00B880)`.

| Offset | Register | Use                                                                                                    |
| ------ | -------- | ------------------------------------------------------------------------------------------------------ |
| `$00`  | READ     | Read a reply from the VideoCore. The low 4 bits are the channel.                                       |
| `$10`  | PEEK     | Read the next reply without removing it from the mailbox.                                              |
| `$14`  | SENDER   | The sender of the next reply.                                                                          |
| `$18`  | STATUS   | Bit 31 is FULL, which means the write side cannot take a message. Bit 30 is EMPTY, which means no reply is waiting. |
| `$1C`  | CONFIG   | Interrupt enables. This project does not use them.                                                     |
| `$20`  | WRITE    | Send a message. The low 4 bits are the channel.                                                        |

A mailbox has eight channels. Channel 8 is the property channel, and it carries the messages in this tutorial.

## The property message

A property message is a block of 32-bit words in RAM. It has a header, one or more tags, and an end tag. The VideoCore reads the request and overwrites the same words with its reply, so the buffer serves as both the request and the answer.

| Offset        | Field                     | Request value                                              | Reply value                                                      |
| ------------- | ------------------------- | ---------------------------------------------------------- | ---------------------------------------------------------------- |
| `$00`         | Total size                | Size of the whole buffer in bytes, including the end tag   | Same value                                                       |
| `$04`         | Request or response code  | `0`                                                        | `$80000000` on success, `$80000001` on error                     |
| `$08`         | Tag identifier            | The tag, for example `$30002`                              | Same value                                                       |
| `$0C`         | Value buffer size         | Bytes reserved for the value                               | Same value                                                       |
| `$10`         | Tag request code          | `0`                                                        | Bit 31 set, and bits 30 to 0 hold the length of the reply value  |
| `$14`         | Value                     | Input words, if the tag takes any                          | Output words                                                     |
| Last word     | End tag                   | `0`                                                        | `0`                                                              |

Each extra tag repeats the block from `$08` onward, and the next tag starts after the value buffer. The end tag is a single zero word, and the VideoCore stops reading there, so you must include it.

> [!IMPORTANT]
> Give every tag a value buffer that is large enough for its reply. The buffer size and the total size must both match the layout you wrote, because the VideoCore uses them to find the end of the message.

## Tags you can use

The tags below are the ones this project uses or that you are likely to need first. The IDs come from the official Raspberry Pi mailbox property interface.

| Tag      | Name                 | Request value                    | Reply value                                    |
| -------- | -------------------- | -------------------------------- | ---------------------------------------------- |
| `$10002` | Board revision       | None                             | Board revision code                            |
| `$30002` | Get clock rate       | Clock id                         | Clock id and rate in Hz                        |
| `$40001` | Allocate frame buffer | Alignment in bytes              | Bus address and size in bytes                  |
| `$40003` | Get physical size    | None                             | Width and height                               |
| `$48003` | Set physical size    | Width and height                 | Width and height                               |
| `$48004` | Set virtual size     | Width and height                 | Width and height                               |
| `$48005` | Set depth            | Bits per pixel                   | Bits per pixel                                 |
| `$48006` | Set pixel order      | `0` for BGR, `1` for RGB         | Pixel order                                    |
| `$48009` | Set virtual offset   | X and Y offset                   | X and Y offset                                 |
| `$60001` | Get DMA channels     | None                             | Bit mask of the DMA channels the ARM may use   |

Common clock ids are 1 for EMMC, 2 for UART, 3 for ARM, 4 for the core, and 5 for V3D.

## Keeping the cache honest

The ARM core caches RAM. A message buffer may hold words that the ARM core has changed but not yet written to RAM, and the VideoCore would then read stale data. Before a message is sent, `send-vcore-msg` cleans each 64-byte cache line that the message covers. It uses `dc civac`, which writes the line back to RAM and removes it from the cache. It then runs `dsb sy` so the writes finish before the mailbox write.

Because `civac` also removes the lines from the cache, the reply the VideoCore writes goes to RAM, and the ARM core reads it from RAM rather than from an old cached copy. Read the reply after `send-vcore-msg` returns. If you change the buffer again, the next send cleans it again.

> [!WARNING]
> `send-vcore-msg` cleans lines in 64-byte steps, starting at the address of the message. If a message crosses a 64-byte boundary, the last line can be missed. Put every message on a 64-byte boundary with `/= 64`, as the fragments in this tutorial do.

## Sending a message

`send-vcore-msg` expects the message address in `x8`. It does the following:

1. Cleans the message from the cache, then runs `dsb sy`.
2. Waits until the STATUS FULL bit is clear.
3. Writes the message address, with channel 8 in the low 4 bits.
4. Waits until the STATUS EMPTY bit is clear.
5. Reads the mailbox, and repeats until the reply is on channel 8.

The routine saves the registers it uses, so your caller keeps `x0` to `x3` and `x8`. It does not check the response code at offset `$04`, and it does not check the response bit of each tag. Your code has to check those, as the exercises show.

## Example: read the ARM clock rate

The smallest useful message asks for one clock rate. It uses tag `$30002` with clock id 3, which is ARM.

```racket
/= 64
:clock-msg
  (write-value-32 (* 8 4))      ; total size: header, one tag, end tag
  (write-value-32 0)            ; request
  (write-value-32 $30002)       ; get clock rate
  (write-value-32 8)            ; value buffer: clock id in, rate out
  (write-value-32 0)            ; tag request
  (write-value-32 3)            ; clock id 3 is ARM
  (write-value-32 0)            ; rate in Hz, filled in by the VideoCore
  (write-value-32 0)            ; end tag

(subr Read-arm-clock () [x8] {
  adr x8 clock-msg:
  bl send-vcore-msg:
  ldr w0 [x8 @24]               ; rate in Hz
})

(subr Demo-init () [] {
  (debug-str "videocore probe" #t)
  bl Read-arm-clock:
  (debug-str-reg "arm clock hz " x0)
})
```

The total size is 32 bytes: a 4-byte header, a 4-byte request word, the 12-byte tag header, the 8-byte value buffer, and the 4-byte end tag. The reply echoes the clock id at offset `$14` and writes the rate at offset `$18`. In a QEMU run, the rate was 700000000 Hz.

## Example: read the DMA channel mask

Tag `$60001` takes no input, and its reply is a single mask word. Bit `n` is set when DMA channel `n` is available to the ARM core. The value buffer is 8 bytes, which is more than the reply needs, and the second word is unused.

```racket
/= 64
:dma-msg
  (write-value-32 (* 8 4))      ; total size
  (write-value-32 0)            ; request
  (write-value-32 $60001)       ; get DMA channels
  (write-value-32 8)            ; value buffer
  (write-value-32 0)            ; tag request
  (write-value-32 0)            ; mask, filled in by the VideoCore
  (write-value-32 0)            ; unused
  (write-value-32 0)            ; end tag

(subr Read-dma-mask () [x8] {
  adr x8 dma-msg:
  bl send-vcore-msg:
  ldr w0 [x8 @20]               ; the DMA channel mask
})
```

In the QEMU run the mask was `$3C`, which sets bits 2 to 5. Use it with the advice in [dma.md](./dma.md) when you choose a channel.

## Example: set up a frame buffer

The frame buffer needs several tags, and they all go in one message. The `mbox-msg` in [platform.rkt](../../demo/platform.rkt) is the complete version. It asks for these tags in order:

1. Set the physical size (`$48003`) to the screen width and height.
2. Set the virtual size (`$48004`) to the width and twice the height, which gives two pages.
3. Set the virtual offset (`$48009`) to 0, 0.
4. Set the depth (`$48005`) to the bits per pixel.
5. Set the pixel order (`$48006`) to 1, which is RGB.
6. Allocate the frame buffer (`$40001`), with an alignment of 4096 bytes.

Each tag has a reply, and the VideoCore may change the values you asked for, so read the replies to find the size and address you were actually given. The allocate reply holds a bus address in its first word and the size in bytes in its second word. Convert the bus address to an ARM address by masking it with `$3FFFFFFF`, as the platform does with `bus-mask`.

To show the other page, the project sends a second message, `flip-msg`, with tag `$48009`. It changes the virtual offset to the page that was off screen, so you draw into the page that is now hidden.

## QEMU and hardware

These results came from the fragments in this tutorial under QEMU:

| Tag                      | Reply under QEMU                                             |
| ------------------------ | ------------------------------------------------------------ |
| `$30002`, clock id 3     | 700000000 Hz                                                 |
| `$10002`                 | `$00A02082`                                                  |
| `$60001`                 | `$0000003C`                                                  |
| `$10004`, board serial   | No value was written, so the buffer kept the words from an earlier reply |

The board serial tag is the one to watch. The fragment sent it, but the reply words did not change, so the values were left over from the clock request. The Pitfalls section explains how to avoid that.

The project notes say that QEMU does not emulate some VideoCore services, including GPU memory allocation (`$3000C`), QPU programs, and V3D. Check [qemu-and-debugging.md](../../agent-context/qemu-and-debugging.md) before you rely on a service under QEMU, and test it on hardware when you can.

## Pitfalls

* A message that crosses a 64-byte boundary can have its last cache line missed by `send-vcore-msg`. Start every message on a 64-byte boundary with `/= 64`.
* The total size must include the end tag, and it must match the words you wrote. A size that is wrong is a bug even when the VideoCore accepts the message.
* The VideoCore only writes the replies it can answer. Clear the reply words before each send, or check the response bit in each tag at offset `$10`. A reply that is not written looks like the last one.
* `send-vcore-msg` does not check the response code. Read offset `$04` and look for `$80000000`.
* The frame buffer reply holds a bus address. Mask it with `$3FFFFFFF` before ARM code uses it.
* Pixel order `0` is BGR and `1` is RGB. Check the colours on screen after you change it.

## Exercises

1. Read the core clock with clock id 4, and print it with `debug-str-reg`.
2. Change `Read-arm-clock` so that it checks the response code at offset `$04` and the tag response bit at offset `$10`. Return 0 if either check fails.
3. Read the EMMC clock (id 1) and the UART clock (id 2) in one message with two clock tags. Work out the new total size from the layout table.
4. Build a message that reads the board revision with tag `$10002`, and compare the reply with the QEMU table.
5. Change the pixel order in `mbox-msg` to BGR, and check on QEMU or hardware what changes on screen.
