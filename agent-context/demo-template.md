---
title: Demo template in demo/
description: How demo/platform.rkt boots QEMU, runs the frame loop and links a demo, the contract a demo must follow, and the helpers it gets for free
ms.date: 2026-10-17
---

## Files

* `demo/platform.rkt` holds everything except the effect: boot, UART, framebuffer, MMU, frame loop, fault handler and the shared graphics helpers.
* `demo/plasma.rkt` is a 32 bit example (per-pixel plasma).
* `demo/cycle.rkt` is an 8 bit example. It draws a pattern once, then animates by rotating the palette every frame.
* `demo/bouncing.rkt` is a 32 bit animation with three bouncing balls. Each ball is a filled circle drawn as horizontal spans, and its position is computed from the frame counter.
* `demo/stars3d.rkt` is a minimal 32 bit 3D starfield. Each star's depth comes from the frame counter, and its screen offset is a perspective divide (`offset * 64 / z`).
Build and run from the `demo` folder:

```powershell
cd c:\repos\Pisemble\demo
& "C:\Program Files\Racket\racket.exe" plasma.rkt     # writes plasma.obj and plasma.img beside plasma.rkt
```

Then run `plasma.img` as described in [qemu-and-debugging.md](qemu-and-debugging.md). Expect `platform up` on the UART.

> [!CAUTION]
> The build writes `<name>.obj` and `<name>.img` beside the demo source, where `<name>` is the source file's name. Running Racket from another folder gives the same output. Two demos with different names do not overwrite each other. To preserve existing artifacts, rename the demo or copy it to a scratch folder before building it. The `require "platform.rkt"` path resolves relative to the source file.

## Writing a new demo

```racket
#lang pisemble
(require "platform.rkt")

(demo {
  :Demo-init
    ret x30

  :Draw-frame          ; x0 = back page address, x1 = frame number
    (PUSH x30)
    mov x20 x0
    mov w1 @0
    bl clear-screen:
    mov x0 x20
    mov x1 @10
    mov x2 @10
    mov x3 @50
    mov x4 @50
    (load-immediate x5 (rgb 255 0 0))
    bl fill-rect:
    (POP x30)
    ret x30
})
```

Pass `#:bpp 8` to `demo` for 8 bit palette mode: `(demo #:bpp 8 { ... })`.

The contract:

* Define `:Demo-init` (called once, after the framebuffer and MMU are up) and `:Draw-frame` (called every frame with `x0` = the page to draw and `x1` = the frame counter).
* Both may change `x0` to `x28`. Save `x30` if you call anything.
* The FP registers `s0` to `s31` are free to use. `subr` and `PUSH` save only integer registers, so if a routine uses FP values across a `bl`, save them yourself.
* `subr` restores only the registers in its clobber list, plus `x30`. Any argument register the body writes must be in the clobber list, or the caller sees a changed value. Inside the body, copy an argument to a free register before anything else writes its register, because a later read sees the new value. The `draw-ball` and `draw-circle` routines in `bouncing.rkt` follow this rule.
* Draw only into the page passed in `x0`. The platform flips to it after `Draw-frame` returns.
* Use lower-case names for every other label. Upper-case names are global, and the linker rejects the demo's `.obj` if it refers to a global defined by the platform. See [linker.md](linker.md).
* `/= n` works for any `n` up to `$1000`.

## Animation and drawing notes

* `Draw-frame` receives a monotonically increasing frame counter in `x1`. Derive animation positions from that value for repeatable motion without storing per-frame state. A triangle wave made with division and remainder gives a simple bounce between two bounds.
* The platform alternates two framebuffer pages. Clear the current page or redraw the background every frame, otherwise an object's previous position can remain visible when that page is reused.
* `fill-rect` does not clip. Keep every shape within the screen bounds; for a circle of radius `r`, constrain its center to `r` through `width - r - 1` horizontally and `r` through `height - r - 1` vertically.
* A filled circle can be drawn as horizontal spans: precompute each row's half-width in Racket, then call `fill-rect` once per row. This avoids per-pixel square-root arithmetic in the frame loop.
* If you emit byte or halfword data before platform helpers or other instructions, align the next code to four bytes with `/= 4`. AArch64 instructions require four-byte alignment; an unaligned helper entry can branch to the preceding aligned address and execute table data as code.
* Boot the linked image in QEMU after building. Check for `platform up`, ensure the UART has no `FAULT` record, and use a monitor `screendump` to verify the visible output and animation.

## Constants and helpers

Racket constants from `platform.rkt`: `width` (320), `height` (200), `FPS` (60), `BPP`, `ROW-BYTES`, `PAGE-BYTES` and `(rgb r g b)`, which builds a pixel word with red in the low byte. `BPP`, `ROW-BYTES` and `PAGE-BYTES` follow the `#:bpp` setting.

Routines assembled into the demo's `.obj` (call with `bl name:`; all preserve registers):

| Routine        | Arguments                                                     | Notes                                                       |
|----------------|---------------------------------------------------------------|-------------------------------------------------------------|
| `clear-screen` | `x0` page, `w1` colour                                        | In 8 bit mode `w1` is a palette index                       |
| `fill-rect`    | `x0` page, `x1` x, `x2` y, `x3` width, `x4` height, `w5` colour | Width and height must be at least 1. No clipping            |
| `set-palette`  | `x0` first index, `x1` count, `x2` address of `rgb` words     | 8 bit mode only. Up to 256 entries                          |
| `send-char`    | `w0` byte                                                     | Makes `debug-str`, `debug-reg` and `debug-str-reg` work     |

Everything from `lib/periph.rkt`, `stack.rkt`, `struct.rkt` and `expr.rkt` is re-exported, so `PUSH`, `subr`, `load-immediate`, `/if`, `/for` and the rest are available. See [language.md](language.md).

## What the platform does

1. Parks cores 1 to 3, sets the stack below `$80000`, initialises the mini UART and checks it is at EL2.
2. Installs the vector table in `vbar_el2`.
3. Requests a framebuffer: 320×200 visible, 320×400 virtual (two pages), the chosen depth, RGB order. In 8 bit mode the GPU palette is set later by the demo.
4. Builds identity-mapped page tables and enables the MMU and caches at EL2:
   * RAM is normal write-back
   * GPU memory, from 2MB block 472 (`$3B000000`) up, is normal non-cacheable, so framebuffer writes need no cache maintenance
   * Peripherals, from block 504 (`$3F000000`), and the 1GB above them are device memory
5. Prints `platform up`, calls `Demo-init`, then loops: `Draw-frame` on the back page, flip with the virtual offset tag, wait for the next 1/60 s tick of `cntpct_el0`. If a frame runs late the next deadline is resynced instead of trying to catch up.
6. On any exception the handler prints `FAULT: vector esr elr far`, the four values and a disassembly of 32 instructions starting at ELR, then halts.

Mailbox messages are cleaned to memory with `dc civac` before being sent. `send-vcore-msg` takes the message address in `x8`.

## Memory layout

| Address                     | Contents                                                        |
|-----------------------------|-----------------------------------------------------------------|
| below `$80000`              | Stack                                                           |
| `$80000`                    | Platform code, vectors (`/= $800`), mailbox messages, page tables (`/= $1000`) |
| next 4KB boundary           | the demo's `.obj` (about `$84000` at the moment)                 |
| about `$3C100000`           | Framebuffer, two pages, returned by the GPU                     |
| `$3F000000`                 | Peripherals                                                     |

## Open items

* DMA is not used anywhere yet. QEMU supports it (see [qemu-and-debugging.md](qemu-and-debugging.md)), so a DMA clear or blit is a possible next step.
* Some routines use raw `w` registers where a register alias would read better. Using an `x` alias with `ldrb`/`strb`, or two aliases for one register, has not been tested.
* Nothing has been tried on real hardware.
