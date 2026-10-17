---
title: Agent guide for Pisemble
description: Start here. Ground rules, environment, repository map and the most important gotchas for agents working on Pisemble and its Raspberry Pi 3 bare-metal code
ms.date: 2026-10-17
---

## What this repository is

Pisemble is a Racket-based AArch64 assembler and linker written by the owner (`#lang pisemble`). It is used to write bare-metal Raspberry Pi 3 programs. The owner currently runs everything under QEMU (`-M raspi3b`), not on real hardware. The repo also contains the `pi2d` layer (`lib/pi2d.rkt`) and example programs (`examples/`). The Wolfenstein 3D raycaster has its own repo (`pisemble-wolf3d`), and loose scratch files, host tooling and reference PDFs live in a private ancillary repo (`pisemble-ancillary`). Neither is part of this package.

The assembler is a work in progress. It supports a subset of AArch64, and instructions are added as the owner needs them. Expect to work around missing instructions rather than add them.

## Ground rules

* Do not change the core of Pisemble without asking first. The core is `expander.rkt`, `objfile.rkt`, `dwarf.rkt`, `reader.rkt`, `main.rkt`, `lang/`, `base/` and `lib/`. Work around limitations in user code (raw opcodes, macros) and propose core changes instead of making them.
* Target QEMU unless told otherwise. Note where behaviour would differ on hardware (caches, timing, DMA).
* Do not rely on the PMU; the owner asked to avoid it.
* Keep example and pi2d code clean: the owner wants to "just write graphics code".
* Do not add loose scratch files, PDFs, binaries or build outputs to the repo root. Build outputs (`*.img`, `*.obj`, `*.elf`, `compiled/`) are git-ignored.

## Environment (Windows)

* Racket: `C:\Program Files\Racket\racket.exe`
* QEMU: `C:\Program Files\qemu\qemu-system-aarch64.exe`
* .NET: `dotnet fsi comms.fsx` runs the UART listener. `comms.fsx` lives in the `pisemble-ancillary` repo. It references `C:\repos\Disarm\Disarm\bin\Debug\netstandard2.0\Disarm.dll` for disassembly.
* Build a program by running its source file: `racket file.rkt`. Output files (`.obj`, `.img`) are written to the current directory, so `cd` to the source folder first.

## Repository map

| Path                       | Purpose                                                                    |
|----------------------------|----------------------------------------------------------------------------|
| `expander.rkt`             | Assembler core: instruction table, encoders, `arm-line`, `aarch64` macro   |
| `objfile.rkt`              | Object file format and the merge step of the linker                        |
| `reader.rkt`, `lang/`      | Reader extensions (`{}`, `$`, `%`, `@`) and the `#lang pisemble` hookup    |
| `lib/periph.rkt`           | Peripheral addresses, `init-uart`, `debug-str`, `uart-object`              |
| `lib/stack.rkt`            | `PUSH`/`POP`, `regs` (register renaming), `subr`, `preserve`               |
| `lib/struct.rkt`           | `load-immediate`, `/struct`                                                |
| `lib/expr.rkt`             | Structured control flow: `/if`, `/when`, `/cond`, `/whilenz`, `/while` ... |
| `base/main.rkt`            | `pisemble/base` language: `racket/base` plus the assembler, for building other `#lang`s |
| `lib/platform.rkt`         | Platform (boot, framebuffer, MMU, frame loop)                              |
| `lib/pi2d.rkt`             | `pi2d` form: size and depth, and the `#:init`, `#:update`, `#:render` hooks |
| `examples/plasma.rkt`      | 32 bit plasma example built with `pi2d`                                    |
| `examples/cycle.rkt`       | 8 bit palette-cycling example built with `pi2d`                            |
| `examples/stars3d.rkt`     | Minimal 3D starfield example built with `pi2d`                             |
| `examples/floor-cube.rkt`  | Rotating textured cube (single precision FPU slab test) over a floor caster |
| `test/`                    | rackunit tests and the self-checking `instruction-test.rkt`                |

## Detailed context

Read the file that matches your task before editing:

* [agent-context/language.md](agent-context/language.md) covers syntax, supported instructions, raw opcode workarounds and the helper macro libraries
* [agent-context/linker.md](agent-context/linker.md) covers object files, images, global labels, relocation and alignment
* [agent-context/qemu-and-debugging.md](agent-context/qemu-and-debugging.md) covers running QEMU, the monitor-based debugging workflow, the UART protocol and what QEMU does and does not emulate
* [agent-context/pi2d.md](agent-context/pi2d.md) covers the `pi2d` hooks in `examples/`, the frame contract and how to write a new program

## Top gotchas

1. Labels that start with a capital letter (`:Foo`) are global linker symbols. Every reference to them goes through link-time relocation, even inside one file, and only `b`, `bl`, `b.cond`, `cbz`, `cbnz`, `adr` and `ldr x` literal can be relocated. `ldr w0 Foo:` fails at link time.
2. A plain reference `name:` errors if more than one label with that name exists in the program. Use `name+` (nearest forward) or `name-` (nearest backward) for repeated local labels such as `:loop`.
3. There are no logical immediates (`and x0 x0 @255` does not exist) and no register-offset addressing (`ldr x0 [x1 x2]`). Put the mask or offset in a register first.
4. `mrs`/`msr` only know a handful of system registers. Emit others with `(write-value-32 opcode)`. See the opcode table in [language.md](agent-context/language.md).
5. `/whilenz`, `/whilez`, `/while` and `/for` test before the body, so the body can run zero times. Use `/do-whilenz`, `/do-whilez`, `/do-while` when the body must run at least once.
6. QEMU `-kernel` enters at EL2, not EL1. Use EL2 system registers.
7. Instruction-level problems usually show up as a silent hang or black screen. Use the QEMU monitor (`info registers`, `x /20i $pc`, `screendump`) rather than guessing; see [qemu-and-debugging.md](agent-context/qemu-and-debugging.md).
8. Building is only partly incremental. `<name>.img` is always relinked. `<name>.obj` is skipped when it and its `.obj.dbg` sidecar are not older than the source module, the modules it requires, or the objects it links. Delete the `.obj` to force a rebuild.
