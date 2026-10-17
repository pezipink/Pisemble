---
title: Pisemble linker and object files
description: How the aarch64 form builds object files and images, how global labels and relocation work, alignment rules and known linker limitations
ms.date: 2026-10-17
---

## The aarch64 form

Everything is assembled by the `aarch64` macro in `expander.rkt`:

```racket
(aarch64 "demo.obj" [] { ...code... })                      ; object file
(aarch64 "demo.img" ["demo.obj" "other.obj"] { ... })    ; linked image
```

* The output file name decides the kind: names ending in `.img` produce a raw binary image, anything else a Pisemble object file.
* The link list works for both kinds. An object can be built from other objects.
* Each form starts with a fresh assembler state. Several forms can run in one source file, in order. `demo/platform.rkt` uses this: `(demo ...)` builds `<name>.obj`, then builds `<name>.img` from it, both beside the demo source.
* Files are written to the current directory, and there is no up-to-date check. Every run reassembles and relinks.
* Object files are read at link time from the current directory, so build the objects first (or in the same file, earlier).

## Image layout and addresses

The image is the raw bytes of the image's own code, followed by each linked object. Locations are 0-based: the first byte of the image is location 0. Under QEMU (`-kernel`) and with the default Pi firmware settings the image is loaded and entered at `$80000`.

* PC-relative code (`b`, `adr`, `ldr` literal) works anywhere, so the load address normally does not matter.
* Any absolute address computed at assembly time from a location must add the load address.
* `(resolve-global-label-address :Label)` reserves 8 bytes and, at image link time, writes the label's 0-based image offset into the low 4 bytes. That equals the real address only if the image runs at address 0 (`kernel_old=1`, as in `config-pi3`). Under QEMU it is `$80000` too small. `wolfmain.rkt` (pisemble-wolf3d repo) uses this for its data objects.

## Global labels

A label is global when the first character after the colon is upper case (`:Draw-frame`, `:AUXB`). The test is `is-global-label?` in `expander.rkt`.

* A global label is added to the object's symbol table.
* Every reference to a global label is recorded as an external target and resolved only when an image is linked. This applies even when the label is defined in the same file.
* Only these instructions can be relocated at link time: `b`, `bl`, `b.cond`, `cbz`, `cbnz`, `adr` and `ldr x` literal. Anything else, including `ldr w` literal, fails with "could not relocate instruction". Load 32 bit values through a lower-case label, or load the 64 bit form.
* Lower-case labels are local to the object and resolved during assembly.
* Defining a global whose name already appears in a linked object's string table errors with "could not create the global label X since it already exists in a linked object".

> [!IMPORTANT]
> An image cannot link an object that refers to a global the image itself defines. The check in the `aarch64` macro compares the image's globals with every name in the merged objects' string table, and that table also contains names the objects only reference. Linking fails with "could not link as the global symbol X already exists". In practice, an object can define globals that the image calls, but cannot call back into globals defined by the image. `demo/platform.rkt` works around this by assembling its own copies of helpers such as `send-char` into the demo's `.obj` under lower-case names. A fix would compare against the merged symbol table instead; that is a core change, so ask the owner first.

When two objects in the link list define the same global, `merge-objs` in `objfile.rkt` stops with "could not link as the global symbol X is defined in more than one object".

## Alignment

`append-data-aligned` in `objfile.rkt` appends each linked object at the next 4KB (`$1000`) boundary.

* `/= n` aligns relative to the start of the object being assembled. Since objects start on 4KB boundaries, `/= n` is exact for any `n` up to `$1000`, including 2KB vector tables and 4KB page tables.
* `/= n` above `$1000` inside an object is not honoured, and nothing warns about it. In the image's own code it is exact, because the image starts at a highly aligned address.
* The same function pads the end of an object to 4KB when it is written, so each object ends with up to 4KB of zeros. This wastes some file size and nothing else.

Before 2026-10-17 the boundary was 16 bytes. Older notes or code comments that mention 16 byte object alignment are out of date.

## Object file format

Little-endian, written by `write-obj-file` and read by `read-obj-file`:

1. Magic number, 64 bits (`pisemobj`)
2. String table: count (u32), then for each entry an index (u32), a length (u32) and the characters
3. Object composition: count (u32), then string indexes (u32) of the object files already merged in. Linking the same object twice is skipped with "already linked"
4. Symbol table: count (u32), then (string index u32, offset u64) for each global defined
5. Target table: count (u32), then (string index u32, offset u64) for each unresolved reference to a global
6. Data: length (u64), then the bytes

## Where the code is

* `aarch64` macro, global label handling and final relocation: `expander.rkt`, from the `(define-syntax (aarch64 stx)` line to the end of the file
* `is-global-label?`, `set-jump-source`: `expander.rkt`, near the `context` struct
* Merging, alignment and the file format: `objfile.rkt`
