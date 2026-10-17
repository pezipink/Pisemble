---
title: Configuring the MMU on the Raspberry Pi 3 with Pisemble
description: Learn how the AArch64 MMU translates addresses at EL2, how to build page tables and memory attributes for the BCM2837, and how to enable the MMU safely from Pisemble code.
ms.date: 2026-10-17
ms.topic: tutorial
---

## What you will learn

* What the MMU does and why bare-metal code needs it.
* What the EL2 translation registers (TCR_EL2, MAIR_EL2, TTBR0_EL2, SCTLR_EL2) control.
* How a 32-bit virtual address is split into table indexes at each level.
* How block descriptors set the memory type and access rules for 2MB and 1GB regions.
* How to build the tables in Pisemble, write the EL2 system registers, and enable the MMU.
* How to check the result and what to do when the system faults or hangs.

You will write the code yourself. The sections below give you the concepts, the register layouts, and short fragments. They do not give you a complete program.

## Before you start

Read these project notes first:

* [language.md](../../agent-context/language.md) for the Pisemble syntax used in the snippets, including the raw opcode rules.
* [qemu-and-debugging.md](../../agent-context/qemu-and-debugging.md) for how the project runs under QEMU and which monitor commands are available.
* [platform.rkt](../../lib/platform.rkt) for the reference `setup-mmu` macro and the memory map constants. The snippets below follow this file.
* [dma.md](dma.md) if you have not read the DMA tutorial. It covers the cache rules that also apply here.

The project boots under QEMU with `-M raspi3b` and `-kernel`, which enters the image at EL2. The code here runs at EL2 and configures the EL2 translation regime. If your code runs at EL1, use the EL1 registers (`tcr_el1`, `mair_el1`, `ttbr0_el1`, `sctlr_el1`) instead, and the field layouts change.

## What the MMU does

The MMU (memory management unit) translates every address the CPU uses into a physical address. It does this with page tables that you build in RAM. The translation also carries attributes for each region, such as whether the region is cacheable and whether the CPU may write to it.

Without the MMU, the CPU treats every address as Device memory. Device memory is not cached, and the CPU must not reorder accesses to it. This is correct for peripherals but slow for RAM, and the data cache cannot be used for normal memory.

The project uses an identity map. Each virtual address equals its physical address. This is the simplest safe choice when you turn the MMU on, because the program counter, the stack, and the data your code is using all keep the same addresses before and after the switch. Once the MMU is on, you can change the mapping, but you do not have to change it to get started.

## The EL2 translation registers

At EL2 there is one translation regime. It uses TTBR0_EL2 only, so there is no TTBR1 half. The four registers below control it.

### TCR_EL2

TCR_EL2 (translation control register) sets the address size, the granule size, the walk attributes, and the shareability. `platform.rkt` uses the value `$80803520`.

| Field | Bits | Value in `platform.rkt` | Meaning |
| ----- | ---- | ----------------------- | ------- |
| T0SZ | 5:0 | 32 | Virtual addresses are 32 bits, so the input space is 4GB. |
| PS | 18:16 | 0 | Physical addresses are 32 bits. |
| TG0 | 15:14 | 0 | 4KB translation granule. |
| SH0 | 13:12 | 3 | Table walks are inner shareable. |
| ORGN0 | 11:10 | 1 | Outer write-back, write-allocate table walks. |
| IRGN0 | 9:8 | 1 | Inner write-back, write-allocate table walks. |
| RES1 | 31, 23 | 1 | Always 1. |

The walk attributes apply to the table fetches, not to the data your code reads and writes. The data attributes come from the descriptors.

### MAIR_EL2

MAIR_EL2 (memory attribute indirection register) holds eight 8-bit attribute slots. A descriptor picks a slot by its AttrIdx field. The value `platform.rkt` uses is `$FF440400`.

| AttrIdx | Byte in MAIR | Memory type | Used for |
| ------- | ------------ | ----------- | -------- |
| 0 | `$00` | Device-nGnRnE | Peripherals |
| 1 | `$04` | Device-nGnRE | Not used |
| 2 | `$44` | Normal, non-cacheable | GPU memory, such as the framebuffer |
| 3 | `$FF` | Normal, write-back | RAM |

Device-nGnRnE means no gathering, no reordering, and no early write acknowledgement. Use it for registers that have side effects. Device-nGnRE allows early write acknowledgement, but it still does not allow gathering or reordering.

### TTBR0_EL2

TTBR0_EL2 holds the physical address of the level 1 table. The table must be aligned to its size. `platform.rkt` places the tables on a 4KB boundary, which is always enough for this layout.

### SCTLR_EL2

SCTLR_EL2 (system control register) switches the features on and off. The bits you need here are:

| Bit | Name | Meaning when set |
| --- | ---- | ---------------- |
| 0 | M | Enable the MMU for EL2. |
| 2 | C | Enable the data cache. |
| 12 | I | Enable the instruction cache. |

The value `$1005` sets all three bits, which is what `platform.rkt` uses.

## How addresses are split

With T0SZ set to 32 and the 4KB granule, the walk starts at level 1. Each level uses a different slice of the virtual address.

| Level | Address bits | Index range | Size covered by one entry |
| ----- | ------------ | ----------- | ------------------------- |
| 1 | VA[31:30] | 0 to 3 | 1GB |
| 2 | VA[29:21] | 0 to 511 | 2MB |

Level 1 has four entries that matter for a 32-bit address space. Level 2 has 512 entries, and each one covers a 2MB block. Together the level 2 table maps the first 1GB, and the level 1 table selects which 1GB region the address is in.

To find the level 2 index for an address, shift it right by 21 bits. For example, the mini UART base `$3F215000` gives `$3F215000 >> 21 = 505`, so it is entry 505 of the level 2 table.

## Descriptor bits

Each table entry is a 64-bit descriptor. The bits that matter for this setup are:

| Bits | Name | Meaning |
| ---- | ---- | ------- |
| 0 | Valid | 1 means the entry is used. 0 means translation fault. |
| 1 | Table or block | At level 1 or 2, 1 means table, 0 means block. |
| 4:2 | AttrIdx | Index into MAIR_EL2. |
| 6 | AP[1] | Set by `platform.rkt`. Keep it as shown. The setup here does not depend on it. |
| 7 | AP[2] | 1 makes the region read-only. |
| 9:8 | SH | Shareability. `11` is inner shareable. |
| 10 | AF | Access flag. Must be 1, or the first access faults. |

A level 2 block descriptor in `platform.rkt` is built as `$741 + attr * 4`. `$741` sets valid, block, AP[1], SH to inner shareable, and AF. Changing AttrIdx means changing bits 4:2, so the difference between two attributes is `4 * (from xor to)`. This is what `attr-flip` computes.

A level 1 table descriptor points to a level 2 table. It sets bits 1:0 to `11`, and the address is the table's physical address.

## The memory map

`platform.rkt` uses this map. Each row is a range of level 2 entries, each entry being 2MB.

| Virtual range | Entries | AttrIdx | Region |
| ------------- | ------- | ------- | ------ |
| `$00000000` to `$3AFFFFFF` | 0 to 471 | 3 (write-back) | RAM |
| `$3B000000` to `$3EFFFFFF` | 472 to 503 | 2 (non-cacheable) | GPU memory, framebuffer |
| `$3F000000` to `$3FFFFFFF` | 504 to 511 | 0 (Device-nGnRnE) | BCM2837 peripherals |
| `$40000000` to `$7FFFFFFF` | Level 1 entry 1 | 0 (Device-nGnRnE) | ARM local peripherals, one 1GB block |

Level 1 entry 0 is a table descriptor that points to the level 2 table. Level 1 entries 2 and 3 are left zero, so any access to `$80000000` and above faults.

The code computes the start of the GPU region from an 80MB figure, but the peripheral range fixes the end at entry 504. The non-cacheable window therefore covers 32 entries, which is 64MB. If you change the GPU size, check the end of the window against the peripheral range before you rely on it.

## Building the tables in Pisemble

The tables are plain data in the image. Put them on a 4KB boundary, then reserve space for both levels. This is how `platform.rkt` places them:

```txt
/= $1000
:id_pgd
(reserve $2000)
```

The level 1 table is at `id_pgd`. The level 2 table is at `id_pgd + $1000`. Both are 4KB tables, and only the first few entries of the level 1 table are used.

The fill loop writes one descriptor per block and increments the output address by 2MB each time. `fill-blocks` is the macro from `platform.rkt` that does this:

```txt
(define-syntax-parser fill-blocks
  [(_ n:expr)
   #'{
      (load-immediate x4 n)
    :fill
      str x2 [x0] @8
      add x2 x2 x5
      sub x4 x4 @1
      cbnz x4 fill-
     }])
```

Before the loop, `x0` holds the table pointer, `x2` holds the first descriptor, and `x5` holds `$200000` (2MB). Each store uses post-index addressing, so `x0` moves to the next entry automatically.

To change the attribute of the next run of blocks, flip the AttrIdx bits in `x2` with `eor`. The fragment from `platform.rkt` changes write-back to non-cacheable, then non-cacheable to device:

```txt
(load-immediate x6 (attr-flip ATTR-WB ATTR-NC))
eor x2 x2 x6
(fill-blocks (- L2-PERIPH-START L2-GPU-START))
```

The fill uses `(- L2-PERIPH-START L2-GPU-START)` as the count, so the number of entries follows the memory map constants. Keep the constants and the code in step.

The level 1 entries are written first. Entry 0 points to the level 2 table, and entry 1 is the 1GB device block:

```txt
adr x0 id_pgd:
(load-immediate x3 $1000)
add x10 x0 x3
mov x3 @3
orr x1 x10 x3
str x1 [x0] @8
(load-immediate x2 $40000741)
str x2 [x0] @8
```

## Writing EL2 system registers

Pisemble has a limited set of `mrs` and `msr` forms, so the system register writes use raw opcodes through `write-value-32`. The EL1 encodings are easier to read, and `el-op` changes them to EL2 at build time. The EL1 form of each system register uses `op1 = 0`. EL2 registers use `op1 = 4`, so `el-op` adds `4 << 16`, which is `$40000`.

The table below lists the EL2 encodings that `el-op` produces. Each line also gives the EL1 literal you write in the source.

| Instruction | Source literal | Encoding at EL2 |
| ----------- | -------------- | --------------- |
| `msr tcr_el2, x0` | `$d5182040` | `$d51c2040` |
| `msr mair_el2, x0` | `$d518a200` | `$d51ca200` |
| `msr ttbr0_el2, x0` | `$d5182000` | `$d51c2000` |
| `msr sctlr_el2, x0` | `$d5181000` | `$d51c1000` |
| `mrs x0, sctlr_el2` | `$d5381000` | `$d53c1000` |
| `msr vbar_el2, x0` | `$d518c000` | `$d51cc000` |
| `tlbi alle2` | `$d508871f` | `$d50c871f` |

Write the EL1 literal and let `el-op` patch it, as this fragment does:

```txt
(load-immediate x0 $80803520)         ; TCR: T0SZ=32, 4KB granule, WB/WB, inner shareable
(write-value-32 (el-op $d5182040))    ; msr tcr_el2, x0
(load-immediate x0 $FF440400)         ; MAIR: attr0 nGnRnE, attr1 nGnRE, attr2 NC, attr3 WB
(write-value-32 (el-op $d518a200))    ; msr mair_el2, x0
```

The barriers are raw opcodes too: `dsb sy` is `$d5033f9f` and `isb` is `$d5033fdf`.

## Enabling the MMU

Enabling the MMU changes how every later instruction and data access is translated. Follow the order below, which is the order `platform.rkt` uses:

1. Write TCR_EL2, MAIR_EL2, and TTBR0_EL2.
2. Fill the level 1 and level 2 tables.
3. Run `dsb sy` so the table writes are complete before the table walker reads them.
4. Invalidate the EL2 TLB with `tlbi alle2`, then run `dsb sy` and `isb`.
5. Read SCTLR_EL2, set M, C, and I, and write it back.
6. Run `dsb sy` and `isb` again so the next instruction runs with the new settings.

```txt
(write-value-32 $d5033f9f)            ; dsb sy
(write-value-32 (el-op $d508871f))    ; tlbi alle2
(write-value-32 $d5033f9f)            ; dsb sy
(write-value-32 $d5033fdf)            ; isb

(write-value-32 (el-op $d5381000))    ; mrs x0, sctlr_el2
(load-immediate x1 $1005)             ; I | C | M
orr x0 x0 x1
(write-value-32 (el-op $d5181000))    ; msr sctlr_el2, x0
(write-value-32 $d5033f9f)            ; dsb sy
(write-value-32 $d5033fdf)            ; isb
```

The identity map is what makes step 5 safe. After the write, the next instruction is fetched through the new tables, and it finds the same code at the same address. If the mapping for the code is missing, the CPU faults on the instruction fetch, and no later step runs.

Because the MMU and caches are off while the tables are written, the code does not clean the cache before it enables the MMU. This works under QEMU, which does not model caches. See [QEMU and hardware](#qemu-and-hardware) for the hardware case.

## Checking the result

Read SCTLR_EL2 back after the write and print it with the debug helpers from `platform.rkt`. Bit 0 should be 1, and bits 2 and 12 should also be 1:

```txt
(write-value-32 (el-op $d53c1000))    ; mrs x0, sctlr_el2
(debug-str-reg "sctlr_el2 after enable " x0)
```

Check the result in the QEMU monitor after the program runs:

* `x <address>` reads memory through the current translation, which is the virtual view.
* `xp <address>` reads physical memory, which is useful for checking the table contents.

Use `help x` and `help xp` in the monitor to confirm the syntax for your QEMU version. The monitor does not show the translation tables directly, so `xp` on the table address is the way to see the descriptors.

## Fault handling

The vector table in `platform.rkt` sends every exception to a common handler that prints ESR_EL2, ELR_EL2, and FAR_EL2. These three registers tell you what went wrong:

* ESR_EL2 holds the exception class in bits 31:26 and the fault status in bits 5:0.
* ELR_EL2 holds the address of the instruction that faulted.
* FAR_EL2 holds the faulting address for data and instruction aborts.

For translation faults, the fault status code (DFSC or IFSC) is `$04` to `$07`. The low two bits give the level where the walk stopped. For access flag faults, the code is `$08` to `$0B`. An access flag fault usually means you forgot to set AF in a descriptor.

The exception class is `$25` for a data abort taken at EL2, and `$21` for an instruction abort taken at EL2. These values are from the architecture manual. They have not been checked under QEMU.

## Troubleshooting

| Symptom | Likely cause | What to check |
| ------- | ------------ | ------------- |
| The system hangs right after `msr sctlr_el2` | The code page is not mapped, or its level 2 entry is wrong. | Compute `address >> 21` for the code and check that the entry is valid and uses the right AttrIdx. |
| Fault with a fault status of `$04` to `$07` | The entry is zero, or a table descriptor points to the wrong table. | Check level 1 entry 0 and level 2 entries with `xp`. |
| Fault with a fault status of `$08` to `$0B` | AF is 0 in the descriptor. | Check bit 10 of the block descriptor. |
| UART output stops after the MMU is on | The peripheral entry uses the wrong AttrIdx, or the entry is missing. | Check entries 504 to 511 and the device attribute in MAIR. |
| The framebuffer does not update | The GPU region is cacheable, or its entries are missing. | Check the non-cacheable entries from 472 to 503. |
| Table walk faults at random | TTBR0 is misaligned, or the tables were written after the TLB invalidate. | Check the alignment of `id_pgd` and the order of steps 2 to 4. |

## QEMU and hardware

QEMU does not model caches. A program that forgets a cache clean still works in QEMU, so QEMU cannot show you whether your cache maintenance is correct. The project notes in [qemu-and-debugging.md](../../agent-context/qemu-and-debugging.md) cover this.

On a real Raspberry Pi 3, clean the table range to the point of coherency before you enable the MMU, for example with `dc cvac` over the level 1 and level 2 tables. That step is not verified on hardware. Treat it as an open question until you test it on a board.

Multi-core setups need more than the table. `emu_clean.rkt` (pisemble-wolf3d repo) sets the Cortex-A53 SMPEN bit (bit 6 of `cpuectlr_el1`) before it enables the caches. `platform.rkt` does not set it. Read [multicore.md](multicore.md) before you enable the caches on more than one core.

## Exercises

1. Change the RAM entries from write-back to non-cacheable, rebuild, and run the program. Explain why QEMU shows no difference and what you would expect on hardware.
2. Find the level 2 index for `$3F215000` and for `$3B000000`. Check each against the memory map above.
3. Make one RAM block read-only by setting AP[2] (bit 7) in its descriptor. Write a store to that block and read the fault from ESR_EL2 and FAR_EL2.
4. Read the level 1 entry for `$80000000`, which is unmapped. Trigger an access to that address, and identify the fault status code and the level.
5. Add a second 2MB device block for a peripheral you choose. Set its AttrIdx, its AF bit, and its position in the table, and verify it with a read from the QEMU monitor.
