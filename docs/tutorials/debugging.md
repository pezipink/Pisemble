---
title: Debugging Pisemble Programs with gdb
description: Learn how a (breakpoint) form in a Pisemble program becomes a gdb breakpoint in QEMU, how to build and run the program, and how to debug it from the command line or from VS Code.
ms.topic: tutorial
---

## What you will learn

* How `(breakpoint)` turns a line of source into a gdb breakpoint, and why it emits one `nop`.
* How the image address and the gdb address are related.
* How to build an image and start it paused under QEMU.
* How to step through the program from the gdb prompt.
* How to run the same steps from VS Code with the tasks and the debug configuration in `.vscode/`.
* The limits of this workflow.

## Before you start

You need four things:

* QEMU for AArch64, at `C:\Program Files\qemu\qemu-system-aarch64.exe`.
* `gdb-multiarch` from MSYS2. Install MSYS2 to `C:\msys64`, then run `pacman -S mingw-w64-x86_64-gdb-multiarch` in its MSYS2 shell. The ARM toolchain's `arm-none-eabi-gdb` cannot debug AArch64 code.
* The `ms-vscode.cpptools` extension, if you want to use VS Code.
* The project notes in [qemu-and-debugging.md](../../agent-context/qemu-and-debugging.md) for the general QEMU setup.

The examples use [bouncing.rkt](../../examples/bouncing.rkt), which already has a breakpoint in it.

> [!NOTE]
> The paths in this tutorial assume Windows and the install locations above. If you install gdb or QEMU somewhere else, change the paths in `.vscode/tasks.json` and `.vscode/launch.json` to match.

## 1. How a breakpoint gets into gdb

The assembler has no debugger. It produces a raw binary, and gdb knows nothing about that binary unless something tells it where the breakpoints are. The `(breakpoint)` form supplies that information in four steps.

1. Expansion. `(breakpoint)` expands into a call that defines a global label named after its position in the source, such as `:Bp-bouncing-L17C2`. The label marks the `nop` that the form emits. The `nop` gives the breakpoint line its own address, so `next` can leave that line. Without it, the code of the next line would share the address and gdb would report the whole body as the breakpoint line.
2. Linking. When you build an image, the linker places every object in the image and resolves each label to an offset from the start of the image.
3. Writing the gdb file. The image build writes `<name>.gdb` next to `<name>.img`, where `<name>` is the program's file name, such as `bouncing`. It sets the image base to `0x80000`, which is where QEMU loads a raw AArch64 kernel, and adds one `break` line per label.
4. Stopping. When gdb connects to QEMU's gdb stub and loads that file, each `break` line sets a breakpoint at `0x80000 + offset`. The CPU stops when it reaches that address.

The generated file for the sample program looks like this:

```gdb
# breakpoints for C:\repos\Pisemble\examples\bouncing.img, load with: source C:\repos\Pisemble\examples\bouncing.gdb
set $image_base = 0x80000
# Bp-bouncing-L17C2
break *($image_base + 0x4004)
```

The comment above each `break` line gives the label it came from, so you can match a breakpoint to its source line. The offset `0x4004` is specific to this build. If you change the program, the offset can change, so rebuild the image before you debug.

> [!IMPORTANT]
> gdb rejects comments after a `break` command on the same line. The generator writes comments on their own lines for this reason. Do not edit `<name>.gdb` by hand unless you know gdb's syntax well.

## 2. Adding a breakpoint

Put `(breakpoint)` in a body where a body form is allowed. The sample program uses it as the first form in the `#:init` hook, which the platform runs once at startup as `Pi2d-init`:

```racket
(pi2d
  #:init () [] {
    (breakpoint)
    (debug-str "bouncing circles" #t)
  }
  ...)
```

The macro is defined in [platform.rkt](../../lib/platform.rkt). You get it when your program requires the platform.

A few rules apply to breakpoints:

* The label includes the file name without its extension, and the line and column of the `(breakpoint)` form. Two breakpoints in the same file cannot share a line and column. Two files with the same name in different folders can produce the same label, and the link fails with a duplicate-symbol error. Rename one of the files if that happens.
* Breakpoints work only in an image build. If you build an object file, no `.gdb` file is written, and nothing is set in gdb.
* Each breakpoint stops the CPU at its `nop`, before the code of the following line runs. `next` from the breakpoint line moves to the following line, and `next` on that line steps over a `debug-str` call.

## 3. Building the image

From the `examples` folder, run the program with Racket. The program builds the image and writes its output next to the source file. The output names come from the source file, not from the folder you run Racket in, so the results are the same from any directory:

```powershell
cd c:\repos\Pisemble\examples
& "C:\Program Files\Racket\racket.exe" bouncing.rkt
```

The build writes these files, where `bouncing` is the source file's name:

* `bouncing.img`: the raw image that QEMU runs.
* `bouncing.gdb`: the breakpoints for that image.
* `bouncing.obj`: the program's own object file, which the image links.
* `bouncing.elf`: a wrapper around `bouncing.img`. gdb only accepts ELF files as the program, so the debug session loads this file. It holds the same bytes at the same address, plus DWARF line information that maps addresses to `.rkt` lines. QEMU still runs the `.img`.

Each program gets its own names, so building `plasma.rkt` writes `plasma.img` and does not overwrite `bouncing.img`.

## 4. Debugging from the command line

You need two processes: QEMU, which runs the program and waits for gdb, and gdb, which controls it.

### Start QEMU paused

Start QEMU with `-S`, so the CPU does not run until gdb continues it, and with `-gdb`, so QEMU listens for gdb on port 1234:

```powershell
& "C:\Program Files\qemu\qemu-system-aarch64.exe" -M raspi3b -kernel bouncing.img -serial null -serial null -S -gdb tcp:127.0.0.1:1234
```

The first `-serial null` is a placeholder. The second one sends the mini UART nowhere. Use `-serial stdio` in its place when you want to see UART output in the terminal.

QEMU does not print anything and waits. Leave that window open.

### Attach gdb

Open a second terminal in the folder that holds `bouncing.img`, and start gdb. Run these commands at the gdb prompt in order:

```gdb
set architecture aarch64
target remote 127.0.0.1:1234
source bouncing.gdb
continue
```

The `continue` command runs the program until the first breakpoint. gdb prints the address where it stopped, and the program is paused there.

If you want a single command that runs the same steps and then prints the registers, use batch mode:

```powershell
& "C:\msys64\mingw64\bin\gdb-multiarch.exe" -batch -nx -ex "set architecture aarch64" -ex "target remote 127.0.0.1:1234" -ex "source bouncing.gdb" -ex "continue" -ex "info registers pc" -ex "x/3i `$pc"
```

The order matters. Set the architecture before you connect, and load `bouncing.gdb` after you connect. Setting a breakpoint after the program has run past its address does nothing, because the program will not return to that address before the session ends.

### Inspect the program

These commands work at the gdb prompt while the program is paused:

| Command | What it shows |
| ------- | ------------- |
| `x/8i $pc` | The next eight instructions, starting at the current one. |
| `info registers` | All general-purpose registers and the flags. |
| `info registers x0` | One register. |
| `x/4gx $sp` | Four 64-bit words at the stack pointer. |
| `stepi` | Run one instruction, then stop. Enter `stepi` again to repeat. |
| `nexti` | Run one instruction, stepping over calls such as `bl`. |
| `info breakpoints` | Every breakpoint that is set, with its address. |
| `break bouncing.rkt:17` | Set a breakpoint on a source line. This works only when the `.elf` was loaded as the program, which the VS Code flow does. |
| `continue` | Run until the next breakpoint or until the program stops. |
| `quit` | Leave gdb. QEMU stops when gdb disconnects. |

> [!NOTE]
> This command-line session loads the `.img`, which has no line information, so gdb shows addresses only. Use `x/8i $pc` to read the instructions. The VS Code flow loads the `.elf` instead and shows source lines.

## 5. Debugging from VS Code

The workspace has tasks and a launch configuration in `.vscode/`. Open the program you want to debug, such as `examples/bouncing.rkt`, and press F5. You do not need to type the commands above.

| Action | How to run it | What it does |
| ------ | ------------- | ------------ |
| Debug the open file | Press F5, or Run and Debug, then `Debug image in QEMU (gdb-multiarch)` | Builds the open `.rkt` file, starts QEMU paused with the gdb stub, attaches the debugger, loads the file's `.gdb` breakpoints, and stops at the first breakpoint. The editor shows the source line. |
| Set a breakpoint in the editor | Click the gutter next to a line in a `.rkt` file, or press F9 | Adds a breakpoint on that source line. The workspace setting `debug.allowBreakpointsEverywhere` in `.vscode/settings.json` lets you set it on any line, not only on lines the debugger already knows about. Build the file first so the line table is current. |
| Build the image | `Ctrl+Shift+B` on an open `.rkt` file | Runs `racket` on the file from its folder. Writes `<name>.img`, `<name>.gdb`, `<name>.obj`, and `<name>.elf` beside the file. |
| Run without debugging | Run the task `Build and run in QEMU` | Builds the image, then runs QEMU with the UART on stdout. |
| Stop QEMU | Run the task `Stop QEMU` | Stops the QEMU process that holds the gdb port. Normally you do not need this, because the debug session stops it when it ends. |

The debug button works on the file that is open in the editor. It builds that file, so the breakpoints in the `.gdb` file match the code you are looking at. Open a different program and press F5 to debug it.

> [!WARNING]
> The debug session starts QEMU as a hidden process, and it stops QEMU when the session ends. If the session fails part way through, QEMU can keep running and hold port 1234, and the next session then fails to connect. Run `Stop QEMU` to clear it.


## 6. Limits of this workflow

* Source lines and function names only. The `.elf` maps addresses to `.rkt` lines and names the functions from `subr`, and it has call-frame info, so `bt`, `next`, `step` and `finish` work through helper calls. Register renames from `regs` and `subr` appear as variables in the Variables panel and in `info locals`, with their values in the top frame. Other registers are read directly with `info registers`.
* Register variables are only trustworthy in the top frame. Registers that a helper may overwrite (x0 to x18) and registers that the call-frame info does not track (x19 to x29) show the callee's values in outer frames. Expect wrong values there.
* Register variables show as unions. A 64-bit rename shows `u64`, `i64` and `f64` members (`reg64`), and a 32-bit rename shows `u32`, `i32` and `f32` members (`reg32`). Expand the variable to pick the interpretation you need. Integers show in hex because `.vscode/launch.json` sets `set output-radix 16` in gdb. The same union also shows `unsigned (dec)`, `signed (dec)`, `bits` rows (32 bits per row, grouped by byte), `bytes (little endian)` and `ascii` rows. These come from the gdb pretty-printer in `tools/gdb/reg-printers.py` (pisemble-ancillary repo), which `.vscode/launch.json` there loads with `source` in `setupCommands`.
* Hovering over a register name in a `.rkt` file shows its value while a `cppdbg` session is paused. The C/C++ extension ignores `.rkt` files, so the local `pisemble.register-hover` extension in `tools/register-hover/` (pisemble-ancillary repo) provides the hover. Run `tools\register-hover\install.ps1` once, then reload the window. It handles raw names such as `x14`, `w0` and `sp`, and renames from `regs` in the same file, such as `ptr (x3)`. Renames from other files are not resolved. The hover lists the value in hex, unsigned, signed and float form, then the bits grouped by byte in 32-bit rows, then the bytes in memory order and as ASCII. The bytes are little-endian, so the low byte comes first. The hover does not flag outer frames, so treat values there with the care described above.
* Code produced by a macro maps to the line of the macro use, not the line inside the macro. For example, code from `(pi2d` shows as that line in `rotozoomer.rkt`. Code written directly in your file keeps its own line.
* The prologue of a `subr` is the exception. Stepping into a helper can stop first on a line in the macro template, such as `lib/stack.rkt`, before it reaches the body in your file. Press `step` again to continue into the body.
* The `brk` instruction does not stop gdb. A `brk` instruction in the program goes to the platform's fault handler, which prints the exception registers over the UART. Use `(breakpoint)` to stop in gdb.
* Addresses are image addresses. The generated file assumes the image is loaded at `0x80000` and that the code runs at that address. A breakpoint in code that runs under a different virtual mapping, such as after the MMU is turned on with a different layout, may never hit. So far only the breakpoint in the `#:init` hook (`Pi2d-init` in the generated code) has been tested.
* Rebuild after each change. The offsets in `<name>.gdb` describe one build. After you edit the source, the old offsets point at the wrong instructions. Pressing F5 rebuilds the file for you.

## 7. Troubleshooting

| Symptom | Likely cause |
| ------- | ------------ |
| `target remote` fails with a connection error | QEMU is not running, or it was started without `-gdb tcp:127.0.0.1:1234`. |
| The program runs to the end without stopping | `<name>.gdb` was loaded after the program ran past the breakpoint, or the file is stale. Restart QEMU, reload the file, and check `info breakpoints`. |
| gdb reports a syntax error in `<name>.gdb` | The file was edited by hand, or an old version of the generator wrote trailing comments. Rebuild the image. |
| F5 says port 1234 did not open, or `target remote` fails | An earlier QEMU is still running. Run `Stop QEMU`, then press F5 again. |
| The build fails with a duplicate symbol for `:Bp-...` | Two breakpoints have the same file name and position. See the rules in section 2. |
| `arm-none-eabi-gdb` shows an error about the architecture | Use `gdb-multiarch` instead. The ARM toolchain's gdb has no AArch64 support. |
