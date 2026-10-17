---
title: Running and debugging under QEMU
description: QEMU commands for the Pi 3 kernel, monitor-driven debugging, the UART protocol used by comms.fsx, and what QEMU does and does not emulate
ms.date: 2026-10-17
---

## Tools

* Racket: `C:\Program Files\Racket\racket.exe`
* QEMU: `C:\Program Files\qemu\qemu-system-aarch64.exe`
* UART listener: `dotnet fsi comms.fsx` from the root of the `pisemble-ancillary` repo. It loads `Disarm.dll` from `C:\repos\Disarm\Disarm\bin\Debug\netstandard2.0\` to disassemble code sent over the UART.

The owner targets QEMU only for now. Code has not been tested on a real Pi 3.

## Running

The Pi 3 has two UARTs. The mini UART, which all the code here uses, is QEMU's second serial port, so the first `-serial` must be a placeholder.

Headless, capturing UART output to a file and opening the monitor on TCP:

```powershell
& "C:\Program Files\qemu\qemu-system-aarch64.exe" -M raspi3b -kernel bouncing.img `
  -display none -serial null -serial file:uart.bin `
  -monitor tcp:127.0.0.1:45454,server,nowait `
  -d int,guest_errors -D qemu.log
```

With the listener: use `-serial tcp:127.0.0.1:5555,server=on,wait=on` as the second serial port, start QEMU, then run `dotnet fsi comms.fsx`. QEMU waits for the connection before the guest starts.

### Playing with the keyboard

Two ways to start QEMU, both open the mini UART on `tcp:127.0.0.1:5555`:

* Run the "Run in QEMU (play, WASD)" task. The guest runs straight away.
* Press F5 with the image's source file active (the "Debug image in QEMU" launch). QEMU starts paused for gdb and the guest runs once gdb continues.

Then, in a terminal at the repo root, run `dotnet fsi control.fsx`. It retries the connection for about 10 seconds, so start it after QEMU is up. Once it prints "Connected", the controls are:

| Key | Action |
|-----|--------|
| W / S | Move forward / back |
| A / D | Turn left / right |
| Esc | Quit the controller (sends 0, so the player stops) |

The controller sends one byte per change, bits 0 to 3 for W, A, S and D, with a 100 ms heartbeat. The guest reads the latest byte in `Read-input` each frame. Key state is read globally, so WASD typed into other windows also moves the player. Guest UART output is echoed to the controller's terminal.

Stop the session by pressing Esc, then closing the QEMU window. For F5 sessions, use the debugger's stop button, which runs "Stop QEMU". Do not run `comms.fsx` on port 5555 at the same time as the controller.

For a visible window, drop `-display none`.

As an agent, start QEMU as an async shell and stop it with `stop_powershell` when finished. It never exits on its own.

## Debugging with the monitor

The monitor is the most reliable way to see what the guest is doing. Send commands from PowerShell over TCP:

```powershell
$c = [Net.Sockets.TcpClient]::new("127.0.0.1", 45454)
$s = $c.GetStream(); $w = [IO.StreamWriter]::new($s); $w.AutoFlush = $true
$r = [IO.StreamReader]::new($s)
Start-Sleep -Milliseconds 300
$w.WriteLine("info registers")
Start-Sleep -Milliseconds 500
while ($s.DataAvailable) { [char]$r.Read() | Write-Host -NoNewline }
$c.Close()
```

Useful commands:

* `info registers` shows PC, registers and the exception level (`EL2h` while the platform runs)
* `x /20i $pc` disassembles at the PC; `x /8gx addr` dumps memory by virtual address
* `xp /8wx addr` dumps physical memory
* `stop` and `cont` pause and resume; sample the PC a few times to see where a loop is spinning
* `quit` exits QEMU, which is handy when it was started with `Start-Process`
* `screendump frame.ppm` saves the display as PPM. Convert it to PNG (for example with `System.Drawing`) to view it. Take two dumps a moment apart to confirm animation

`uart.bin` can be read while QEMU is running if you open it with `FileShare.ReadWrite`. It contains the binary protocol below, not plain text.

`qemu.log` with `-d int,guest_errors` logs every exception and every bad peripheral access. In a healthy run it contains one line, `bcm2835_gpio_write: Bad offset e8`, caused by `init-uart` and harmless.

Triage for a black or frozen screen:

1. Check `qemu.log` for exceptions. The platform's fault handler also prints the vector, ESR, ELR and FAR over the UART, followed by a disassembly starting at ELR.
2. If there are no faults, sample the PC. If it is in the frame loop the problem is drawing logic: wrong page address, colours too dark to see, or something moving too fast.
3. Take a `screendump` rather than trusting a window that may not have refreshed.

## Debugging with gdb and breakpoints

Use `(breakpoint)` anywhere a body form is allowed, such as inside a `subr`. It emits one `nop`, so its line has its own address; without it, `next` cannot leave the line. The label `:Bp-<file>-L<line>C<col>` is defined on that `nop`, where `<file>` is the source file name without its extension. When the image is linked, `<image>.gdb` is written next to it, with one `break *(0x80000 + offset)` per breakpoint. A program that uses breakpoints must build as an image, not an object.

Setup, once: `gdb-multiarch` from MSYS2 (`pacman -S mingw-w64-x86_64-gdb-multiarch` in `C:\msys64`). The ARM toolchain's `arm-none-eabi-gdb` cannot debug AArch64.

VS Code (needs `ms-vscode.cpptools`), using the files in `.vscode/`:

* `Ctrl+Shift+B` (Build image) runs `racket` on the open `.rkt` file, from its folder. It writes `<name>.img`, `<name>.gdb`, `<name>.obj` and `<name>.elf` beside the source file, where `<name>` is the file name without its extension.
* `Build and run in QEMU` builds, then runs QEMU with the UART on stdout.
* F5 with `Debug image in QEMU (gdb-multiarch)` builds the open file, starts QEMU paused with the gdb stub on `127.0.0.1:1234`, attaches, loads `<name>.gdb` and stops at the first `(breakpoint)`. QEMU is stopped when the session ends.
* `Stop QEMU` stops whatever process is listening on port 1234. It runs `.vscode/qemu-debug.ps1 -Action stop`.

The debug start is `.vscode/qemu-debug.ps1 -Action start -Image <name>.img`. It checks that `<name>.elf` exists (the build writes it, and the script fails with "Run the build task first" if it is missing), stops any old QEMU on the port, starts QEMU, and waits until the port accepts connections. The debug session loads `<name>.elf` as the program, because cpptools (and gdb) reject a raw binary. The ELF is a single segment at `0x80000` holding the same bytes as the image. It has DWARF 4 line info, a function symbol table, `.debug_frame` call-frame info, and `.debug_info` with the `regs`/`subr` register renames as variables (`DW_OP_regN` locations, typed as the `reg64` union of `u64`/`i64`/`f64` or the `reg32` union of `u32`/`i32`/`f32`), all written by `dwarf.rkt`. The launch config sets gdb `output-radix 16`, so integers show in hex. Frames and `finish` work through helper calls. Register variables are reliable in the top frame only.

Source-line debugging: gutter breakpoints in `.rkt` files work because `.vscode/settings.json` sets `debug.allowBreakpointsEverywhere`. gdb resolves `file.rkt:line` through the line info. Code produced by a macro maps to the macro use site, such as the `(pi2d` line.

Debugging is at the source-line level for code in `.rkt` files, with function names in backtraces. A `subr` prologue can map to a macro template line in `lib/stack.rkt`. Renames from `regs` and `subr` show as variables in the top frame (scope is the enclosing `subr`). Use `x/8i $pc` and `info registers` for the instructions and the machine state. The C/C++ hover does not apply to `.rkt` files, since it only activates for `c` and `cpp` language IDs. The local `pisemble.register-hover` extension (`tools/register-hover/` in the pisemble-ancillary repo, installed by `install.ps1`) shows register values on hover in `.rkt` files during a `cppdbg` session. It handles raw names and same-file `regs` renames, and it does not flag outer frames. The hover also shows the bits, grouped by byte, and the bytes and ASCII in little-endian memory order. The Variables panel shows the same extra rows for `reg64` and `reg32` unions, through the gdb pretty-printer in `tools/gdb/reg-printers.py` (pisemble-ancillary repo), which `launch.json` loads in `setupCommands`.

Manual check, from the folder that holds the image (replace `bouncing` with the program's name):

```powershell
& "C:\msys64\mingw64\bin\gdb-multiarch.exe" -batch -nx -ex "set architecture aarch64" -ex "target remote 127.0.0.1:1234" -ex "source bouncing.gdb" -ex "continue" -ex "info registers pc" -ex "x/3i `$pc"
```


Note: a `brk` instruction in the guest goes to the platform's fault handler, not to gdb. Use `(breakpoint)` for gdb stops.

## UART protocol

`debug-str`, `debug-reg` and the timing code send typed records. The first byte selects the record type; numbers are little-endian.

| Type | Payload                                                                  | `comms.fsx` prints              |
|------|--------------------------------------------------------------------------|---------------------------------|
| 1    | Null-terminated string                                                   | The text                        |
| 2    | u32                                                                      | The value                       |
| 3    | u64                                                                      | The value                       |
| 4    | u64 address, u32 count, then `count` instruction words                   | Disassembly                     |
| 5    | Null-terminated label, then u64 bytes, u64 ticks, u64 counter frequency  | Time and bandwidth for the test |

Add new record types to both the sending code and the `match` in `comms.fsx`. `go-comms/pibear.go` (in the pisemble-ancillary repo) is an older listener for real hardware on `/dev/ttyUSB0` at 115200 baud.

## What QEMU emulates

* `-kernel` loads the image at `$80000` and starts all four cores there at EL2. Code must park cores 1 to 3 (check `mpidr_el1`).
* The mailbox framebuffer works. The address returned is a bus address; mask with `$3FFFFFFF` to get the ARM address (about `$3C100000`).
* Pixel order tag `$48006` with value 1 gives RGB order: red is the low byte of each 32 bit pixel.
* Virtual height larger than physical height plus the virtual offset tag `$48009` gives double buffering and hardware scrolling.
* 8 bit palette mode works. Set the palette with tag `$4800B` (offset, count, then RGB words).
* Other mailbox tags handled: blank screen, physical and virtual size, depth, alpha mode, pitch, overscan, clocks, temperature, memory split, board info, DMA channel mask. Not handled: GPU memory allocation, executing GPU code, the hardware cursor.
* The generic timer works: `cntpct_el0` and `cntfrq_el0`. Read the frequency from `cntfrq_el0` rather than hard-coding it. The ARM local timer at `$4000001C` and the SP804 timer are not modelled.
* DMA works but completes the whole control block chain instantly when CS is written. 2D mode is supported; 128 bit destination width is not.
* There is no cache model and no realistic timing. Memory speed or cycle measurements under QEMU say nothing about real hardware.
* Not implemented: V3D (the 3D GPU), the VPU, SMI, the transposer, I2S. There is no GPU acceleration of any kind.

## Real hardware differences to keep in mind

* `emu_clean.rkt` (pisemble-wolf3d repo) has two build settings at the top. Under QEMU use `EL 2` and `SOFT-FLIP 1`: the flip happens straight after drawing and no vsync interrupt is enabled. On the old hardware code use `EL 3` and `SOFT-FLIP 0`, which enables the SMI vsync interrupt. The `CurrentEL` check at boot stops the program if the EL is wrong. `lib/platform.rkt` hard-codes EL2 and has no switch.
* Caches matter. Anything the GPU reads (mailbox messages, the framebuffer if mapped cacheable) must be cleaned with `dc civac` or mapped non-cacheable. The platform code already does this, but QEMU will not catch mistakes.
* `config-pi3` has `kernel_old=1` and `arm_64bit=1`, so firmware loads the image at address 0, not `$80000`. Code that depends on absolute addresses behaves differently.
* Timing, DMA speed and vsync behaviour will differ.
