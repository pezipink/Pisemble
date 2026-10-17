# Pisemble

An AArch64 assembler and linker written in Racket, for bare-metal programs such
as the Raspberry Pi 3 (run under QEMU with `-M raspi3b`).

Pisemble is a work in progress. It supports a growing subset of AArch64, and
instructions are added as they are needed. Expect to work around missing
instructions with raw opcodes (see the tutorials).

## Installing

```
raco pkg install pisemble
```

## Languages

Pisemble provides two module languages:

* `#lang pisemble` is the full language: all of `racket`, plus the assembler.
  Use it for programs.
* `#lang pisemble/base` is the minimal language: `racket/base`, plus the
  assembler. Use it as the base for your own languages (see below).

Pisemble does not re-export its helper libraries from `#lang pisemble`. Require
the ones you need explicitly:

```racket
#lang pisemble
(require pisemble/lib/periph pisemble/lib/stack pisemble/lib/struct pisemble/lib/expr)
```

| Library                  | Purpose                                                        |
|--------------------------|----------------------------------------------------------------|
| `pisemble/lib/stack`     | `PUSH`/`POP`, `regs` (register renaming), `subr`, `preserve`   |
| `pisemble/lib/struct`    | `load-immediate`, `/struct`                                    |
| `pisemble/lib/expr`      | Structured control flow: `/if`, `/when`, `/cond`, `/while` ... |
| `pisemble/lib/periph`    | Peripheral addresses, `init-uart`, `debug-str`                 |
| `pisemble/lib/platform` | Boot, framebuffer, MMU and frame loop for the example programs |
| `pisemble/lib/pi2d`      | `pi2d` form: size and depth, and the `#:init`, `#:update`, `#:render` hooks |

## A first program

```racket
#lang pisemble
(aarch64 "kernel8.img" []{
 :start
  nop
  ret
})
```

Run the file with `racket hello.rkt`. The image is written to the current
directory, so `cd` to the source folder first.

## Building a language on pisemble/base

Any module language can use `pisemble/base` as its base. Create a module that
re-exports `pisemble/base/main` (plus whatever else you need), then a reader in
`lang/reader.rkt` that uses it:

```racket
;; mylang/main.rkt
#lang racket/base
(require pisemble/base/main)
(provide (all-from-out pisemble/base/main))
```

```racket
;; mylang/lang/reader.rkt
#lang s-exp syntax/module-reader
mylang/main
#:wrapper1 wrapper1
(require pisemble/reader)
```

Programs then start with `#lang mylang`. Because `pisemble/base` is
`racket/base`, your language gets only the base libraries unless you add more.

## Examples and tests

* [examples/](examples/) contains example programs built with `pi2d`
  ([lib/pi2d.rkt](lib/pi2d.rkt)), which sits on the platform in
  [lib/platform.rkt](lib/platform.rkt). Run one with `racket examples/plasma.rkt`, then open the image in QEMU.
* [test/](test/) contains the rackunit suite. `raco test test` runs it.

## Documentation

* [docs/tutorials/](docs/tutorials/) covers instructions, the MMU, DMA,
  multicore, interrupts and debugging.
* [scribblings/](scribblings/) contains the reference manual.
* [agent-context/](agent-context/) contains detailed notes on the language,
  the linker, and QEMU and debugging.

## Related repositories

* `pisemble-wolf3d` is a Wolfenstein 3D style raycaster built with Pisemble.
  Its game data is not included.

## License

MIT. See [LICENSE](LICENSE).
