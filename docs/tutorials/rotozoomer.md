---
title: Writing a Rotozoomer on the Raspberry Pi 3 with Pisemble
description: Learn the math behind a rotozoomer, how screen and texture space relate, how to pin the zoom point, and how to write the loop in fixed point or with the FPU. Includes a chessboard texture and an image loaded at build time.
ms.date: 2026-10-17
ms.topic: tutorial
---

## What you will learn

* What a rotozoomer draws and why it maps screen pixels back to texture coordinates.
* How screen space and texture space relate, and how texture coordinates wrap.
* How to choose a pivot point and keep it fixed while the image rotates and zooms.
* How to step through the texture with 16.16 fixed point arithmetic and a sine table.
* How the same loop looks with the single precision FPU, and why it needs a bias before the float to integer conversion.
* How to build a chessboard texture and how to turn an image file into texel words when the program is assembled.

You will write the code yourself. This tutorial gives you the math, a register plan, and short fragments. It does not give you a complete program.

## Before you start

Read these project notes first:

* [language.md](../../agent-context/language.md) for the Pisemble syntax used in the snippets.
* [demo-template.md](../../agent-context/demo-template.md) for the demo contract, the `Draw-frame` subroutine, and the constants in `platform.rkt`.
* [qemu-and-debugging.md](../../agent-context/qemu-and-debugging.md) for how the project builds and runs under QEMU.
* [platform.rkt](../../lib/platform.rkt) for `width`, `height`, and `rgb`.
* [floor-cube.rkt](../../demo/floor-cube.rkt) for an example of single precision floating point code.

The examples target the 320 by 200 screen in 32 bit colour mode, as set up by `platform.rkt`. Each pixel is one 32 bit word. The screen row length is `ROW-BYTES`, which is 1280 bytes.

## What a rotozoomer draws

A rotozoomer shows one image that is rotated and scaled, usually animated so it spins and pulses over time. The name comes from ROTate and zOOM. The image repeats (tiles) across the whole screen, so the picture never ends at an edge.

The effect needs three things:

* A texture, which is a square array of colours. A chessboard is the easiest one to debug because every square is easy to see.
* A transform that says, for each screen pixel, which point of the texture it shows.
* A loop that runs that transform over every pixel, every frame.

The transform is where the math lives. The rest of the program is a loop and a few tables.

## Screen space

Screen space uses integer pixel coordinates `(x, y)`:

* `x` runs from 0 to 319, left to right.
* `y` runs from 0 to 199, top to bottom.
* The origin `(0, 0)` is the top left pixel.

The y axis points down. This matters when you read the rotation formulas later, because a positive angle rotates the image in the opposite direction from the one you might expect on paper.

## Texture space

Texture space uses texture coordinates `(u, v)`. For a texture with `N` texels on each side, the texel at `(u, v)` is the one at column `floor(u)` and row `floor(v)`. Texels are stored row by row, so the word index is:

```text
index = (floor(v) mod N) * N + (floor(u) mod N)
```

Two details make the effect work:

* `N` is a power of two. In this tutorial `N` is 64. For a power of two, `mod N` is the same as `and (N - 1)`, which is one instruction. The mask also works on negative numbers stored in two's complement, so a coordinate of `-1` wraps to `63`.
* Coordinates outside `[0, N)` are allowed. The mask wraps them back into the texture, which is why the image repeats across the screen.

The coordinates are real numbers, not integers. The fractional part is what makes rotation and zoom smooth, because the step from one pixel to the next can be less than one texel.

## Forward and inverse mapping

There are two ways to connect the two spaces.

Forward mapping walks the texture and places each texel on the screen. Rotating and scaling the texels leaves gaps between them and places some texels on top of each other. Filling the gaps is awkward.

Inverse mapping walks the screen and asks which texel each pixel shows. Every pixel receives exactly one texel, so there are no gaps. This is the method a rotozoomer uses. The transform therefore goes from screen space to texture space:

```text
screen (x, y)  ->  texture (u, v)  ->  texel colour
```

The inverse of a rotation and scale is a rotation and scale by the opposite amounts. You never need to compute that inverse explicitly, because you only need to know where each screen pixel lands in the texture.

## The transform

Let `theta` be the rotation angle and `z` be the scale. `z` is the number of texels that one screen pixel covers. Small `z` makes the texture large on screen (a zoom in), and large `z` makes it small (a zoom out).

For a screen point, take its offset `(dx, dy)` from a pivot point `(Sx, Sy)`:

```text
dx = x - Sx
dy = y - Sy

u = Tc + z * ( cos(theta) * dx + sin(theta) * dy )
v = Tc + z * ( -sin(theta) * dx + cos(theta) * dy )
```

`Tc` is the texture centre, which is `N / 2`. For a 64 texel texture, `Tc` is 32.

Read the formula as two steps. The first part, `(cos * dx + sin * dy, -sin * dx + cos * dy)`, rotates the offset by the angle. Multiplying by `z` scales it. Adding `Tc` moves the result into texture space.

> [!NOTE]
> The sign of `sin` in the `v` row depends on the direction you want the image to turn. Swap the sign if the image turns the wrong way for your choice of axes.

## Pinning the zoom point

The formula uses the offset from the pivot, not the raw screen coordinates. This is what pins the zoom point.

Consider the texture centre. When `x = Sx` and `y = Sy`, both `dx` and `dy` are zero, so `u = v = Tc`. The texture centre always appears on the pivot, whatever `z` and `theta` are. The image grows and turns around that spot, and the spot does not move.

If you used `x` and `y` directly instead of `dx` and `dy`, the rotation and scale would happen around the top left corner of the screen. The image would slide across the screen as it zoomed. The offset is the piece that moves the centre of rotation to the pivot.

Pick the pivot to suit what you are showing. The middle of the screen, `(160, 100)`, is a good start. Any other point works, and you can move it over time to make the zoom wander.

## Starting values and steps

Rather than multiply for every pixel, the loop adds a step. Differentiating the transform with respect to `x` and `y` gives four constant steps:

| Step   | Meaning                                       | Value          |
| ------ | --------------------------------------------- | -------------- |
| `dudx` | change in `u` for one pixel to the right     | `z * cos`      |
| `dvdx` | change in `v` for one pixel to the right     | `-z * sin`     |
| `dudy` | change in `u` for one pixel down             | `z * sin`      |
| `dvdy` | change in `v` for one pixel down             | `z * cos`      |

These four values change once per frame, because they depend on the angle and the scale. Within a frame they are constant, so the inner loop only needs additions.

The loop also needs the texture coordinate of the top left pixel, `(0, 0)`. Substitute `dx = -Sx` and `dy = -Sy`:

```text
p = cos * Sx + sin * Sy
q = sin * Sx - cos * Sy

u0 = Tc - z * p
v0 = Tc + z * q
```

The loop works like this. For each row, start with `(urow, vrow)`, which is the coordinate of the first pixel in that row. For each pixel in the row, read the texel at `(u, v)`, then add `dudx` and `dvdx`. At the end of the row, add `dudy` and `dvdy` to `urow` and `vrow` to move down one row.

## Two number formats

The transform needs fractions, so the loop needs a number format that can hold them. This tutorial shows two.

16.16 fixed point stores a real value `x` as the integer `x * 65536`. The low 16 bits are the fraction and the high 16 bits are the whole part. Adding two fixed point values is a normal integer addition. Multiplying two of them gives a product scaled by `65536 * 65536`, so you shift the product right by 16 to get back to 16.16. Integer code needs no FPU, and the wrap is a mask on the shifted value.

Single precision floating point stores each value as a 32 bit float. The `s` registers hold them, and `fmul`, `fadd` and `fsub` do the arithmetic. The float code reads more like the formula, but the float to integer conversion needs care, as explained below.

| Topic                 | 16.16 fixed point                         | Single precision FPU                              |
| --------------------- | ----------------------------------------- | ------------------------------------------------- |
| Multiply              | `mul`, then `asr @16`                     | `fmul`                                            |
| Texel index           | `asr @16`, then `and @63`                 | `fcvtzs`, after adding a bias, then `and @63`     |
| Negative coordinates  | Handled by two's complement and the mask  | Need a bias, because `fcvtzs` truncates toward zero |
| Precision             | Fixed step, 1/65536 of a texel            | 24 bit significand, grows with the magnitude      |
| Register file         | General purpose `x` registers             | `s` registers, a separate file                    |

The next two sections build the integer version in full, and then the float version.

## Building the sine table

Rotation needs `sin` and `cos`. Computing them in the frame loop is slow, so the program stores a table of 1024 values, one for each step around the circle. The angle is an index from 0 to 1023. The table is finer than a 256 entry one, so each frame's rotation step is about 0.35 degrees, which looks smooth.

The table holds `65536 * sin(angle)` as a 32 bit signed integer. The Racket code that builds it is short:

```racket
(define sine-words
  (for/list ([i 1024])
    (bitwise-and (exact-round (* 65536 (sin (* 2 pi (/ i 1024))))) #xFFFFFFFF)))
```

`bitwise-and` with `#xFFFFFFFF` gives the two's complement bit pattern for negative values. `write-value-32` masks each byte, so negative numbers are stored correctly.

Emit the table in the data section of the demo:

```text
/= 4
:sine-table
(for ([s sine-words]) (write-value-32 s))
```

A quarter turn is 256 steps, so cosine is the sine value 256 entries further along. That gives `cos(theta)` as `sine[(angle + 256) & 1023]`.

## Setting up each frame in fixed point

Each frame, the program needs `cos`, `sin`, the scale `z`, the four steps, and the start values. Here is the setup as a fragment. It assumes the `Draw-frame` subroutine has these aliases:

```text
(subr Draw-frame
      ([vptr x0] [frame x1] [sines x2] [tex x3] [ang x4] [cs x5] [sn x6]
       [z x7] [dudx x8] [dvdx x9] [dudy x10] [dvdy x11] [urow x12]
       [vrow x13] [u x14] [v x15] [tmp x16] [tmp2 x17] [pp x21] [qq x22]
       [ny x19] [nx x20]) [] {
```

The setup uses these steps:

```text
adr sines sine-table:
adr tex texture:
lsl ang frame @1         ; angle = (2 * frame) & 1023, so one turn takes 512 frames
and ang ang @1023

; cos = sine[(ang + 256) & 1023], sin = sine[ang]
add tmp ang @256
and tmp tmp @1023
lsl tmp tmp @2
add tmp sines tmp
ldrsw cs [tmp @0]
lsl tmp ang @2
add tmp sines tmp
ldrsw sn [tmp @0]
```

`ldrsw` sign extends the 32 bit table entry into a 64 bit register. Use it for every signed table value. `ldr w` zero extends, so a negative value loaded that way becomes a large positive number.

Next, the scale. Here `z` goes from 0.2 to 1.4 texels per pixel, which zooms in and out. Below 1 the texture is magnified, and above 1 it shrinks:

```text
; z = 0.8 + 0.6 * sine[(4 * frame) & 1023]   (texels per pixel, 16.16)
lsl tmp frame @2         ; one zoom cycle takes 256 frames
and tmp tmp @1023
lsl tmp tmp @2
add tmp sines tmp
ldrsw z [tmp @0]
mov tmp @39322           ; 0.6 in 16.16
mul z z tmp
asr z z @16
mov tmp @52429           ; 0.8 in 16.16
add z z tmp
```

The `mov` constants are the 16.16 forms of 0.6 and 0.8, which are 39322 and 52429. `mov` takes a value up to 16 bits, so both fit. Larger constants need `load-immediate`, which the language notes describe.

Now the four steps. Each product is 16.16 times 16.16, so the result is shifted right by 16. Use 64 bit registers so the intermediate product does not overflow. At the largest values, the product is about 2^40, which does not fit in 32 bits.

```text
; per-pixel steps (16.16)
mul dudx cs z
asr dudx dudx @16
mul dvdx sn z
asr dvdx dvdx @16
mov tmp @0
sub dvdx tmp dvdx
mul dudy sn z
asr dudy dudy @16
mul dvdy cs z
asr dvdy dvdy @16
```

`mov x @-1` is not allowed, so `dvdx` is negated by subtracting it from zero.

Finally, the start values. This pins the texture centre to the pivot, as described above. The integer example keeps the pivot fixed at `(160, 100)`. The float version in the demo moves it, as shown below:

```text
; pin texture centre (32,32) to screen centre (160,100)
; p = cos*Sx + sin*Sy,  q = sin*Sx - cos*Sy
mov tmp @160
mul pp cs tmp
mul qq sn tmp
mov tmp2 @100
mul tmp sn tmp2
add pp pp tmp
mul tmp cs tmp2
sub qq qq tmp
mul pp pp z
asr pp pp @16
mul qq qq z
asr qq qq @16
mov tmp @32
lsl tmp tmp @16
sub urow tmp pp
add vrow tmp qq
```

Check this against the formula. `urow` is `Tc - z * p` and `vrow` is `Tc + z * q`. The `lsl tmp tmp @16` puts `Tc` into 16.16 form.

## The inner loop in fixed point

The inner loop runs once per pixel, so it needs to be short. This is the part of the program that the rest exists to feed.

```text
mov ny @height
:next-row
mov u urow
mov v vrow
mov nx @width
:next-pixel
asr tmp v @16
and tmp tmp @63
lsl tmp tmp @6
asr tmp2 u @16
and tmp2 tmp2 @63
add tmp tmp tmp2
lsl tmp tmp @2
add tmp tex tmp
ldr w17 [x16 @0]
str w17 [vptr] @4
add u u dudx
add v v dvdx
sub nx nx @1
cbnz nx next-pixel-
add urow urow dudy
add vrow vrow dvdy
sub ny ny @1
cbnz ny next-row-
```

Read the pixel body in order:

* `asr tmp v @16` takes the whole part of `v`. `and tmp tmp @63` wraps it into the texture, and `lsl tmp tmp @6` multiplies the row by 64 texels per row.
* The same happens for `u`, and the two are added to make the texel index.
* `lsl tmp tmp @2` converts the index to a byte offset, because each texel is 4 bytes.
* `ldr w17 [x16 @0]` reads the texel and `str w17 [vptr] @4` writes it to the screen. The `@4` form adds 4 to `vptr` after the store, so the pointer moves to the next pixel.
* `add u u dudx` and `add v v dvdx` step to the next pixel.

Each row starts again at `urow` and `vrow`. The row pointer `vptr` moves on because the store post-increments it. Count pixels and rows with `nx` and `ny`.

> [!TIP]
> If the picture looks shifted or mirrored, check the pivot and the sign of `dvdx` before you change the loop. Most errors come from the setup, not from the inner loop.

## The same loop with the FPU

The FPU version has the same structure. The differences are that the values are floats, the conversion to an integer needs a bias, and the constants come from a table of floats rather than from `mov`.

### Float constants and tables

Build the float bit patterns in Racket with a helper that writes a 32 bit IEEE value in little endian order:

```racket
(define (f32 x)
  (integer-bytes->integer (real->floating-point-bytes x 4 #f) #f #f))

(define sine-bits
  (for/list ([i 1024]) (f32 (sin (* 2 pi (/ i 1024))))))

(define const-bits
  (list (f32 0.8) (f32 0.6) (f32 160.0) (f32 100.0) (f32 32.0) (f32 640.0)
        (f32 90.0) (f32 60.0)))
```

The constants are, in order: the zoom base `0.8` and amplitude `0.6`, the pivot centre `x` and `y`, the texture centre `Tc`, the bias, and the two orbit radii `90` and `60`. The pivot orbits its centre, so the image also drifts across the screen. Emit them with the same data pattern as the integer tables:

```text
/= 4
:consts
(for ([c const-bits]) (write-value-32 c))
/= 4
:sine-table
(for ([s sine-bits]) (write-value-32 s))
```

Load a constant into an `s` register with `ldr s3 [consts @4]`, which reads the word at offset 4. The `consts` alias holds the address of the table, set by `adr consts consts:`.

### Per-frame setup with floats

The setup follows the same ideas as before, with `fmul` and `fadd` in place of the shifts. The table lookups use the same index arithmetic as the integer setup, but read words with `ldr s` instead of `ldrsw`, because the entries are floats:

```text
; tmp = address of sine[(ang + 256) & 1023]
ldr s0 [tmp @0]          ; cos
; tmp = address of sine[ang]
ldr s1 [tmp @0]          ; sin
; tmp = address of sine[(4 * frame) & 1023]
ldr s2 [tmp @0]
ldr s3 [consts @4]       ; 0.6
fmul s2 s2 s3            ; 0.6 * sine
ldr s3 [consts @0]       ; 0.8
fadd s2 s2 s3            ; z = 0.8 + 0.6 * sine
fmul s10 s2 s0           ; dudx = z * cos
fmul s11 s2 s1           ; dvdx = z * sin
fneg s11 s11             ; dvdx = -z * sin
fmul s14 s2 s1           ; dudy = z * sin
fmul s15 s2 s0           ; dvdy = z * cos
```

The pinning uses the same `p` and `q` as before, with floats. The pivot is the screen centre plus an orbit offset, `(AX * sin, AY * cos)`, which reuses the rotation's `sin` and `cos`:

```text
ldr s16 [consts @24]     ; AX = 90
fmul s16 s16 s1          ; AX * sin
ldr s17 [consts @28]     ; AY = 60
fmul s17 s17 s0          ; AY * cos
ldr s18 [consts @8]      ; pivot x
fadd s4 s18 s16          ; Sx = pivot x + AX * sin
ldr s19 [consts @12]     ; pivot y
fadd s5 s19 s17          ; Sy = pivot y + AY * cos
fmul s21 s0 s4           ; p = cos * Sx + sin * Sy
fmul s7 s1 s5
fadd s21 s21 s7
fmul s22 s1 s4           ; q = sin * Sx - cos * Sy
fmul s7 s0 s5
fsub s22 s22 s7
fmul s21 s21 s2          ; scale by z
fmul s22 s22 s2
ldr s6 [consts @16]      ; Tc
fsub s12 s6 s21          ; urow = Tc - z * p
fadd s13 s6 s22          ; vrow = Tc + z * q
```

### The float conversion needs a bias

`fcvtzs` rounds toward zero. For positive numbers that is the same as taking the floor, which is what a texel index needs. For negative numbers it is not. `-3.25` becomes `-3`, but the texel that contains `-3.25` is texel `-4`.

The fix is to add a bias before the conversion. The bias must be a positive number larger than the most negative coordinate the screen can reach, so every value becomes positive and truncation matches the floor. Here the pivot can sit up to 250 pixels right of the screen's left edge and 160 pixels below its top. Since `u - Tc` is at most `z * (|x - Px| + |y - Py|)`, with `z` up to 1.4, the largest offset is about `1.4 * (250 + 160) = 574`. The most negative `u` is then about `32 - 574 = -542`, so a bias of 640 is safe.

The bias also must be a multiple of the texture size, so the mask still gives the right texel. 640 is 10 times 64.

> [!WARNING]
> Pick the bias with care. A bias of 65536 also looks safe, but a 32 bit float has a 24 bit significand. At 65536 the step between two floats is 2^-7 of a texel, which makes the sampling visibly coarse. At 640 the values stay below 2^11, so the step is about 2^-12 of a texel, which is fine.

### The float inner loop

```text
:next-pixel
fadd s7 s8 s20           ; u + bias
fcvtzs tmp2 s7
and tmp2 tmp2 @63
fadd s7 s9 s20           ; v + bias
fcvtzs tmp s7
and tmp tmp @63
lsl tmp tmp @6
add tmp tmp tmp2
lsl tmp tmp @2
add tmp tex tmp
ldr w17 [x16 @0]
str w17 [vptr] @4
fadd s8 s8 s10           ; u += dudx
fadd s9 s9 s11           ; v += dvdx
sub nx nx @1
cbnz nx next-pixel-
```

The texel lookup is the same as in fixed point, except that the conversion replaces the shift. The float code also needs `fmov s8 s12` and `fmov s9 s13` at the start of each row, to copy the row start into the working values.

The float version runs in QEMU, and the project's `floor-cube.rkt` demo already uses the FPU. Check the notes in [language.md](../../agent-context/language.md) about which exception levels allow FP before you use it in other code.

> [!NOTE]
> This tutorial does not compare the two versions on hardware. Measure both on your own Raspberry Pi 3 if the speed matters to you.

## Chessboard texture

A chessboard shows the rotation and zoom clearly, because every edge is a straight line in texture space. It also shows the wrap, because the pattern should continue across the edge of the texture.

Build the board in Racket with a function that picks a colour from the texel position:

```racket
(define tex-size 64)

(define (chess x y)
  (if (even? (+ (quotient x 8) (quotient y 8)))
      (rgb 240 240 240)
      (rgb 20 20 80)))

(define texels
  (for*/list ([y tex-size] [x tex-size]) (chess x y)))
```

Each square is 8 texels, so the 64 texel texture holds an 8 by 8 board. Emit the words with the same pattern as before:

```text
/= 4
:texture
(for ([c texels]) (write-value-32 c))
```

A texel word is 32 bits. `rgb` puts red in the low byte, as described in `platform.rkt`, and the top byte is 255, which is the alpha value the image loader below also uses.

> [!TIP]
> The wrap only looks right if the board has an even number of squares along each side. With 8 squares, the square in column 7 and the square in column 8 (column 0 of the next copy) have different parity, so the colours alternate across the edge.

The texture has `64 * 64 * 4 = 16384` bytes of data. A 256 by 256 texture takes 262144 bytes, which is much larger but still fits in memory.

## Loading an image as a texture

A chessboard is generated, but an image comes from a file. The approach is to convert the image to texel words when you run the Racket code, so the assembled program already contains the pixels. The program does no file I/O at run time.

### Keep Racket graphics out of the Pisemble file

Put the `racket/draw` code in a plain `#lang racket/base` module, and require it from the Pisemble file. The Pisemble reader does not handle some `racket/draw` forms:

* `(new bitmap-dc% [bitmap dst])` fails with a `string-replace` contract error. The reader mangles the keyword bracket syntax.
* `(make-object bitmap-dc% dst)` fails with `bitmap-dc: unbound identifier`. The reader strips the `%` from the class name.

The helper module avoids both problems. Save it as `texture.rkt` in the same folder as your demo:

```racket
#lang racket/base
;; Loads an image and returns a list of rgb texel words, row by row.
(require racket/draw racket/class)
(provide load-texture)

(define (load-texture path tex-size)
  (define src (read-bitmap path))
  (define w (send src get-width))
  (define h (send src get-height))
  (define side (max w h))
  (define dst (make-bitmap tex-size tex-size #t))
  (define dc (new bitmap-dc% [bitmap dst]))
  (send dc set-smoothing 'smoothed)
  (send dc set-scale (/ tex-size side) (/ tex-size side))
  (send dc draw-bitmap src (/ (- side w) 2) (/ (- side h) 2))
  (define argb (make-bytes (* 4 tex-size tex-size) 0))
  (send dst get-argb-pixels 0 0 tex-size tex-size argb)
  (for/list ([i (* tex-size tex-size)])
    (define a (bytes-ref argb (* 4 i)))
    (define (channel k) (quotient (* (bytes-ref argb (+ (* 4 i) k)) a) 255))
    (+ #xFF000000 (* 65536 (channel 3)) (* 256 (channel 2)) (channel 1))))
```

Each step in the helper has a reason:

* The image is drawn into a square of side `max(w, h)`, so its aspect ratio is kept. A tall image gets empty space on the sides, drawn as transparent.
* The square is scaled to the texture size with `set-scale`, and the image is centred.
* `make-bitmap` with `#t` keeps an alpha channel. `get-argb-pixels` returns four bytes per pixel in the order alpha, red, green, blue. Transparent pixels are composited over black by multiplying each colour by `alpha / 255`.
* The texel word is `#xFF000000 + blue * 65536 + green * 256 + red`, which matches the layout `rgb` uses.

`racket/draw` is part of the full Racket distribution. If `require` fails with a missing module, install the `draw-lib` package.

### Use the texture in the demo

The Pisemble file requires the helper and points at the image with `define-runtime-path`. That makes the path relative to the source file, so it does not depend on the directory where you run `racket`:

```racket
#lang pisemble
(require (file "C:/path/to/Pisemble/lib/platform.rkt")
         "texture.rkt"
         racket/runtime-path)

(define-runtime-path logo-path "logo.png")

(define tex-size 64)

(define texels (load-texture logo-path tex-size))
```

Replace the `platform.rkt` path with your own. Copy `logo.png` next to the demo. The repository has a test image at [pink-squirrel.png](../../demo/pink-squirrel.png), a 64 by 64 pixel-art squirrel based on the Pink Squirrel Labs favicon, so you can copy that and rename it to `logo.png`.

Emit `texels` in the data section exactly as you emitted the chessboard. The image is now a table of words, and the inner loop does not know or care where they came from.

> [!NOTE]
> The loader turns the image into a `64` by `64` texture, so a 190 by 240 image loses detail. To use 256 texels, set `tex-size` to 256 and make these changes:
>
> * Change `and @63` to `and @255` in the inner loop, and `lsl @6` to `lsl @8`, because each row is now 256 texels.
> * Change the texture centre `Tc` from 32 to 128 in the setup.
> * Change the float bias to 512. It is a multiple of 256, and it is still larger than the most negative coordinate, which is about `128 - 574 = -446` here.

## Common mistakes

* Using `ldr w` for a signed table entry. The result is zero extended, so a negative sine becomes a large positive number. Use `ldrsw`.
* Multiplying two 16.16 values in 32 bit registers. The product overflows. Use 64 bit `x` registers for the products and shift right by 16 afterwards.
* Forgetting the pivot offset. Without `dx = x - Sx`, the image rotates and zooms about the top left corner of the screen.
* Forgetting the bias in the float version. Negative coordinates are truncated toward zero, so the texels shift by one for those coordinates.
* Using `mov` with a value above 16 bits, or a negative value. Use `load-immediate` for large constants, or subtract from zero.
* Judging the speed of the effect in QEMU. QEMU timing does not match a real board. Compare frames for correctness, not for frame rate.

## Build and run

Build the demo by running the Racket file. The build writes a `.img` file beside the source. Run the image with the QEMU command in [qemu-and-debugging.md](../../agent-context/qemu-and-debugging.md).

The finished version of this tutorial's image rotozoomer is in [rotozoomer.rkt](../../demo/rotozoomer.rkt), with the image loader in [texture.rkt](../../demo/texture.rkt). Build it from the `demo` folder with `racket rotozoomer.rkt`.

To check the output without a window, connect to the QEMU monitor and send `screendump frame.ppm`. Take two dumps a second apart and compare them. If the hashes are the same, the frames are not changing and the angle or scale is stuck.

## Exercises

* Move the pivot away from the centre, for example to `(100, 60)`. Confirm that the point stays fixed while the image spins.
* Change the scale so `z` ranges from 0.1 to 2.0. Check where the texture repeats most often.
* Replace the chessboard with the image texture and verify that the logo tiles across the screen.
* Build the 256 texel version and compare the detail with the 64 texel version.
* Change the orbit radii `90` and `60` to make the pivot wander further, or change the zoom base and amplitude to zoom more or less. The demo already moves the pivot in a circle.

Bilinear filtering, which blends the four nearest texels instead of picking one, is the next step after these exercises. It uses the fractional part of `u` and `v`, which the loop above drops.
