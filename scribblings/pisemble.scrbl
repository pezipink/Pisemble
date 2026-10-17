#lang scribble/manual

@(require (for-label racket/base))

@title{Pisemble: an AArch64 Assembler and Linker}

@italic{Written by @hyperlink["http://www.pinksquirrellabs.com"]{Ross McKinlay}}

Pisemble is an AArch64 assembler and linker written in Racket. It is used to
write bare-metal programs, for example for the Raspberry Pi 3 under QEMU. It
supports a subset of AArch64 and adds instructions as they are needed.

@section{Languages}

@litchar{pisemble} is a language whose module body is Racket. Use it when
you want Racket's full library available alongside assembly.

@litchar{pisemble/base} is the same assembler built on @racket[racket/base],
which loads faster and suits languages built on top of Pisemble.

@section{Helper libraries}

The languages do not re-export the helper libraries, so a program requires the
ones it uses explicitly:

@itemlist[
 @item{@litchar{pisemble/lib/stack}: @racket[PUSH], @racket[POP],
   register renaming with @racket[regs], and @racket[subr] and @racket[preserve].}
 @item{@litchar{pisemble/lib/struct}: @racket[load-immediate] and @racket[/struct].}
 @item{@litchar{pisemble/lib/expr}: structured control flow such as
   @racket[/if], @racket[/when], @racket[/cond], @racket[/whilenz] and @racket[/while].
   Loops test before the body. The @racket[/do-whilenz], @racket[/do-while] and
   similar forms test after it, so the body always runs at least once.}
 @item{@litchar{pisemble/lib/periph}: UART helpers, including
   @racket[init-uart], @racket[debug-str] and @racket[uart-object].}
]

@section{Building a language on Pisemble}

A new language reuses @litchar{pisemble/base} and the Pisemble reader. Use
the Pisemble reader rather than a plain @racket[s-exp] reader, because the plain
one does not understand block syntax or labels:

@codeblock|{
#lang s-exp syntax/module-reader
pisemble/base/main
#:wrapper1 wrapper1
#:language-info #(pisemble/lang/language-info get-language-info #f)

(require pisemble/reader)
}|

See the @hyperlink["https://github.com/pezipink/Pisemble"]{README} for a
complete example.

@section{Running a program}

Build a program by running its source file. Output files (@filepath{.obj} and
@filepath{.img}) are written to the current directory at run time, so change
into the source folder first:

@commandline{racket my-program.rkt}

An image (@filepath{.img}) is always rebuilt. An object (@filepath{.obj}) is skipped
when it is up to date, meaning it is not older than the source module, the modules
it requires, or the objects it links. A skipped object keeps its debug records in a
@filepath{.obj.dbg} file written beside it.

@section{Tests}

@commandline{raco test test}

