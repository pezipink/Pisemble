#lang pisemble
(require (for-syntax syntax/parse racket/stxparam racket/syntax))
(require syntax/parse/define)

(require racket/stxparam)
(require racket/file)
(require racket/string)
(require racket/match)
(require racket/math)
(require threading)
(require "../demo/platform.rkt")
(require racket/runtime-path)

(define-runtime-path palette-file "wolfpal.inc")

(define-syntax-parser inc [(_ reg:register) #'{ add reg reg @1 }])
(define-syntax-parser dec [(_ reg:register) #'{ sub reg reg @1 }])

(set-bpp! 32)

(aarch64 "wolfmain.obj" [] {
     TILESHIFT = 16
     TILEGLOBAL = (arithmetic-shift 1 16)
     TILEGLOBAL/2 = (/ TILEGLOBAL 2)
     ANGLES = 360
     FINEANGLES = 3600
     ANGLEQUAD = (/ 360 4)
     VIEWHEIGHT = 160
     VIEWWIDTH = 320
     TEXTUREMASK = 4032
     TEXTUREFROMFIXEDSHIFT = 4
     MINDIST = $5800
     TEXTURESIZE = (arithmetic-shift 1 6)
     MOVESPEED = $800   ; fixed point per frame
     TURNSPEED = 3      ; degrees per frame
     ; host input byte: one bit per key (bits 4-7 unused)
     KEY-W = 1
     KEY-A = 2
     KEY-S = 4
     KEY-D = 8
     AUX-IO = $40
     AUX-LSR = $54

 


(subr Demo-init () [] {
  ; the linker writes image-relative offsets into the *-address cells,
  ; the image is loaded at 0x80000, so rebase each one before use
  (load-immediate x2 $80000)
  adr x1 textures-address:
  ldr x0 [x1]
  add x0 x0 x2
  str x0 [x1]
  adr x1 maps-address:
  ldr x0 [x1]
  add x0 x0 x2
  str x0 [x1]
  adr x1 2d-images-address:
  ldr x0 [x1]
  add x0 x0 x2
  str x0 [x1]
  adr x1 2d-image-header-address:
  ldr x0 [x1]
  add x0 x0 x2
  str x0 [x1]

  bl setup-game-level:
  bl scan-info-plane:

  ; costable = sintable + 360 (sintable entries are 4 bytes, so 360 is a quarter turn)
  adr x1 sintable:
  (load-immediate x0 ANGLES)
  add x0 x1 x0
  adr x1 costable:
  str x0 [x1]
})

; x0 = back page, x1 = frame number
(subr Draw-frame ([vptr x0]) [] {
  bl Read-input:
  bl Update-player:

  mov x1 vptr
  (preserve [x4 x8 x9 x10 x14] {
    bl ThreeDRefresh:
  })
})

(define-syntax-parser FixedMul
  ; these should be x registers only, cba to put a check in
  ; trashes x0
  ; DO NOT USE x0 AS TARGET!
  ; make sure you SIGN ENTEND into x and y first!
  ([_ target:register x:register y:register]
   #'{
      ; TODO: madd here when i implement it
      mul target x y
      (load-immediate x0 $8000)
      add target target x0
      asr target target @16 
     }))

(subr Read-input () [x0 x1 x2] {
  ; drain the mini uart rx fifo, keeping the newest byte
  ldr x1 aux-base:
:input-drain
  ldr w2 (x1 @AUX-LSR)
  mov w0 @1
  and w2 w2 w0            ; data ready?
  cbz w2 input-done+
  ldrb w0 [x1 @AUX-IO]
  adr x2 input-state:
  str w0 [x2]
  b input-drain-
:input-done
})

; forward is (cos, -sin), the same direction ThreeDRefresh looks along
(subr Update-player ([ply x1] [inp w2] [angle w3] [tmp w4] [speed x5]
                     [ptr x8] [off w9])
      [x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14] {
  adr ptr input-state:
  ldr inp [ptr]
  ldr ply player:
  (_objstruct/angle-get angle ply)

  ; A turns left, D turns right (angle grows to the left)
  mov tmp @KEY-A
  and tmp inp tmp
  (/when (tmp ne @0) {
    add angle angle @TURNSPEED
    (/when (angle ge @ANGLES) {sub angle angle @ANGLES})
  })
  mov tmp @KEY-D
  and tmp inp tmp
  (/when (tmp ne @0) {
    add angle angle @(- ANGLES TURNSPEED)
    (/when (angle ge @ANGLES) {sub angle angle @ANGLES})
  })

  ; W moves forward, S back (both held cancels out)
  mov speed @0
  mov tmp @KEY-W
  and tmp inp tmp
  (/when (tmp ne @0) {add speed speed @MOVESPEED})
  mov tmp @KEY-S
  and tmp inp tmp
  (/when (tmp ne @0) {sub speed speed @MOVESPEED})

  ; cos = costable[angle], sin = sintable[angle] (4 byte entries, 16.16 fixed)
  lsl off angle @2
  ldr ptr costable:
  add ptr ptr off
  ldr w6 [ptr]
  sxtw x6 w6
  adr ptr sintable:
  add ptr ptr off
  ldr w7 [ptr]
  sxtw x7 w7

  ; dx = speed * cos, dy = -(speed * sin)
  (FixedMul x10 x5 x6)
  (FixedMul x11 x5 x7)
  mov x12 @0
  sub x11 x12 x11

  ; player->x and player->y are 16.16 fixed, stored as 32 bit
  (_objstruct/x-get w13 ply)
  sxtw x13 w13
  add x13 x13 x10
  (_objstruct/x-set w13 ply)
  (_objstruct/y-get w14 ply)
  sxtw x14 w14
  add x14 x14 x11
  (_objstruct/y-set w14 ply)

  (_objstruct/angle-set angle ply)
})

(subr VgaClearScreen
      ([vptr x1]
       [temp x2]       
       [x x3]
       [y x4]
       [ptr x5]
       [colour w6])
      [x1] {

   adr ptr palette:
   mov temp @$1d    
   lsl temp temp @2
   add ptr ptr temp
   ldr colour [ptr]

   xsteps = (/ 320 4)
   ; ceiling hardcoded to palette $1d for now
   (/for { mov y @0 } (y lt @(/ VIEWHEIGHT 2)) (inc y) {
     (/for { mov x @0 } (x lt @xsteps) (inc x) {                                                   
         str colour [vptr]
         str colour [vptr @4]
         str colour [vptr @8]
         str colour [vptr @12]
         add vptr vptr @16
     })

   })

   ; floor is always $19
   adr ptr palette:
   mov temp @$19    
   lsl temp temp @2
   add ptr ptr temp
   ldr colour [ptr]

   (/for { } (y lt @VIEWHEIGHT) (inc y) {
     (/for { mov x @0 } (x lt @xsteps) (inc x) {
         str colour [vptr]

         str colour [vptr @4]
         str colour [vptr @8]
         str colour [vptr @12]
         add vptr vptr @16
     })
   })
})
(subr ScalePost
      ([vptr x1]

       [ywcount w2]
       [yoffs w3]
       [yw w4]
       [yd w5]
       [yendoffs w6]
       [col w7]
       [ptr x8]
       )
      [x1] {
  ; scales and draws a strip of pixels from a wall texture

;  (debug-str "enter scalepost " #t)

  ;ywcount = yd = wallheight[postx] >> 3;
  adr ptr wallheight:
  ldr w10 postx:
  lsl w10 w10 @2
  add ptr ptr w10
  ldr w10 [ptr]
;  (debug-str-reg "wallheight[postx] " w10)
  lsr w10 w10 @3
;  (debug-str-reg ">>3 " w10)
  mov ywcount w10
  mov yd w10
  (/when (yd le @0) { mov yd @100 })

  ;yoffs = (viewheight / 2 - ywcount) * vbufPitch;
  mov w10 @(/ VIEWHEIGHT 2)
  sub w10 w10 ywcount
  ldr w11 vbufPitch:
  mul yoffs w10 w11
;  (debug-str-reg "yoffs " yoffs)
  (/when (yoffs lt @0) { mov yoffs @0 })

  ; yoffs += postx
  ldr w10 postx:
  add yoffs yoffs w10
;  (debug-str-reg "yoffs " yoffs)

 ; yendoffs = viewheight / 2 + ywcount - 1;
  mov w10 @(/ VIEWHEIGHT 2)
  add w10 w10 ywcount
  sub yendoffs w10 @1
;  (debug-str-reg "yendoffs " yendoffs)
  
  ;yw=TEXTURESIZE-1;
  mov yw @(- TEXTURESIZE 1)
;  (debug-str-reg "yw " yw)
  
  ; this loops scales when the wall takes up more than the
  ; vertical size of the screen, don't need it for first
  ; render (TODO)
   ;; while(yendoffs >= viewheight)
   ;;  {
   ;;      ywcount -= TEXTURESIZE/2;
   ;;      while(ywcount <= 0)
   ;;      {
   ;;          ywcount += yd;
   ;;          yw--;
   ;;      }
   ;;      yendoffs--;
   ;;  }
   ;;  if(yw < 0) return;

  ;col = postsource[yw]
  ldr ptr postsource:
  lsl w10 yw @2
  add ptr ptr w10
  ldr col [ptr] ; this is the 32bit pixel data 
;  (debug-str-reg "first col is " col)
  
  ;yendoffs = yendoffs * vbufPitch + postx;
  ldr w10 vbufPitch:
  ldr w11 postx:
  mul yendoffs yendoffs w10
  add yendoffs yendoffs w11
;  (debug-str-reg "yendoffs before loop " yendoffs)

;  (debug-str "entering loop .." #t)
  (/while (yoffs le yendoffs) {
;    (debug-str-reg "loop yoffs " yoffs)
    
    mov x20 vptr
    lsl x10 yendoffs @2
    add x20 x20 x10
    str col [x20]  ; write pixel data to video memory

    ;ywcount -= TEXTURESIZE/2
    mov x10 @(/ TEXTURESIZE 2)
    sub ywcount ywcount x10
  ;  (debug-str-reg "iloop ywcount " ywcount)
    (/when (ywcount le @0) {
  :top-loop

      add ywcount ywcount yd
 ;     (debug-str-reg "ywcount " ywcount)
      sub yw yw @1
;      (debug-str-reg "yw " yw)
      (/when (ywcount le @0)
             {
  ;            (debug-str "jmp 1 " #t)
              b top-loop-
              })
      (/when (yw lt @0)
             {
 ;             (debug-str "jmp 2 " #t)
              b local-end+
             } )
      ;col = postsource[yw]
      ldr ptr postsource:
      lsl w10 yw @2
      add ptr ptr w10
      ldr col [ptr] ; this is the 32bit pixel data 
;      (debug-str-reg "col is " col)      
    })
    ldr w10 vbufPitch:
    sub yendoffs yendoffs w10
;    (debug-str-reg "yendoffs nojw " yendoffs)
  })
:local-end                               
;  (debug-str "exit scalepost " #t)            
})

(subr HitHorizWall
      ([vptr x1]
       [wallpic w2]
       [texture w3]
       [ptr x4]
       )
      [x1] {
;  (debug-str "in hithorizwall" #t)
  ;texture = ((xintercept+texdelta)>>TEXTUREFROMFIXEDSHIFT)&TEXTUREMASK;  
  ldr w10 xintercept:
  ldr w11 texdelta:
  add w10 w10 w11
  asr w10 w10 @TEXTUREFROMFIXEDSHIFT
  mov w12 @TEXTUREMASK
  and w10 w10 w12
  mov texture w10
;  (debug-str-reg "texture " w10)

  ;if ytilestep == -1
  ldr w10 ytilestep:
  mov w11 @0
  sub w11 w11 @1 ; -1
  (/if (w10 eq w11)
       {
         ;yintercept += TILEGLOBAL;
         ldr w10 yintercept:
         (load-immediate w11 TILEGLOBAL)
         add w10 w10 w11
         adr ptr yintercept:
         str w10 [ptr]
       }
       ;else 
       {
         ; texture = TEXTUREMASK-texture
         mov w11 @TEXTUREMASK
         sub texture w11 texture
       })
  
  ;if(lastside!=-1) ScalePost();
  ldr w10 lastside:
  mov w11 @0
  sub w11 w11 @1
  (/when (w10 ne w11) (preserve [x1 x2 x3 x4] { bl ScalePost: }))

  ;lastside = 0
  mov x10 @0
  adr ptr lastside:
  str w10 [ptr]

  ;lastintercept = ytile
  ldr w10 ytile:
  adr ptr lastintercept:
  str w10 [ptr]

  ;lasttilehit = tilehit
  ldr w10 tilehit:
  adr ptr lasttilehit:
  str w10 [ptr]
  
  ;lasttexture = texture
  adr ptr lasttexture:
  str texture [ptr]


  ;wallheight[pixx] = CalcHeight()
  (preserve [x1 x2 x3 x4] { bl CalcHeight: })
 ; (debug-str-reg "calcheight " x0)
  adr ptr wallheight:
  ldr w10 pixx:
  lsl w10 w10 @2
  add ptr ptr w10
  str w0 [ptr]
  
  ;postx = pixx
  ldr w10 pixx:
  adr ptr postx:
  str w10 [ptr]


  ;; ;wallpic = horizwall[tilehit]  ;
  ldr w10 tilehit:
  lsl w10 w10 @1
  adr ptr horizwall:
  add ptr ptr w10
  ldrh wallpic [ptr]
;  (debug-str-reg "wallpix " wallpic)

  ;postsource = PM_GetTexture(wallpic) + texture
  ; this is a bit different for us since we don't
  ; have a page manager and are not using palette indexes.

  ; we need to find the start of the image data for wallpic index
  ; then offset into it texture << 2

  ldr ptr textures-address:
  mov w10 @$4000      ; texture size
  mul w11 w10 wallpic ; size * index
  add ptr ptr w11     ; start+offset

  ; now we are pointing at the start of image
  ; move along texture * 4 bytes
  lsl w10 texture @2
  add ptr ptr w10

  mov x10 ptr
  adr ptr postsource:
  str x10 [ptr]

  
  })

(subr HitVertWall
      ([vptr x1]

       [wallpic w2]
       [texture w3]
       [ptr x4]
       )
      [x1] {
;  (debug-str "in hitvertwall" #t)
  ;texture = ((yintercept+texdelta)>>TEXTUREFROMFIXEDSHIFT)&TEXTUREMASK;  
  ldr w10 yintercept:
  ldr w11 texdelta:
  add w10 w10 w11
  asr w10 w10 @TEXTUREFROMFIXEDSHIFT
  mov w12 @TEXTUREMASK
  and w10 w10 w12
  mov texture w10
;  (debug-str-reg "texture " w10)

  ;if xtilestep == -1
  ldr w10 xtilestep:
  mov w11 @0
  sub w11 w11 @1 ; -1
  (/when (w10 eq w11)
       {
         ; texture = TEXTUREMASK-texture
         mov w11 @TEXTUREMASK
         sub texture w11 texture
         ;xintercept += TILEGLOBAL;
         ldr w10 xintercept:
         (load-immediate w11 TILEGLOBAL)
         add w10 w10 w11
         adr ptr xintercept:
         str w10 [ptr]             
       })
  
  ;if(lastside!=-1) ScalePost();
  ldr w10 lastside:
  mov w11 @0
  sub w11 w11 @1
  (/when (w10 ne w11) (preserve [x1 x2 x3 x4] {
                                               bl ScalePost:
                                                  }))

  ;lastside = 1
  mov x10 @1
  adr ptr lastside:
  str w10 [ptr]

  ;lastintercept = xtile
  ldr w10 xtile:
  adr ptr lastintercept:
  str w10 [ptr]

  ;lasttilehit = tilehit
  ldr w10 tilehit:
  adr ptr lasttilehit:
  str w10 [ptr]
  
  ;lasttexture = texture
  adr ptr lasttexture:
  str texture [ptr]


  ;wallheight[pixx] = CalcHeight()
  (preserve [x1 x2 x3 x4] { bl CalcHeight: })
;  (debug-str-reg "calcheight " x0)
  adr ptr wallheight:
  ldr w10 pixx:
  lsl w10 w10 @2
  add ptr ptr w10
  str w0 [ptr]
  
  ;postx = pixx
  ldr w10 pixx:
  adr ptr postx:
  str w10 [ptr]


  ;; ;wallpic = vertwall[tilehit]  ;
  ldr w10 tilehit:
  lsl w10 w10 @1
  adr ptr vertwall:
  add ptr ptr w10
  ldrh wallpic [ptr]
;  (debug-str-reg "wallpix " wallpic)

  ;postsource = PM_GetTexture(wallpic) + texture
  ; this is a bit different for us since we don't
  ; have a page manager and are not using palette indexes.

  ; we need to find the start of the image data for wallpic index
  ; then offset into it texture << 2

  ldr ptr textures-address:
  mov w10 @$4000      ; texture size
  mul w11 w10 wallpic ; size * index
  add ptr ptr w11     ; start+offset

  ; now we are pointing at the start of image
  ; move along texture * 4 bytes
  lsl w10 texture @2
  add ptr ptr w10

  mov x10 ptr
  adr ptr postsource:
  str x10 [ptr]

  
})

(subr CalcHeight
      ([vptr x1]

       )
      [x1] {
;  (debug-str "in calcheight " #t)
  ;fixed z = FixedMul(xintercept - viewx, viewcos) - FixedMul(yintercept - viewy, viewsin);
  ldr w10 xintercept:
  ldr w11 viewx:                
  ldr w12 viewcos:
;  (debug-str-reg "xintercept " w10)
;  (debug-str-reg "viewx " w11)
;  (debug-str-reg "viewcos " w12)
  sub w10 w10 w11
  sxtw x10 w10
  sxtw x12 w12
  (FixedMul x20 x10 x12)
;  (debug-str-reg "fixed1 " x20)
  
  ldr w10 yintercept:
  ldr w11 viewy:                
  ldr w12 viewsin:
  sub w10 w10 w11
  sxtw x10 w10
  sxtw x12 w12
  (FixedMul x21 x10 x12)
;  (debug-str-reg "fixed2 " x21)
  ; x10 = z
  sub w10 x20 x21
 ; (debug-str-reg "z " w10)
  ; if (z < mindist) z = mindist
  mov x11 @MINDIST
  (/when (w10 lt x11) { 
    mov w10 @MINDIST
  })

  ; int height = heightnumberator / (z>>8)
  asr w10 w10 @8
  ldr w11 heightnumerator:
;  (debug-str-reg "asr z " w10)
;  (debug-str-reg "numerator " w11)
  udiv w10 w11 w10
;  (debug-str-reg "udiv " w10)
  
 ; pretty sure this code isn't used/*
 ;   if(height < min_wallheight)
 ;       min_wallheight = height;
  ;   */

  mov x0 x10 
})

(subr AsmRefresh
      ([vptr x1]

       [xstep w2]
       [ystep w3]
       [xpartial w4]
       [ypartial w5]
       [ptr x6]
       [xtemp x7]

       [local-pixx w8]
       [wtemp w9]

       )
      [x1] {
  ; NOTE: this function uses x10+ for all item temporaries, except the named ones above
  ; (and our old friend x0 of course)
 
  ; this is the actual raycaster

;  (debug-str "enter asm referesh" #t)          
  mov xstep @0
  mov ystep @0
  mov xpartial @0
  mov ypartial @0

  ; for each X column of the screen ..
  (/for {mov local-pixx @0} (local-pixx lt  @320) (inc local-pixx) {
;    (debug-str "pixx loop" #t)
;    (debug-str-reg "LOOPINDEX " local-pixx)
    ; store global pixx (keep in sync with loop counter)
    adr ptr pixx:
    str local-pixx [ptr]
    ; perform angle calcs
    
    ;w0 = midangle + pixelangle[pixx]
    adr ptr pixelangle:    
    mov wtemp local-pixx
    lsl wtemp wtemp @1
    add ptr ptr wtemp 
    ldrh w0 [ptr]
    sxth w0 w0
;    (debug-str-reg "pixelangle[pixx] = " w0)
    ldr wtemp midangle:
    add w10 w0 wtemp
;    (debug-str-reg "+midangle = " w0)

    (/when (w10 lt @0)  {
;      (debug-str "angl < 0" #t)
      mov w11 @FINEANGLES
      add w10 w10 w11
    })

    (/when (w10 ge @3600)  {
;      (debug-str "angl >= 3600" #t)                  
      mov w11 @FINEANGLES
      sub w10 w10 w11

      })

    (/cond
     ([(w10 lt @900)
       {
;         (debug-str "angl < 900" #t)
         mov w0 @1
         adr ptr xtilestep:
         str w0 [ptr]
         ; TODO: need movn / neg
         mov w0 @0
         sub w0 w0 @1  ; -1
         adr ptr ytilestep:
         str w0 [ptr]
         ; xstep = finetangent[900-1-angl]
         adr ptr finetangent:
         mov w11 @899 ; w11 index
         sub w11 w11 w10 ; index -= angl
;         (debug-str-reg "index " w11)
         lsl w11 w11 @2  ; 32 bit
         add ptr ptr w11
         ldr xstep [ptr]
;         (debug-str-reg "xstep " xstep)

         ;ystep = -finetangent[angl]
         mov w11 w10 ; w11 = angl
         adr ptr finetangent:
         lsl w11 w11 @2
         add ptr ptr w11
         ldr ystep [ptr]
         sub ystep wzr ystep
;         (debug-str-reg "ystep " ystep)
         ; xpartial = xpartialup
         ldr xpartial xpartialup:
;         (debug-str-reg "xpartial " xpartial)
         ; ypartial = ypartialdown
         ldr ypartial ypartialdown:
;         (debug-str-reg "ypartial " ypartial)
         
       }]
      [(w10 lt @1800)
       {

;         (debug-str "angl < 1800" #t)
         mov w0 @0
         sub w0 w0 @1  ; -1
         adr ptr xtilestep:
         str w0 [ptr]
         adr ptr ytilestep:
         str w0 [ptr]
         ; xstep = -finetangent[angl-900]
         adr ptr finetangent:
         mov w11 @900 ; w11 index
         sub w11 w10 w11 ; angle - 900
;         (debug-str-reg "index " w11)
         lsl w11 w11 @2  ; 32 bit
         add ptr ptr w11
         ldr xstep [ptr]
         sub xstep xzr xstep
;         (debug-str-reg "xstep " xstep)

         ;ystep = -finetangent[1800-1-angl]
         mov w11 @1799
         sub w11 w11 w10
         adr ptr finetangent:
         lsl w11 w11 @2
         add ptr ptr w11
         ldr ystep [ptr]
         sub ystep wzr ystep
;         (debug-str-reg "ystep " ystep)
         ldr xpartial xpartialdown:
;         (debug-str-reg "xpartial " xpartial)
         ldr ypartial ypartialdown:
;         (debug-str-reg "ypartial " ypartial)
         

       }]
      [(w10 lt @2700)
       {
;        (debug-str "angl < 2700" #t)
         mov w0 @0
         sub w0 w0 @1  ; -1
         adr ptr xtilestep:
         str w0 [ptr]
         mov w0 @1
         adr ptr ytilestep:
         str w0 [ptr]
         ; xstep = -finetangent[2700-1-angl]
         adr ptr finetangent:
         mov w11 @2699 ; w11 index
         sub w11 w11 w10 ; 
;         (debug-str-reg "index " w11)
         lsl w11 w11 @2  ; 32 bit
         add ptr ptr w11
         ldr xstep [ptr]
         sub xstep xzr xstep
;         (debug-str-reg "xstep " xstep)

         ;ystep = finetangent[angl-1800]
         mov w11 @1800
         sub w11 w10 w11
         adr ptr finetangent:
         lsl w11 w11 @2
         add ptr ptr w11
         ldr ystep [ptr]
;         (debug-str-reg "ystep " ystep)
         ldr xpartial xpartialdown:
;         (debug-str-reg "xpartial " xpartial)
         ldr ypartial ypartialup:
;         (debug-str-reg "ypartial " ypartial)
      
        }]
      [(w10 lt @3600)
       {
;        (debug-str "angl < 3600" #t)
   
         mov w0 @1
         adr ptr xtilestep:
         str w0 [ptr]
         adr ptr ytilestep:
         str w0 [ptr]
         ; xstep = finetangent[angl-2700]
         adr ptr finetangent:
         mov w11 @2700 ; w11 index
         sub w11 w10 w11 ; 
;         (debug-str-reg "index " w11)
         lsl w11 w11 @2  ; 32 bit
         add ptr ptr w11
         ldr xstep [ptr]
;         (debug-str-reg "xstep " xstep)

         ;ystep = finetangent[3600-1-angl]
         mov w11 @3599
         sub w11 w11 w10
         adr ptr finetangent:
         lsl w11 w11 @2
         add ptr ptr w11
         ldr ystep [ptr]
;         (debug-str-reg "ystep " ystep)
         ldr xpartial xpartialup:
;         (debug-str-reg "xpartial " xpartial)
         ldr ypartial ypartialup:
;         (debug-str-reg "ypartial " ypartial)
   
        }]))

    ;yintercept = FixedMul(ystep,xpartial) + viewy
    ; make sure these are in x registers first
    ; sign extend to 64 bits

    sxtw x10 ystep
    sxtw x11 xpartial
    (FixedMul x12 x10 x11)
    mov x0 x12
;    (debug-str-reg "fixed mul res " w0)
    ldr wtemp viewy:
    add w0 w0 wtemp
    adr ptr yintercept:
    str w0 [ptr]
;    (debug-str-reg "yintercept " w0)
    mov w12 w0 ; w12 = yintercept (used in a moment)
    
    ;xtile = focaltx + xtilestep
    ldr w10 focaltx:
    ldr w11 xtilestep:
    add w10 w10 w11
    adr ptr xtile:
    str w10 [ptr]
;    (debug-str-reg "xtile " w10)
    
    ; xspot = (xtile << 6) + (yintercept >> 16)
    lsl w10 w10 @6
    lsr w12 w12 @16
    add w0 w10 w12
    adr ptr xspot:
    str w0 [ptr]
;    (debug-str-reg "xspot " w0)

    ;xintercept = FixedMul(xstep,ypartial) + viewx
    sxtw x10 xstep
    sxtw x11 ypartial
    (FixedMul x12 x10 x11)
    mov x0 x12
;    (debug-str-reg "fixed mul res " w0)
    ldr wtemp viewx:
    add w0 w0 wtemp
    adr ptr xintercept:
    str w0 [ptr]
;    (debug-str-reg "xintercept " w0)
    mov w12 w0 ; w12 = xintercept (used in a moment)

    ;ytile = focalty + ytilestep
    ldr w10 focalty:
;        (debug-str-reg "focalty " w10)
    ldr w11 ytilestep:
;            (debug-str-reg "ytilestep " w11)
    add w10 w10 w11
    adr ptr ytile:
    str w10 [ptr]
;    (debug-str-reg "ytile " w10)
    
    ; yspot = ((xintercept >> 16) << 6) + ytile

    lsr w12 w12 @16
    lsl w12 w12 @6
    add w0 w10 w12
    adr ptr yspot:
    str w0 [ptr]
;    (debug-str-reg "yspot " w0)

    adr ptr texdelta:
    mov x0 @0
    str w0 [ptr]
    

    ;  special pushwall handling (TODO)
            
    ; begin raycasting loop
  :start-vert-loop

    ; if ytilestep = -1 && (yintercept>>16 <= ytile) goto horizentry
    ; if ytilestep = 1 && (yintercept>>16 >= ytile) goto horizentry
    ldr w10 ytilestep:
    ldr w11 yintercept:
    asr w11 w11 @16
;    (debug-str-reg "asr " w11)
    mov w12 @0
    sub w12 w12 @1 ; w12 = -1
    ldr w13 ytile:
    ; if ytilestep = -1 && (yintercept>>16 <= ytile) goto horizentry
    (/when (/and (w10 eq w12) (w11 le w13)) {
;      (debug-str "check 1 true" #t)
      b horizentry:                                             
      })
    ; if ytilestep = 1 && (yintercept>>16 >= ytile) goto horizentry
    (/when (/and (w10 eq @1) (w11 ge w13)) {
;      (debug-str "check 2 true" #t)
      b horizentry:                                             
    })
  :vertentry
    ; TODO: there's an OOB check we don't need right now
    ; if(xspot > maparea) break;
;    (debug-str "in vertentry" #t)
    ; tilehit = tilemap[xspot]
    adr ptr tilemap:
    ldr wtemp xspot:
    add ptr ptr wtemp
    ldrb w0 [ptr]
;    (debug-str-reg "tilehit " w0)
    adr ptr tilehit:
    str w0 [ptr]

    ;if tilehit
    (/when (w0 ne @0) {

      ;TODO: a bunch of door and pushwall stuff
;      (debug-str-reg "HIT V " w0)                 

      ;xintercept=xtile<<TILESHIFT;
      ldr w10 xtile:
      lsl w10 w10 @TILESHIFT
      adr ptr xintercept:
      str w10 [ptr]
;      (debug-str "1 " #t)
      ;ytile = (short) (yintercept >> TILESHIFT);
      ldr w10 yintercept:
      asr x10 x10 @TILESHIFT
      adr ptr ytile:
      str w10 [ptr]
;      (debug-str "2 " #t)

      (preserve [x1 x2 x3 x4 x5 x6 x7 x8 x9] {
        bl HitVertWall:
      })        
      b loop-next+
    })                   

  :passvert  

    ;; todo spotvis update when we take on sprites
    ;xtile += xtilestep
    ldr w10 xtile:
    ldr w11 xtilestep:
    add w10 w10 w11
    adr ptr xtile:
    str w10 [ptr]

    ;yintercept += ystep
    ldr w12 yintercept:
    ;ldr w11 ystep:
    add w10 w12 ystep
    adr ptr yintercept:
    str w10 [ptr]

    ; xspot = (xtile << 6) + (yintercept >> 16)
    mov w12 w10
    ldr w10 xtile:
    lsl w10 w10 @6
    lsr w12 w12 @16
    add w0 w10 w12
    adr ptr xspot:
    str w0 [ptr]
;    (debug-str-reg "xspot v " w0)

    b start-vert-loop:

  :start-horiz-loop
    ; if xtilestep = -1 && (xintercept>>16 <= xtile) goto vertentry
    ; if xtilestep = 1 && (xintercept>>16 >= xtile) goto vertentry
    ldr w10 xtilestep:
    ldr w11 xintercept:
    asr w11 w11 @16
    mov w12 @0
    sub w12 w12 @1 ; w12 = -1
    ldr w13 xtile:
    ; if xtilestep = -1 && (xintercept>>16 <= xtile) goto vertentry
    (/when (/and (w10 eq w12) (w11 le w13)) {
;      (debug-str "check 3 true" #t)
      b vertentry:                                             
      })
    ; if xtilestep = 1 && (xintercept>>16 >= xtile) goto vertentry
    (/when (/and (w10 eq @1) (w11 ge w13)) {
;      (debug-str "check 4 true" #t)
      b vertentry:                                             
    })
  :horizentry
;    (debug-str "in horizentry" #t)
    ; tilehit = tilemap[yspot]
    adr ptr tilemap:
    ldr wtemp yspot:
    add ptr ptr wtemp
    ldrb w0 [ptr]
;    (debug-str-reg "tilehit " w0)
    adr ptr tilehit:
    str w0 [ptr]

    ;if tilehit
    (/when (w0 ne @0) {

      ;TODO: a bunch of door and pushwall stuff
;      (debug-str-reg "HIT H " w0)                 
      ;yintercept=ytile<<TILESHIFT;
      ldr w10 ytile:
      lsl w10 w10 @TILESHIFT
      adr ptr yintercept:
      str w10 [ptr]
      
      ;xtile = (short) (xintercept >> TILESHIFT);
      ldr w10 xintercept:
      asr x10 x10 @TILESHIFT
      adr ptr xtile:
      str w10 [ptr]
      (preserve [x1 x2 x3 x4 x5 x6 x7 x8 x9] {
        bl HitHorizWall:
      })        
  
  
      
      ;break
      b loop-next+
    })                   

    
  :passhoriz
    ;; todo spotvis update when we take on sprites
    ;ytile += ytilestep
    ldr w10 ytile:
;    (debug-str-reg "ytile " w10)
    ldr w11 ytilestep:
;    (debug-str-reg "ytilestep " w11)
    add w10 w10 w11
    adr ptr ytile:
    str w10 [ptr]
;    (debug-str-reg "ph ytile " w10)
    ;xintercept += xstep
    ldr w12 xintercept:
;    (debug-str-reg "xintercept " w12)
    ;ldr w11 xstep:
;    (debug-str-reg "xstep " xstep)
    add w10 w12 xstep
    adr ptr xintercept:
    str w10 [ptr]
;    (debug-str-reg "ph xintercept " w10)
    ; yspot = ((xintercept >> 16) << 6) + ytile
    mov w12 w10
    asr w12 w12 @16
    lsl w12 w12 @6
    ldr w10 ytile:
    add w0 w10 w12
    adr ptr yspot:
    str w0 [ptr]
 ;   (debug-str-reg "yspot h " w0)

    
    b start-horiz-loop:

  :loop-next
    ldr local-pixx pixx:
  })

;  (debug-str "exit asm referesh" #t)          
})

(subr WallRefresh
      ([vptr x1]

       [ptr x2]
       [temp x2]
       )
      [x1] {
 ; xpartialdown = viewx & (TILEGLOBAL-1)
  (load-immediate temp (- TILEGLOBAL 1))
  ;; adr x0 viewx:
  ;; ldr w0 [x0]
  ldr w0 viewx:
  and x0 x0 temp
  adr ptr xpartialdown:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "xpartialdown is " x0)
  ;; ; end debug

  ; xpartialup = TILEGLOBAL - xpartialdown (x0)
  (load-immediate temp TILEGLOBAL)
  sub x0 temp x0
  adr ptr xpartialup:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "xpartialup is " x0)
  ;; ; end debug

  
  ; ypartialdown = viewy & (TILEGLOBAL-1)
  (load-immediate temp (- TILEGLOBAL 1))
  ldr w0 viewy:
;  (debug-str-reg "viewy is " w0)
  and x0 x0 temp
;  (debug-str-reg "result is " w0)
  adr ptr ypartialdown:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "ypartialdown is! " x0)
  ;; ; end debug

  
  ; ypartialup = TILEGLOBAL - ypartialdown (x0)
  (load-immediate temp TILEGLOBAL)
  sub x0 temp x0
  adr ptr ypartialup:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "ypartialup is " x0)
  ;; ; end debug

  ; lastside = -1
  mov x0 @0
  sub x0 x0 @1
  adr ptr lastside:
  str w0 [ptr]
  bl AsmRefresh:
  bl ScalePost:
})

(subr ThreeDRefresh
      ([vptr x1]
       
       [ply x2]
       [temp w3]
       [temp2 w4]
       [temp3 x5]
       [ptr x5]
       [vangle w6])
      [x1]
      {

  ; first we'd clear spotvis that will detect sprite hits
  ; but we are not doing this yet     

  ; ===============
  ; CalcViewVariables

  ; player: is a pointer!
  ldr ply player:

  ; viewangle = ply->angle    
  (_objstruct/angle-get temp ply)    
  adr x0 viewangle:
  str temp [x0]
  mov vangle temp
  ;; ; debug
  ;; ldr w0 [x0]
  ;; (debug-str-reg "viewangle is " x0)
  ;; ; end debug
  
  
  ;midangle = viewangle * (FINEANGLES/ANGLES)
  (load-immediate x0 (/ FINEANGLES ANGLES))
  mul temp2 temp x0
  adr x0 midangle:
  str temp2 [x0]

  ; debug
  ldr w0 [x0]
;  (debug-str-reg "midangle is " x0)
  ; end debug
  
  ;TODO: really need to add support for the ldr / str with
  ; offset registers optionally shifted
  
  ; viewsin = sintable[viewangle]
  ; temp = viewangle
  adr ptr sintable:
  lsl temp2 vangle @2  ; multiply by 4 since 32bits
  add ptr ptr temp2    ; todo: this could all be 1 instruction
  ldr w0 [ptr]
  adr ptr viewsin:
  str w0 [ptr]

  ; debug
  ldr w0 [ptr]
;  (debug-str-reg "viewsin is " x0)
  ; end debug

  
  ; viewy = player->x + FIxedMul(focallength,viewsin)  
  ldr temp2 focallength:
  sxtw x0 w0
  sxtw temp2 temp2
  (FixedMul temp3 x0 temp2)
;  (debug-str-reg "rresult " temp3)
  (_objstruct/y-get temp2 ply)
;  (debug-str-reg "player->y " temp2)
  sub x0 temp2 temp3
  adr ptr viewy:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "viewy is " x0)
  ;; ; end debug

  ; focalty = viewy >> 16
  lsr w0 w0 @16
  adr ptr focalty:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "focalty is " x0)
  ;; ; end debug

  ; viewcos = costable[viewangle]
  ; tmep = viewangle
  ; costable is a pointer
  ldr ptr costable:
  lsl temp2 vangle @2  ; multiply by 4 since 32bits
  add ptr ptr temp2
  ldr w0 [ptr]
  adr ptr viewcos:
  str w0 [ptr]

  ;;   ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "viewcos is " x0)
  ;; ; end debug

  
  ; viewx = player->x - FIxedMul(focallength,viewcos)  
  ldr temp2 focallength:
  ;  (debug-str-reg "focallength " temp2)
  sxtw x0 w0
  sxtw temp2 temp2
  (FixedMul temp3 x0 temp2)
;  (debug-str-reg "result " temp3)
  (_objstruct/x-get temp2 ply)
;  (debug-str-reg "player->x " temp2)
  sub x0 temp2 temp3
  adr ptr viewx:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "viewx is " x0)
  ;; ; end debug

  
  ; focaltx = viewx >> 16
  lsr w0 w0 @16
  adr ptr focaltx:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "focaltx is " x0)
  ;; ; end debug
  
  
  ; viewtx = player->x >> 16}
  (_objstruct/x-get temp2 ply)
  lsr x0 temp2 @16
  adr ptr viewtx:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "viewtx is " x0)
  ;; ; end debug

  
  ; viewty = player->y >> 16}
  (_objstruct/y-get temp2 ply)
  lsr x0 temp2 @16
  adr ptr viewty:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "viewty is " x0)
  ;; ; end debug

  
  ; =================

  bl VgaClearScreen:
  bl WallRefresh:
  ; ==================

;  (debug-str "finish 3d refersh" #t)
})

; old gfx code that does the slideshows
;; (subr update-gfx  (
;;                   [temp x4] ; temporary / intermediates
;;                   [tptr x8]
;;                   [vptr x9] ; video memory pointer
;;                   [row-size x14]
;;                   ) [x0 x1 x2 x3 x4]
;;                     {

;; :loop
;;     ldr w0 finished-rendering:
;;     cbnz x0 loop-
;;     bl get-back-buffer:
;;     mov vptr x0

;;     ; ===================
;;     ; clear screen
    
;;     ; we do 8 pixels each iteration so
;;     ; we need 320*200/8 = $1F40 iterations
;;     mov temp @$1F40
;;     mov x3 @0
;; :next
;;     str x3 [vptr]
;;     str x3 (vptr @8)
;;     str x3 (vptr @16)
;;     str x3 (vptr @24)
;;     add vptr vptr @32

;;     sub temp temp @1
;;     cbnz temp next-
;;     mov vptr x0

;;     ; end clear screen
;;     ; ====================
    
;;     ; x27 holds a delay
;;     ; this should be in memory
;;     ; since we never even push these!
;;     sub x27 x27 @1
;;     cbnz x27 skip+
;;     mov x27 @10
;;     ; x28 holds next picture to display
;;     ; also can easily be trashed!
;;     sub x28 x28 @1

;;     cbnz x28 skip+
;;     mov x28 @105  ; 130 for 2d pics, 105 for textures
    
;; :skip    
;;     mov x0 vptr
;;     mov x1 x28

;;     mov x1 @15
    
;;     ;bl render-pic:  ; render 2d pics
;;     bl render-tex:   ; render 2d textures
;;     ;signal render has finished
;;     mov temp @1
;;     adr x0 finished-rendering:
;;     str temp [w0]


;;     b loop-
;;    })


; //////////////////////////////////////////////
; WOLFENSTEIN CODE 
; //////////////////////////////////////////////
(define MAXACTORS 150)
(define MAP_HEIGHT 64)
(define MAP_WIDTH 64)
  
(/struct _objstruct
 ([statetype       8] ; *state
  [next            8] ; *next objstruct
  [prev            8] ; *prev objstruct

  [flags           4] ; FL_SHOOTABLE, etc
  [distance        4] ; signed; if negative, wait for that door to open
  [speed           4] ; signed  
  [x               4] ; fixed point
  [y               4] ; fixed point
  [transx          4] ; fixed, in global coord
  [transy          4] ; fixed, in global coord
  
  [ticcount        2]
  [tilex           2]
  [tiley           2]
  [viewx           2]
  [viewheight      2]
  [angle           2]
  [hitpoints       2]

  [temp1           2]
  [temp2           2]
  [hidden          2]
  
  [active          1] ;activetype enum
  [obclass         1] ;classtype enum
  [dirtype         1] ; dirtype enum
  [areanumber      1]

  [pad             4] ; I'm adding this so it is 8 byte aligned (72 bytes)
  ))

(subr setup-game-level
      ([raw-ptr x1]
       [lvl-ptr x2]
       [x x3]
       [y x4]
       [tile w5]
       [offset x6])

      [x1 x2 x3 x4 x5 x6] {

  ; setup the walls and actors
  ; x1 will point at the raw map data for level 1
  ; x2 will point at the 2d byte array tilemap          
  AREATILE = 107
  ldr raw-ptr maps-address: ; this is a linker resolved label
  ; each raw map section is a 16 bit word

  ; for some reason the map is stored sideways compared to the raw data
  ; so we have to calculate the index here;
  ; x is the row and y is the offset [x][y]


  (/for { mov y @0 } (y lt @MAP_HEIGHT) (inc y) {
    (/for { mov x @0 } (x lt @MAP_WIDTH) (inc x) {
      ldrh tile [raw-ptr] @2
      (/when (tile lt @AREATILE) {

        ; lvlptr = tilemap: + (64 * x) + y
        adr lvl-ptr tilemap:
        lsl offset x @6 ; * 64
        add lvl-ptr lvl-ptr offset
        add lvl-ptr lvl-ptr y
        strb tile [lvl-ptr] 
      })
    })                                                     
  })
  
    
})

(subr spawn-player
      ([tilex x1]
       [tiley x2]
       [dir x3]

       [temp x4]
       [ptr x5])
      [x4] {

  ; for now we assign the player to actors[0]
  ; in the future this needs to be in InitActorList
  ; and it must update the freelist etc.         
  ; for now we only have a player and it is hardcoded         
  adr ptr objlist:
  adr temp player:
  str ptr [temp]  ; player = objlist[0]

  mov temp @1

  (_objstruct/obclass-set temp ptr) ; playerobj = 1
  (_objstruct/active-set temp ptr)  ; ac_yes = 1
  (_objstruct/tilex-set tilex ptr)  ; tilex = tilex
  (_objstruct/tiley-set tiley ptr)  ; tiley = tiley

  ; areanumber = map index (tiley << 6) + tilex  
  lsl temp tiley @6
  add temp temp tilex
  ; multiply by 2 here because we need words not bytes. the C code is silently doubling the index
  add temp temp temp
  ldr x0 maps-address: ; this is a linker resolved label
  add temp temp x0
  
  ldrb temp [temp]
  (_objstruct/areanumber-set temp ptr) 

  ; x = (tilex << TILESHIFT) + (TILEGLOBAL/2) (fixed point)
  lsl temp tilex @TILESHIFT
  (load-immediate x0 TILEGLOBAL/2)
  add temp temp x0
  (_objstruct/x-set temp ptr)

  ; y = (tiley << TILESHIFT) + (TILEGLOBAL/2) (fixed point)
  lsl temp tiley @TILESHIFT
  (load-immediate x0 TILEGLOBAL/2)  
  add temp temp x0
  (_objstruct/y-set temp ptr)  


  ; the last bits are state, angle and flags.
  ; state we don't care about for now
  ; angle resolves to zero for the first map so we ignore that for now
  mov temp @180
  (_objstruct/angle-set temp ptr)
  
  mov temp @4
  (_objstruct/flags-set temp ptr)  ; FL_NEVERMARK = 4
  
  ;; (debug-str "obclass" #t)
  ;; (_objstruct/obclass-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "active" #t)
  ;; (_objstruct/active-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "tilex" #t)
  ;; (_objstruct/tilex-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "tiley" #t)
  ;; (_objstruct/tiley-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "areanumber" #t)
  ;; (_objstruct/areanumber-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "x" #t)
  ;; (_objstruct/x-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "y" #t)
  ;; (_objstruct/y-get temp ptr)
  ;; (debug-reg temp)

})          
(subr scan-info-plane
      ([raw-ptr x1]
       [x x2]
       [y x3]
       [temp x4]
       [tile w5])
      [x1 x2 x3 x4] {

  ; scan second map data plane and spawn player, statics, guards etc
  ; for now we are only going to spawn the player        
  ldr raw-ptr maps-address: ; this is a linker resolved label
  ; add 64 * 64 * 2 for start of second plane
  mov x4 @64
  lsl x4 x4 @7
  add raw-ptr raw-ptr x4

  (/for { mov y @0 } (y lt @MAP_HEIGHT) (inc y) {
    (/for { mov x @0 } (x lt @MAP_WIDTH) (inc x) {
      ldrh tile [raw-ptr] @2
      (/when (/and (tile le @22) (tile ge @19)) {
        ;; call spawn-player with x, y, (tile - 19)
        ;; calculate direction
        (preserve [x1 x2 x3] {
          mov x1 x
          mov x2 y
          sub x3 tile @19
          bl spawn-player:
        })
      })
    })                                                     
  })
          
})          
;; ///////////////////////////////////////////////////////
;; wolfenstein GAME STATE VARIABLES
;; ///////////////////////////////////////////////////////
/= 8
; wall locations (byte[64][64])
:tilemap (reserve (* 64 64))
/= 8
; actor locations (ptr[64][64]
:actorat (reserve (* 64 64 8))
/= 8
:objlist (reserve (* (sizeof/_objstruct) MAXACTORS))
/= 8
:player  (write-value-64 0)  ; objstruct*
/= 8
:aux-base (write-value-64 (+ PERIPH-BASE $215000))  ; mini uart registers
:input-state (write-value-32 0) ; latest button byte from the host (KEY-* bits)

/= 8
:horizwall
(write-value-16 0)
(for ([i (in-range 1 64)])
  (write-value-16 (* (- i 1) 2)))

/= 8
:vertwall
(write-value-16 0)
(for ([i (in-range 1 64)])
  (write-value-16 (+ (* (- i 1) 2) 1)))

/= 8
:wallheight
(for ([x (in-range 320)])
  (write-value-32 0))

  ; the following values are used for loading 64 bit addresses/values via LDR(literal)
  ; and they must be 64 bit aliged otherwise the CPU faults!
/= 8
  ; resolve-global-label-address will tell the linker to write the absolute
  ; address of the exported labels here, post-link.  These might be quite far away and
  ; as such out of range of the adr instruction and some jumps.
  ; this lets us load them with ldr.
  :2d-image-header-address (resolve-global-label-address :DATA-2d-image-header)
  :2d-images-address (resolve-global-label-address :DATA-2d-images)
  :textures-address (resolve-global-label-address :DATA-textures)
:maps-address (resolve-global-label-address :DATA-maps)

  ; end linker
  ; ///
; wolf lets you resize the viewport and thus it recalcualtes various projection bits
; our version is hardcoded at 320 * 160 (no border) so we can calculate the bits at compile time
/= 8
;wolf uses a lot of global state!
:postsource (write-value-64 0) ; pointer to texture column data
:focallength (write-value-32 $5700)
:scale (write-value-32 128)
:heightnumerator (write-value-32 223232)
:viewx (write-value-32 0)
:viewy (write-value-32 0)
:viewsin (write-value-32 0)
:viewcos (write-value-32 0)
:vbufPitch (write-value-32 320)
:xpartialup (write-value-32 0)
:xpartialdown (write-value-32 0)
:ypartialup (write-value-32 0)
:ypartialdown (write-value-32 0)
:xintercept (write-value-32 0)
:yintercept (write-value-32 0)
:pixx (write-value-32 0)
:texdelta (write-value-32 0)
:lastside (write-value-32 0)
:lasttilehit (write-value-32 0)
:lasttexture (write-value-32 0)
:lastintercept (write-value-32 0)
:postx (write-value-32 0)
; these next ones are shorts but we'll store them as 4 bytes
:tilehit (write-value-32 0)
:viewangle (write-value-32 0)
:midangle (write-value-32 0)
:angle (write-value-32 0)
:xstep (write-value-32 0)
:ystep (write-value-32 0)
:xtile (write-value-32 0)
:ytile (write-value-32 0)
:xtilestep (write-value-32 0)
:ytilestep (write-value-32 0)
:viewtx (write-value-32 0)
:viewty (write-value-32 0)
:focaltx (write-value-32 0)
:focalty (write-value-32 0)
:xspot (write-value-32 0)
:yspot (write-value-32 0)
; end shorts

(define viewwidth 320)
(define halfview (/ viewwidth 2))
(define facedist (+ 0.0 #x5700 MINDIST))
(define scale 128)
(define heightnumerator 223232)
(define radtoint 572.9578)
/= 8
:pixelangle
(let ([vec (make-vector viewwidth)]
      [calcangle (λ (i)
         (define tang (/(* i #x10000) viewwidth facedist))
         (define ang (atan tang))
         (define intang  (inexact->exact (floor (* ang radtoint))))
         intang)])
  (for ([i (in-range halfview)])
    (let* ([ang (calcangle i)]
           [nang (- ang)])
      (vector-set! vec (- halfview 1 i) ang)
      (vector-set! vec (+ halfview i) nang)))
  (for ([v vec])
;    (displayln v)
    (write-value-16 v)
    ))

(define GLOBAL1 (arithmetic-shift 1 16))
; next up is the tangent table
/= 8
:finetangent
(let ([vec (make-vector (/ FINEANGLES 4))])
  (for ([i (in-range (/ FINEANGLES 8))])
    (define tang (tan (/ (+ i 0.5) radtoint)))
    (vector-set! vec i (inexact->exact (floor (* tang GLOBAL1))))
    (vector-set! vec (- (/ FINEANGLES 4) 1 i) (inexact->exact (floor (* (/ 1 tang) GLOBAL1)))))
  (for ([v vec])
    (write-value-32 v)))

; now for sin / cos
; costable overlays sintable with a quarter phase shift
(define anglestep (/ pi 2 ANGLEQUAD))
/= 8
:costable (write-value-64 0)  ; pointer into sintable
:sintable  ; costable is :sintable + (ANGLEQUAD * 4 (since its 32bits)) (or in this case, just ANGLES!)
(let ([vec (make-vector (+ ANGLES ANGLEQUAD))])
  (define ang 0)
  (for ([i (in-range ANGLEQUAD)])
    (define value (inexact->exact (floor (* (sin ang) GLOBAL1))))
    (vector-set! vec i value)
    (vector-set! vec (+ i ANGLES) value)
    (vector-set! vec (- (/ ANGLES 2) i) value)

    (vector-set! vec (- ANGLES i) (- value))
    (vector-set! vec (+ (/ ANGLES 2) i) (- value))

    (set! ang (+ ang anglestep))
    
    )
  (vector-set! vec ANGLEQUAD 65536)
  (vector-set! vec (* 3 ANGLEQUAD) -65536)
  (for ([v vec])
    (write-value-32 v)))

/= 8
; palette (only used for backgroudn colours)
:palette
(let*
    ([lines  (file->lines palette-file)]
     [splits  (map (λ (line) (string-split line "RGB")) lines)]
     [clean  (map (λ (line) (string-replace (string-replace line ")" "") "(" "")) (flatten splits))]
     [rgbs (map (λ (line) (string-split line ",")) clean)])
  (for ([rgb rgbs])
    (write-value (string->number (string-trim (list-ref rgb 0))))
    (write-value (string->number (string-trim (list-ref rgb 1))))
    (write-value (string->number (string-trim (list-ref rgb 2))))
    (write-value 255)))

(local-send-char)
})

(platform-image "wolfmain.img" "wolfmain.obj" "2dgfx.obj" "textures.obj" "maps.obj")


