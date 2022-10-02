#lang pisemble
(require threading)
(require syntax/location racket/path)





(aarch64 "2dgfx.obj" [] {

 
; layout grpahics used in the non-3d parts of the game.
; first there will be a header of a struct
; { short width; short height; int offset } // 8 bytes
; the offset will point to where the data starts 
; when added to the DATA-2d-image: label.
; the game loads the images by index which will correspond
; to index << 3 in the header array

:DATA-2d-image-header
(let ([image-bytes
       (~>>
        (file->lines "C:/repos/pisemble/wolf3d/wolfpics/index.dat")
        (map (λ (s) (string-split s ";")))
        (map (match-lambda

               [(list index width height offset filename)
                (begin
                  ; write the header data array as a side-effect
                  (write-value-16 (string->number width))
                  (write-value-16 (string->number height))
                  (write-value-32 (string->number offset))
                  ; load the 24bit pixel data from the file
                  (bytes->list (file->bytes filename)))])))])
  (begin
    {
     ; label the start of the actual data
     :DATA-2d-images
     ; dump pixel bytes
     (write-values image-bytes)
    }
  ))
})

(aarch64 "textures.obj" [] {



; these are the textures used in the raycaster
; they are all 64*64 bitmaps of 4 byte pixels                            
; the program can work out the indexing 

(let*
    ([texture-bytes
      (~>>
       (directory-list "c:/repos/pisemble/wolf3d/wolftex")
       (map path->string)
       (filter (λ (path) (string-suffix? path ".dat")))
       (sort _ (λ (path1 path2)
                 (< (string->number (car (string-split path1 "." )))
                    (string->number (car (string-split path2 "." ))))))
       (map (λ (path) (bytes->list (file->bytes (string-append "c:/repos/pisemble/wolf3d/wolftex/" path))))))])
  (begin
    {
     :DATA-textures
     (write-values texture-bytes)
    }))
    
})


















