#lang racket/gui
(require 2htdp/image (only-in pict pict->bitmap))
(require "chessstate.rkt")
(define imgFolder (path->string (build-path (current-directory) "racket-chess\\sprites")))

(define root (new frame% [label "Chess"][width 194][height 218]))

(define img->bmap (lambda (image)
    (pict->bitmap image)
))

(define placePiece (lambda (board cPiece coords)
    (overlay/offset cPiece (first coords) (second coords) board)
))

(define getBitmap (lambda (filename)
    (bitmap/file (string-append imgFolder "\\" filename))
))



(define drawBoard (lambda (boardState iBoardState)
    (cond
        [(empty? boardState) (img->bmap iBoardState)]
        [#t (drawBoard (rest boardState) (placePiece iBoardState (getBitmap (piece-sprite (piecePos-piece (first boardState)))) (send board translatePiecePosToBoardPos (first boardState))))]
    )
))

(define ccanvas% (class canvas%
    (super-new)
    [init-field (startPos '())]
    [init-field (endPos '())]
    (define/override (on-e
    vent event)
        (when (send event button-down? 'left)
            (let ((click (clickHandler (send event get-x) (send event get-y))))
                (cond
                    [(equal? click #f)]
                )
            )
            
        )
    )
    (define/private clickHandler (lambda (x y)
        (let ((coordX "") (coordY "") (finalCoords ""))
            (cond
                [(and (> x 0) (<= x 180) (> y 0) (<= y 183)) (cond
                    [(and (>= x 0) (<= x 22.5)) (set! coordX "A")]
                    [(and (<= x 45) (> x 22.5)) (set! coordX "B")]
                    [(and (<= x 67.5) (> x 45)) (set! coordX "C")]
                    [(and (<= x 90) (> x 67.5)) (set! coordX "D")]
                    [(and (<= x 112.5) (> x 90)) (set! coordX "E")]
                    [(and (<= x 135) (> x 112.5)) (set! coordX "F")]
                    [(and (<= x 157.5) (> x 135)) (set! coordX "G")]
                    [(and (<= x 180)(> x 157.5)) (set! coordX "H")]
                )
                (cond
                    [(and (>= y 0)(<= y 22.875)) (set! coordY "8")]
                    [(and (<= y 45.75) (> y 22.875)) (set! coordY "7")]
                    [(and (<= y 68.625) (> y 45.75)) (set! coordY "6")]
                    [(and (<= y 91.5) (> y 68.625)) (set! coordY "5")]
                    [(and (<= y 114.375) (> y 91.5)) (set! coordY "4")]
                    [(and (<= y 137.25) (> y 114.375)) (set! coordY "3")]
                    [(and (<= y 160.125) (> y 137.25)) (set! coordY "2")]
                    [(and (<= y 183)(> y 160.125)) (set! coordY "1")]
                ) (cons coordX coordY)]
                [else #f]
            )
        )
    ))
))

(new ccanvas%
    [parent root]
)
#|
(new canvas% 
    [parent  root]
    [paint-callback (lambda (canvas dc)
        (send dc draw-bitmap (drawBoard (send board getBoard) (getBitmap "board.png")) 0 0)
    )]    
)|#

(send root show #t)