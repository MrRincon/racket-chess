#lang racket/gui
(require 2htdp/image (only-in pict pict->bitmap))
(struct piece (sprite posx posy) #:mutable)
(define imageFolder (path->string (build-path (current-directory) "racket-chess\\sprites")))

;map coordinates to cells
(define startingPosition (list
    (piece "white_rook.png" 77 -78)
    (piece "white_knight.png" 55 -78)
    (piece "white_bishop.png" 33 -78)
    (piece "white_queen.png" 11 -78)
    (piece "white_king.png" -11 -78)
    (piece "white_bishop.png" -33 -78)
    (piece "white_knight.png" -55 -78)
    (piece "white_rook.png" -77 -78)
    (piece "white_pawn.png" 77 -56)
    (piece "white_pawn.png" 55 -56)
    (piece "white_pawn.png" 33 -56)
    (piece "white_pawn.png" 11 -56)
    (piece "white_pawn.png" -11 -56)
    (piece "white_pawn.png" -33 -56)
    (piece "white_pawn.png" -55 -56)
    (piece "white_pawn.png" -77 -56)

    (piece "black_pawn.png" 77 54)
    (piece "black_pawn.png" 55 54)
    (piece "black_pawn.png" 33 54)
    (piece "black_pawn.png" 11 54)
    (piece "black_pawn.png" -11 54)
    (piece "black_pawn.png" -33 54)
    (piece "black_pawn.png" -55 54)
    (piece "black_pawn.png" -77 54)
    (piece "black_rook.png" 77 78)
    (piece "black_knight.png" 55 78)
    (piece "black_bishop.png" 33 78)
    (piece "black_queen.png" 11 78)
    (piece "black_king.png" -11 78)
    (piece "black_bishop.png" -33 78)
    (piece "black_knight.png" -55 78)
    (piece "black_rook.png" -77 78)
))

(define img->bmap (lambda (image)
    (pict->bitmap image)
))

(define overlayImage (lambda (a b)
    (overlay/offset a 0 0 b)
))

(define drawToBoard(lambda (board piece x y)
    (overlay/offset piece x y board)
))

(define getChessPieceBitmap (lambda (filename)
    (bitmap/file(string-append imageFolder "\\" filename))
))

(define drawAllPieces (lambda (currentBoardState initialBoard)
    (cond
        [(empty? currentBoardState) (img->bmap initialBoard)]
        [#t (drawAllPieces (rest currentBoardState) (drawToBoard initialBoard (getChessPieceBitmap (piece-sprite (first currentBoardState))) (piece-posx (first currentBoardState)) (piece-posy (first currentBoardState))))]
    )
))

(define root (new frame% [label "Chess"][width 194][height 218]))
(define my-canvas% (class canvas%
    (super-new)
    (define/override (on-event event)
        (mousePosHandler (send event get-x) (send event get-y))
    )

    (define/private mousePosHandler (lambda (x y)
        (cond
            [(and (> x 0) (<= x 180) (> y 0) (<= y 183)) (cond
                [(and (>= x 0) (<= x 22.5)) (display "COL A")]
                [(and (<= x 45) (> x 22.5)) (display "COL B ")]
                [(and (<= x 67.5) (> x 45)) (display "COL C ")]
                [(and (<= x 90) (> x 67.5)) (display "COL D ")]
                [(and (<= x 112.5) (> x 90)) (display "COL E ")]
                [(and (<= x 135) (> x 112.5)) (display "COL F ")]
                [(and (<= x 157.5) (> x 135)) (display "COL G ")]
                [(and (<= x 180)(> x 157.5)) (display "COL H")]
            )
            (cond
                [(and (>= y 0)(<= y 22.875)) (displayln "ROW 8")]
                [(and (<= y 45.75) (> y 22.875)) (displayln "ROW 7")]
                [(and (<= y 68.625) (> y 45.75)) (displayln "ROW 6")]
                [(and (<= y 91.5) (> y 68.625)) (displayln "ROW 5")]
                [(and (<= y 114.375) (> y 91.5)) (displayln "ROW 4")]
                [(and (<= y 137.25) (> y 114.375)) (displayln "ROW 3")]
                [(and (<= y 160.125) (> y 137.25)) (displayln "ROW 2")]
                [(and (<= y 183)(> y 160.125)) (displayln "ROW 1")]
            )]
        )
        
    ))
))



(new my-canvas% [parent root]
    [paint-callback (lambda (canvas dc)
        (send dc draw-bitmap (drawAllPieces startingPosition (getChessPieceBitmap "board.png")) 0 0)
    )]
)
(send root show #t)