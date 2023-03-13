#lang racket

(struct piece (name color sprite))
(struct piecePos (piece column row))
(define whiteRook(piece "rook" "white" "white_rook.png"))
(define blackRook (piece "rook" "black" "black_rook.png"))
(define whiteKnight (piece "knight" "white" "white_knight.png"))
(define blackKnight (piece "knight" "black" "black_knight.png"))
(define whiteBishop (piece "bishop" "white" "white_bishop.png"))
(define blackBishop (piece "bishop" "black" "black_bishop.png"))
(define whiteQueen (piece "queen" "white" "white_queen.png"))
(define blackQueen (piece "queen" "black" "black_queen.png"))
(define whiteKing (piece "king" "white" "white_king.png"))
(define blackKing (piece "king" "black" "black_king.png"))
(define whitePawn (piece "pawn" "white" "white_pawn.png"))
(define blackPawn (piece "pawn" "black" "black_pawn.png"))

(define chess% (class object%
    (super-new)
    [init-field (boardState (list
        (piecePos whiteRook "A" "1")
        (piecePos whiteKnight "B" "1")
        (piecePos whiteBishop "C" "1")
        (piecePos whiteQueen "D" "1")
        (piecePos whiteKing "E" "1")
        (piecePos whiteBishop "F" "1")
        (piecePos whiteKnight "G" "1")
        (piecePos whiteRook "H" "1")
        (piecePos whitePawn "A" "2")
        (piecePos whitePawn "B" "2")
        (piecePos whitePawn "C" "2")
        (piecePos whitePawn "D" "2")
        (piecePos whitePawn "E" "2")
        (piecePos whitePawn "F" "2")
        (piecePos whitePawn "G" "2")
        (piecePos whitePawn "H" "2")

        (piecePos blackPawn "A" "7")
        (piecePos blackPawn "B" "7")
        (piecePos blackPawn "C" "7")
        (piecePos blackPawn "D" "7")
        (piecePos blackPawn "E" "7")
        (piecePos blackPawn "F" "7")
        (piecePos blackPawn "G" "7")
        (piecePos blackPawn "H" "7")
        (piecePos blackRook "A" "8")
        (piecePos blackKnight "B" "8")
        (piecePos blackBishop "C" "8")
        (piecePos blackQueen "D" "8")
        (piecePos blackKing "E" "8")
        (piecePos blackBishop "F" "8")
        (piecePos blackKnight "G" "8")
        (piecePos blackRook "H" "8")
    ))]

    [init-field (cells (hash 
        "A" 77
        "B" 55
        "C" 33
        "D" 11
        "E" -11
        "F" -33
        "G" -55
        "H" -77
        "1" -78
        "2" -56
        "3" -34
        "4" -12
        "5" 10
        "6" 32
        "7" 54
        "8" 76
    ))]


    (define/private checkPieceAtPos (lambda (checkPiece board col row)
        (cond
            [(empty? board) #f]
            [(and (equal? (piece-name (piecePos-piece (first board))) (piece-name checkPiece)) (equal? (piecePos-column (first board)) col) (equal? (piecePos-row (first board)) row))#t]
            [#t (checkPieceAtPos checkPiece (rest board) col row)]
        )
    ))
    (define/private getPieceAtPos (lambda (board col row)
        (cond
            [(empty? board) #f]
            [(and (equal? (piecePos-column (first board)) col) (equal? (piecePos-row (first board)) row)) (first board)]
            [#t (getPieceAtPos (rest board) col procedure-reduce-keyword-arity-mask)]
        )
    ))
    (define/private updateBoard (lambda (newBoard)
        (set! boardState newBoard)
    ))

    (define/public translatePiecePosToBoardPos (lambda (cPiece)
        (list (hash-ref cells (piecePos-column cPiece)) (hash-ref cells (piecePos-row cPiece)))
    ))

    (define/public returnPosPx (lambda (x y)
        (list (hash-ref cells x) (hash-ref cells y))
    ))

    (define/public getBoard(lambda ()
        boardState
    ))

    (define/public makeMove (lambda (cPiece icolumn irow column row)
        (println(checkPieceAtPos cPiece boardState icolumn irow))
        (println(getPieceAtPos boardState column row))
    ))
))

(define board (new chess%))
(provide board)
(provide (struct-out piece))
(provide (struct-out piecePos))