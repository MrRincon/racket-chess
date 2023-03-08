#lang racket

(define chess (class object%
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

    (define returnPosPx)
))