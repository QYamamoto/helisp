#lang racket

(require syntax/strip-context)
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

(define-tokens basic-tokens (
    NUM
    ID
))

(define-empty-tokens punct-tokens (
    LBRACKET
    RBRACKET
    BITWISE-OR
    SEMICOLON
    EQ
    DOUBLE-ADD
    ADD
    SUB
    MUL
    U8
    PRINT
    SPRINT
    EXIT
    CYBERSPACES
    EOF
))

(define (id->reserved id)
    (case id
    [("u8") (token-U8)]
    [("print") (token-PRINT)]
    [("sprint") (token-SPRINT)]
    [("exit") (token-EXIT)]
    [("cyberspaces") (token-CYBERSPACES)]
    [else (token-ID (string->symbol id))]))

(define helang-lexer (lexer
    [(eof) (token-EOF)]
    ["[" (token-LBRACKET)]
    ["]" (token-RBRACKET)]
    ["|" (token-BITWISE-OR)]
    [";" (token-SEMICOLON)]
    ["=" (token-EQ)]
    ["++" (token-DOUBLE-ADD)]
    ["+" (token-ADD)]
    ["-" (token-SUB)]
    ["*" (token-MUL)]
    [(:+ numeric) (token-NUM (string->number lexeme))]
    [(:: "//" (:* (:~ #\newline))) (helang-lexer input-port)]
    [(:: (:or alphabetic #\_)
        (:* (:or alphabetic numeric #\_)))
        (id->reserved lexeme)]
    [whitespace (helang-lexer input-port)]))

(define helang-parser (parser
    [start +module]
    [end EOF]
    [error (curry printf "~a ~a ~a\n")]
    [tokens basic-tokens punct-tokens]
    [grammar
        [+module
            [(body) `(module anything racket
                (require helang)
                ,@$1)]]
        [body
            [(stmt SEMICOLON) (list $1)]
            [(stmt SEMICOLON body) (cons $1 $3)]]
        [stmt
            [(CYBERSPACES) '(cyberspaces)]
            [(EXIT) '(exit)]
            [(expr) $1]
            [(PRINT expr) `(+print ,$2)]
            [(SPRINT expr) `(sprint ,$2)]
            [(U8 ID EQ expr) `(define ,$2 ,$4)]
            [(U8 ID) `(define ,$2 (vector 0))]
            [(ID EQ expr) `(set! ,$1 ,$3)]
            [(ID DOUBLE-ADD) `(increment ,$1)]
            [(factor LBRACKET expr RBRACKET EQ expr)
                `(array-set! ,$1 ,$3 ,$6)]]
        [expr
            [(term) $1]
            [(expr ADD term) `(add ,$1 ,$3)]
            [(expr SUB term) `(sub ,$1 ,$3)]]
        [term
            [(factor) $1]
            [(factor MUL factor) `(mul ,$1 ,$3)]]
        [factor
            [(ID) $1]
            [(array) (cons 'vector $1)]
            [(LBRACKET expr RBRACKET) `(make-array ,$2)]
            [(factor LBRACKET expr RBRACKET) `(array-ref ,$1 ,$3)]]
        [array
            [(NUM) (list $1)]
            [(NUM BITWISE-OR array) (cons $1 $3)]]]))

(provide (rename-out 
    [helang-read read]
    [helang-read-syntax read-syntax]))

(define (helang-read in)
    (syntax->datum (helang-read-syntax #f in)))

(define (helang-read-syntax src in)
    (strip-context
        (datum->syntax #f 
            (helang-parser (thunk (helang-lexer in))))))