#lang racket
(require "aoc.rkt"
         parser-tools/lex
         syntax/parse/define
         threading
         (for-syntax syntax/for-body))
(provide (all-from-out threading)
         dbg vector-reverse
         values->list list->values
         blank? datum? format-datum
         for/min for/max
         lex-string
         answer)

(define (dbg a)
  (pretty-print a)
  a)

(define (vector-reverse v)
  (list->vector (reverse (vector->list v))))

(define-simple-macro (values->list body:expr ...+)
  (call-with-values (lambda () body ...) list))

(define (list->values lst)
  (apply values lst))

(define (blank? str)
  (for/and ([c (in-string str)])
    (char-blank? c)))

(define (datum? x)
  (or (list? x) (symbol? x)))

(define (string->datum str)
  (unless (blank? str)
    (define result (read (open-input-string (format "(~a)" str))))
    (if (= (length result) 1)
        (car result)
        result)))

(define (format-datum datum-template . args)
  (unless (datum? datum-template)
    (raise-argument-error 'format-datums "datum?" datum-template))
  (string->datum (apply format (format "~a" datum-template)
                        (map (lambda (arg)
                               (if (syntax? arg)
                                   (syntax->datum arg)
                                   arg))
                             args))))

(define-syntax (for/max stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(for/fold/derived original
           ([current-max -inf.0])
           clauses
           pre-body ...
           (define maybe-new-max (let () post-body ...))
           (if (> maybe-new-max current-max)
               maybe-new-max
               current-max)))]))
(define-syntax (for/min stx)
  (syntax-case stx ()
    [(_ clauses body ... tail-expr)
     (with-syntax ([original stx]
                   [((pre-body ...) (post-body ...))
                    (split-for-body stx #'(body ... tail-expr))])
       #'(for/fold/derived original
           ([current-min +inf.0])
           clauses
           pre-body ...
           (define maybe-new-min (let () post-body ...))
           (if (< maybe-new-min current-min)
               maybe-new-min
               current-min)))]))

(define (lex-string lexer str)
  (define in (open-input-string str))
  (port-count-lines! in)
  (let loop ([v (lexer in)])
    (cond [(void? (position-token-token v)) (loop (lexer in))]
          [(eof-object? (position-token-token v)) '()]
          [else (cons v (loop (lexer in)))])))

(define (answer day part answer)
  (printf "answer ~a.~a: ~s\n" day part answer)
  (unless (aoc-complete? day part)
    (printf "submit? [Y/n]: ")
    (match (string-downcase (string-trim (read-line)))
      [(or "" "y" "yes")
       (printf "glhf\n")
       (define resp
         (aoc-submit-answer (getenv "AOC_YEAR")
                            (~a day)
                            (getenv "AOC_SESSION")
                            (~a part)
                            (~a answer)))
       (printf "server responded: ~a\n" resp)]
      [_ (printf "coward\n")])))
