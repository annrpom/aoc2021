#lang racket
(require net/uri-codec net/http-client)
(provide aoc-fetch-input
         aoc-fetch-challenge
         aoc-submit-answer
         aoc-complete?
         aoc-set-complete!)

; stolen from:
; https://git.lain.faith/haskal/aoc2020/src/branch/aoc2020/scripts/aoc-lib.rkt

(define *host* "adventofcode.com")
(define *status-file* ".status.rktd")

; generate API path
(define/contract (puzzle-path year day endpoint)
  (-> string? string? (or/c "input" "answer" false/c) path?)
  (define base (build-path "/" year "day" day))
  (if endpoint (build-path base endpoint) base))

; sets necessary headers for API
(define (make-headers session)
  (list (string-append "Cookie: session=" session)
        "Content-Type: application/x-www-form-urlencoded"))

; http request helper
(define (aoc-request year day endpoint session
                     [method 'GET] [data #f])
  (define (parse-headers hlist)
    (for/list ([h (in-list hlist)])
      (match h
        [(pregexp #px"^([^:]+): (.*?)$" (list _ k v))
         (cons (string->symbol (string-downcase (bytes->string/utf-8 k)))
               (bytes->string/utf-8 v))]
        [x (cons 'unknown x)])))
  (define (do-request path headers method data)
    (define-values (status headers-out content)
      (http-sendrecv *host* path
                     #:ssl? #t
                     #:headers headers
                     #:method method
                     #:data data))
    (define headers-out/parsed (parse-headers headers-out))
    (match status
      [(pregexp #px"^HTTP/1\\.[10] 200") content]
      [(pregexp #px"^HTTP/1\\.[10] 302")
       (define location (rest (or (assoc 'location headers-out/parsed)
                                  (error "got 302 with no location"))))
       (printf "got redirect to ~a\n" location)
       (close-input-port content)
       (do-request location headers 'GET #f)]
      [(pregexp #px"^HTTP/1\\.[10] 404")
       (error "endpoint returned 404.\n response: " (port->bytes content))]
      [stat
       (error "endpoint returned unexpected data.\n status: "
              stat
              "\n response: "
              (port->bytes content))]))
  (do-request (path->string (puzzle-path year day endpoint))
              (make-headers session)
              method data))

; gets the input file for a challenge
(define/contract (aoc-fetch-input year day session)
  (-> string? string? string? input-port?)
  (aoc-request year day "input" session))

; submits an answer to the server
(define/contract (aoc-submit-answer year day session part answer)
  (-> string? string? string? (or/c 1 2 "1" "2") string? (or/c symbol? bytes?))
  (define data `((level  . ,(~a part))
                 (answer . ,answer)))
  (define resp
    (port->bytes (aoc-request year day "answer" session 'POST
                              (alist->form-urlencoded data))))
  (match resp
    [(pregexp #px"That's the right answer")
     (aoc-set-complete! day part)
     'answer-correct]
    [(pregexp #px"That's not the right answer")
     'answer-incorrect]
    [(pregexp #px"You gave an answer too recently")
     'rate-limited]
    [(pregexp #px"Did you already complete it?")
     (aoc-set-complete! day part)
     'already-completed]
    [x x]))

; fetches the HTML page for a challenge
(define/contract (aoc-fetch-challenge year day session)
  (-> string? string? string? input-port?)
  (aoc-request year day #f session))

; generates entries for the challenge status file
(define (day+part->key day part)
  (when (string? day)
    (set! day (string->number day)))
  (when (string? part)
    (set! part (string->number part)))
  (cons day part))

; gets the status file
(define (aoc-get-status)
  (cond [(file-exists? *status-file*)
         (call-with-input-file *status-file* read)]
        [else '()]))

; is a challenge complete already?
(define (aoc-complete? day part)
  (set-member? (aoc-get-status) (day+part->key day part)))

; marks a challenge as completed
(define (aoc-set-complete! day part)
  (define status (set-add (aoc-get-status) (day+part->key day part)))
  (call-with-output-file *status-file*
    (lambda (prt)
      (write status prt))
    #:mode 'binary
    #:exists 'replace))
