
;; This module analysis wav file format,
;; and generate series of sequence sound data.

(use srfi-42)
(use gauche.sequence)

(debug-print-width 1000)

(define =: set!)

(define (mean xs) (/ (apply + xs) (length xs)))

(define (read-nbyte port n)
  (list-ec (: i 0 n) (read-byte port)))

(define-method out ((port <port>) (xs <string>))
  (display xs port))

(define-method out ((port <port>) (xs <pair>))
  (do-ec (: x xs)
         (write-byte x port)))

(define (make-output filename)
  (^ (stream)
     (call-with-output-file
         filename
       (^ (port)
          (out port stream)))))

(define (make-std-output)
  (^ (stream) (out (standard-output-port) stream)))

(define (make-std-error)
  (^ (stream) (out (standard-error-port) stream)))

(define-macro (while-list-ec var end? => binded-name . main)
  (let ([value (if (null? main) binded-name main)])
    `(let ([result '()])
       (while ,var ,end? => ,binded-name
              (set! result (cons ,value result)))
       (reverse result))))

(define (file->lines filename)
  (call-with-input-file
      filename
    (^ (port)
       (while-list-ec (read-line port) (.$ not eof-object?) => line))))

(define (file->bytes filename)
  (call-with-input-file
      filename
      (^ (port)
         (while-list-ec (read-byte port) (.$ not eof-object?) => char))))

(define (make-stream xs)
  (define state xs)
  (^ (:optional (n 1))
     (let ([ys (if (< (length state) n)
                   (append state (make-list (- n (length state)) (eof-object)))
                   state)])
         (receive (head tail) (split-at ys n)
                  (=: state tail)
                  head))))

(define (file->stream filename)
  (define port (open-input-file filename))
  (^ (n)
     (read-nbyte port n)))

(define (id x) x)

;; low bandpass filter
(define (moving-average items n)
  (let* ([sum (make-list n 0)])
    (list-ec (: i 0 (length items))
             ;; v may be over #xffffffff...
             (let ([v (x->integer (mean sum))]) 
               (=: sum (append (cdr sum) (list (car items))))
               (=: items (cdr items))
               v))))

(define bytes->string
  (.$ list->string (map$ integer->char)))

(define zero? (pa$ eqv? 0))

(define (bytes->uint xs)
  ((apply$ +)
   (map-with-index (^ (i x) (* (ash 1 (* 8 i)) x)) (reverse xs))))

(define (bytes->int xs :optional (bytes-length 4))
  (let* ([v (bytes->uint xs)]
         [len (- (* bytes-length 8) 1)]
         [M (ash 1 len)])
    (if (eqv? (logand (ash v (- len)) 1) 1)
        (+ (- M) (logxor M v))
        v)))

(define (uint->bytes value :optional (bytes-length 4))
    (reverse
     (list-ec (: i 0 bytes-length)
              (let ([v value])
                (begin (=: value (quotient value 256))
                       (if (zero? v) 0 (modulo v 256)))))))

(define (int->bytes value :optional (bytes-length 4))
  (let ([mask (- (ash 1 (* bytes-length 8)) 1)])
    (uint->bytes (logand mask value) bytes-length)))

(define (string->bytes value :optional len)
  (map char->integer value))

;; these function is identity.
;; ((.$ bytes->int (cut int->bytes <>)) -28)
;; ((.$ bytes->uint (cut uint->bytes <>)) 29834)
;; ((.$ bytes->string (cut string->bytes <>)) "RIFF")

(define-method ~ ((xs <pair>) (i <integer>) :optional default)
  (if (<= (length xs) i) default
      (ref xs i)))

(define (endian-order :optional (key :little))
  (cond [(eqv? key :big) (^ (x) x)]
        [(eqv? key :little) (^ (x) (reverse x))]
        [else (error "unknown endian type.")]))

(define (decoder type)
  (cond [(eqv? type :uint) bytes->uint]
        [(eqv? type :int) bytes->int]
        [(eqv? type :string) bytes->string]
        [else (error #`"unknown decode type: ,|type|")]))

(define (encoder type)
  (cond [(eqv? type :uint) uint->bytes]
        [(eqv? type :int) int->bytes]
        [(eqv? type :string) string->bytes]
        [else (error #`"unknonw type: ,|type|")]))

(define (normalize-specs-model specs-model)
  ((cut map <> specs-model)
   (^ (line)
      (let* ([name (~ line 0)]
             [len (~ line 1 4)]
             [endian (~ line 2 :little)]
             [type (~ line 3 :uint)])
        `(,name (name . ,name)
                (len . ,len)
                (type . ,type)
                (endian . ,endian))))))

;; default: little endian and uint
(define wav-specs-model
  (normalize-specs-model
  `([chunk-id 4 :big :string] ;; start "RIFF" chunk
    [chunk-size 4]
    [format 4 :big :string]
    [sub-chunk-1-id 4 :big :string] ;; start "fmt" sub chunk
    [sub-chunk-1-size 4]
    [audio-format 2]
    [num-channels 2]
    [sample-rate 4]
    [byte-rate 4]
    [block-align 2]
    [bits-per-sample 2]
    [sub-chunk-2-id 4 :big :string] ;; start "data" sub chunk
    [sub-chunk-2-size 4])))

(define-method ~ ((pair <pair>) (sym <symbol>))
  (assoc-ref pair sym))

(define (string->type type)
  (cond [(or (eqv? type :uint) (eqv? type :int)) x->integer]
        [(eqv? type :string) id]
        [else (error #`"unknown type: ,|type|")]))

(define (read-specs stream specs-model)
  ((cut map <> specs-model)
   (^ (spec)
      (let ([value 
             ((decoder (~ spec 'type))
              ((endian-order (~ spec 'endian))
               (stream (~ spec 'len))))])
        (append spec `((value . ,value)))))))

(define (lines->specs lines specs-model)
  ((cut map <> lines)
   (^ (item)
      (let* ([key (string->symbol (~ item 0))]
             [spec (assv key specs-model)]
             [value ((string->type (~ spec 'type)) (~ item 1))])
        (if spec (append spec `((value . ,value)))
            (error #`"unknown meta data: ,|key| ,|value|"))))))

(define (encode-spec spec)
  (let ([type (~ spec 'type)]
        [value (~ spec 'value)]
        [len (~ spec 'len)]
        [endian (~ spec 'endian)])
  ((endian-order endian) ((encoder type) value len))))

(define read-wav-specs (cut read-specs <> wav-specs-model))

(define (run-encode-specs specs)
  ((append-map$ id)
   (reverse (map (^ (spec) (encode-spec spec)) specs))))

(define add-comment-tag (^ (str) #`"# ,|str|"))

(define (mask n) (- (ash 1 n) 1))

(define (transpose items)
  (apply map (^ x x) items))

(define (show items)
  (do-ec (: item items)
         (print
          ((cut string-join <> " ")
           (list-ec (: v item) (number->string v))))))

(define (is-spec? x) (eqv? (~ x 0) #\#))

(define (append-map$ fun) (^ (items) (append-map fun items)))

(define (quat x)
  (integer->char (if (< x 10) (+ x 48) (+ x 87))))

(define (byte->hex x)
  (string (quat (quotient x 16)) (quat (modulo x 16))))

(define (specs->comments specs :optional (add-suffix id))
  ($ (cut string-append <> "\n") $
     (cut string-join <> "\n") $
     (cut map <> specs)
     (^ (spec) (add-suffix #`",(~ spec 'name):,(~ spec 'value)"))))

(define string->stream (.$ (map$ char->integer) string->list))

(define (separate-specs lines)
  (let ([xs '()])
    (while
     (car lines) is-spec? => line
     (begin
       (=: xs (cons (string-split (substring  line 2 -1) ":") xs))
       (=: lines (cdr lines))))
    (values (lines->specs xs wav-specs-model) lines)))

(define (show-specs stream)
  (display (specs->comments (read-wav-specs stream))))

;; items is stored in little endian form.
(define (fmt nbyte nbyte->number channels)
   (map (.$ nbyte->number reverse)
        (slices nbyte (/ (length nbyte) channels))))

(define (run-decode-main specs stream :optional (hook id))
  (define (value x) (~ (~ specs x) 'value))
  (let* ([bits-per-sample (value 'bits-per-sample)]
         [channels (value 'num-channels)]
         [mask (- (ash 1 bits-per-sample) 1)]
         [block (value 'block-align)]
         [nbyte->number (cut bytes->int <> (/ block channels))]
         [ret '()])
    (while (stream block) (.$ not eof-object? car) => nbyte
           (=: ret (cons (fmt nbyte nbyte->number channels) ret)))
    ($ transpose $ (map$ hook) $ transpose (reverse ret))))

(define (run-decode stream :optional (hook id))
  (let ([specs (read-wav-specs stream)])
    (string-append
     (specs->comments specs add-comment-tag)
     ($ (cut string-join <> "\n") $
        (map$ (.$ (cut string-join <> " ") (map$ number->string)))
        (run-decode-main specs stream hook)))))

(define (run-encode-main specs lines)
  (define (value x) (~ (~ specs x) 'value))
  (let ([ret '()]
        [bytes-length (/ (value 'block-align) (value 'num-channels))])
    ($ (cut append-map id <>) $
       (cut map <> lines)
       (^ (line)
          (append-map (.$ reverse
                          (cut int->bytes <> bytes-length) x->integer)
                      (string-split line " "))))))

;; content is string
(define (run-encode lines)
  (receive (specs lines) (separate-specs lines)
           (append
            (run-encode-specs specs)
            (run-encode-main specs lines))))

;; (define specs (separate-specs (file->lines "a.txt")))

((make-std-output) (run-decode (file->stream "music_A.wav")))

;; ((make-output "hoge.wav") (run-encode (file->lines "out.txt")))

;; (show-specs (file->stream "a.wav"))



