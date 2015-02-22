
;; This module analysis wav file format,
;; and generate series of sequence sound data.

(use srfi-42)
(use gauche.sequence)

(debug-print-width 1000)

(define =: set!)

(define (read-nbyte port n)
  (list-ec (: i 0 n) (read-byte port)))

(define MSB (ash 1 31))
(define F #xffffffff)

(define (bytes->uint xs)
  ((apply$ +)
   (map-with-index (^ (i x) (* (ash 1 (* 8 i)) x)) (reverse xs))))

(define (bytes->int xs)
  (let ([v (bytes->uint xs)])
    (if (eqv? (logand (ash v -31) 1) 1)
        (+ (- MSB) (logxor MSB v))
        v)))

(define bytes->string
  (.$ list->string (map$ integer->char)))

(define zero? (pa$ eqv? 0))

(define (uint->bytes value len)
    (reverse
     (list-ec (: i 0 len)
              (let ([v value])
                (begin (=: value (quotient value 256))
                       (if (zero? v) 0 (modulo v 256)))))))

(define (int->bytes value len)
  (uint->bytes (logand F value) len))

(define (string->bytes value len)
  (take (map char->integer value) len))

;; these function is identity.
;; ((.$ bytes->int (cut int->bytes <> 4)) -28)
;; ((.$ bytes->uint (cut uint->bytes <> 4)) 29834)
;; ((.$ bytes->string (cut string->bytes <> 4)) "RIFF")

(define-method ~ ((xs <pair>) (i <integer>) :optional default)
  (if (<= (length xs) i) default
      (ref xs i)))

(define (key->endian :optional (key :little))
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
        [(eqv? type :int)]
        [(eqv? type :string) string->bytes]
        [else (error #`"unknonw type: ,|type|")]))

;; default: little endian and uint
(define wav-specs
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
    [sub-chunk-2-size 4]))

(define (with-spec spec fn)
  (let* ([name (~ spec 0)]
         [len (~ spec 1)]
         [endian (key->endian (~ spec 2 :little))]
         [type (~ spec 3 :uint)])
    (fn name len endian type)))

(define (string->type type)
  (cond [(or (eqv? type :uint) (eqv? type :int)) x->integer]
        [(eqv? type :string) id]
        [else (error #`"unknown type: ,|type|")]))

(define (encode-spec spec value)
  (with-spec
   spec
   (^ (name len endian type)
      (endian ((encoder type) ((string->type type) value) len)))))

(define (read-specs port specs)
  (if (null? specs) '()
      (with-spec
       (car specs)
       (^ (name len endian type)
          (cons `(,name . ,((decoder type) (endian (read-nbyte port len))))
                (read-specs port (cdr specs)))))))

(define (read-wav-specs port) (read-specs port wav-specs))

(define (id x) x)

(define (show-wav-specs specs :optional (add-suffix id))
  ($ (cut string-append <> "\n") $
     (cut string-join <> "\n")
     (list-ec (: spec specs)
            (format (add-suffix #`",(car spec):,(cdr spec)")))))

(define (call-with-wav-specs filename fun)
  (call-with-input-file filename
    (^ (port)
       (let ([specs (read-wav-specs port)])
         (fun port specs)))))

(define (show-info filename :optional (add-suffix id))
  (call-with-wav-specs
   filename
   (^ (port specs)
      (show-wav-specs specs add-suffix))))

(define add-comment-tag (^ (str) #`"# ,|str|"))

;; items is stored in little endian form.
(define (fmt items channel)
   (map (.$ bytes->int reverse)
        (slices items (/ (length items) channel))))

(define (transpose items)
  (apply map (^ x x) items))

(define (decode filename :optional (hook id))
  (call-with-wav-specs
   filename
   (^ (port specs)
      (let ([bits-per-sample (/ (assoc-ref specs 'bits-per-sample) 8)]
            [block-align (assoc-ref specs 'block-align)]
            [channels (assoc-ref specs 'num-channels)]
            [ret '()])
        (while (read-nbyte port block-align) (.$ not eof-object? car) => nbyte
               (=: ret (cons (fmt nbyte channels) ret)))
        ($ transpose $ (map$ hook) $ transpose (reverse ret))))))

(define (show items)
  (do-ec (: item items)
         (print #`",(~ item 0) ,(~ item 1)")))

(define (is-comment? x) (eqv? (~ x 0) #\#))

(define (read-metadata port fn)
  (let ([ret '()])
    (while (read-line port) is-comment? => line
           (=: ret (cons (fn (string-split (substring  line 2 -1) ":")) ret)))
    (reverse ret)))

(define (append-map$ fun) (^ (items) (append-map fun items)))

(define (encode-metadata port specs)
  ((append-map$ id)
   (read-metadata
    port
    (^ (item)
       (let* ([key (string->symbol (~ item 0))]
              [value (~ item 1)]
              [spec (assv key specs)])
         (if spec (encode-spec spec value)
             (error #`"unknown meta data: ,|key| ,|value|")))))))

(define (encode-data port)
  (let ([ret '()])
    (while (read-line port) (.$ not eof-object?) => line
           (=: ret
               (cons (append-map (.$ reverse
                                     (cut uint->bytes <> 4) x->integer)
                                 (string-split line " ")) ret)))
    (append-map id (reverse ret))))

(define (quat x)
  (integer->char (if (< x 10) (+ x 48) (+ x 87))))

(define (byte->hex x)
  (string (quat (quotient x 16)) (quat (modulo x 16))))

(define (run-decode filename :optional (hook id))
  (display (show-info filename add-comment-tag))
  (show (decode filename hook)))

(define (run-encode in-filename out-filename)
  (call-with-input-file
      in-filename
    (^ (in-port)
       (call-with-output-file
           out-filename
         (^ (out-port)
            (let* ([cs (encode-metadata in-port wav-specs)]
                   [cs (append cs (encode-data in-port))])
              (do-ec (: c cs)
                     (write-byte c out-port))))))))

(define (mean xs) (/ (apply + xs) (length xs)))

;; low bandpass filter
(define (moving-average items n)
  (let* ([sum (make-list n 0)])
    (list-ec (: i 0 (length items))
             ;; v may be over #xffffffff...
             (let ([v (x->integer (mean sum))]) 
               (=: sum (append (cdr sum) (list (car items))))
               (=: items (cdr items))
               v))))

(run-decode "test.wavpcm" (cut moving-average <> 8))
;; (run-encode "a.txt" "out.wavpcm")



