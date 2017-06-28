(define-module (slug slug))

(export string-slug slug-filter)

(define (char-numeric? char)
  "True if `char` is a digit."
  (char-set-contains? char-set:digit char))

(define (char-alphabetic-or-numeric? char)
  "True if `char` is a letter or a digit."
  (char-set-contains? char-set:letter+digit char))

(define (char-decompose char)
  "Compatibility decomposition of `char` as a list of characters."
  (if (char-alphabetic? char)
      (let* ((str (string-normalize-nfkd (string char))))
        (if (string-null? str)
            (list char)
            (string->list str)))
      (list char)))

(define (string-simplify str)
  "Strip diacritical marks and decompose ligatures."
  (string-concatenate
   (map (lambda (char)
          (if (char-alphabetic? char)
              (string-filter char-alphabetic?
                             (string-normalize-nfkd (string char)))
              (string char)))
        (string->list str))))

;; Slightly faster than the version that uses slug-filter.
(define (string-slug str)
  "Return string in slug format."
  (let ((out str)
        (sep "-"))
    (when (not (string-null? str))
      (set! out (string-downcase out))
      (set! out
        (string-join
         (filter (negate string-null?)
                 (string-split
                  out (negate char-alphabetic-or-numeric?)))
         sep))
      (set! out (string-simplify out))
      (when (string-null? out)
        (set! out sep)))
    out))

(define (slug-filter)
  "Read textual data from the current input port and write a slug to the
current output port."
  (let iter ((char (read-char))
             (any? #f)
             (prev? #f))
    (when (not (eof-object? char))
      (let ((output-chars
             (cond
              ((char-numeric? char)
               (list char))
              ((char-alphabetic? char)
               (filter char-alphabetic?
                       (char-decompose (char-downcase char))))
              (#t #f))))
        (if (and output-chars
                 (not (null? output-chars)))
            (begin
              (when (and any?
                         (not prev?))
                (write-char #\-))
              (map write-char output-chars)
              (iter (read-char) #t #t))
            (iter (read-char) any? #f))))))

;;
;; LOCAL Variables:
;; mode: scheme
;; End:
