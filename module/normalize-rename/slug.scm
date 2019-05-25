(define-module (normalize-rename slug)
  #:export     (slug))

(define (char-alphanumeric? char)
  "True if CHAR is a letter or a digit."
  (char-set-contains? char-set:letter+digit char))

(define (char-not-alphanumeric? char)
  "True if CHAR is neither a letter nor a digit."
  (not (char-alphanumeric? char)))

(define (string-delete-non-alphanumeric str)
  "Delete non-alphanumeric characters."
  (string-filter char-alphanumeric? str))

(define %slug
  (compose (lambda (lst)
             (string-join lst "-"))
           (lambda (lst)
             (map (compose string-delete-non-alphanumeric
                           string-normalize-nfkd
                           string-downcase)
                  lst))
           (lambda (lst)
             (filter (negate string-null?) lst))
           (lambda (str)
             (string-split str char-not-alphanumeric?))))

(define* (slug str #:optional beg end)
  "Return a slug."
  (%slug (if beg
             (if end
                 (substring str beg end)
                 (substring str beg))
             str)))

