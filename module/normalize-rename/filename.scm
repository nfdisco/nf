(define-module (normalize-rename filename)
  #:use-module (normalize-rename slug)
  #:export     (filename-normalize delete-filename-separator-right))

(define (filename-error msg . args)
  "Signal a filename error."
  (scm-error 'filename-error #f msg args #f))

(define (delete-filename-separator-right str)
  "Delete filename separators from string."
  (string-trim-right str file-name-separator?))

(define (filename-valid? filename)
  "True if FILENAME is a valid filename."
  (let ((fn (delete-filename-separator-right (basename filename))))
    (not (or (string-null? fn)
             (string=? fn ".")
             (string=? fn "..")
             (string-any #\/ fn)
             (string-any #\nul fn)))))

(define (%filename-split filename)
  "Split FILENAME into directory part, basename and zero or more
extensions."
  (define (iter str parts)
    (let ((i (string-rindex (string-trim-right str #\.) #\.)))
      (if (or (not i)
              (zero? i))
          (cons str parts)
          (iter (substring str 0 i)
                (cons (substring str (1+ i)) parts)))))
  (cons (dirname filename)
        (iter (delete-filename-separator-right (basename filename))
              (list))))

(define (filename-split filename k)
  "Split FILENAME into directory part, basename and K or less extensions, or
zero or more extensions if K < 0."
  (let* ((parts (%filename-split filename))
         (i (if (< k 0) 2 (max (- (length parts) k) 2))))
    (cons (car parts)
          (cons (string-join (cdr (list-head parts i)) ".")
                (list-tail parts i)))))

(define (filename-normalize filename k)
  "Normalize FILENAME.  Signal filename-error if for whatever reason
FILENAME cannot be normalized."
  (let* ((parts (filename-split filename k))
         (dir (car parts))
         (base (cadr parts)))
    (when (string-null? base)
          (filename-error "defective filename: empty string"))
    (let ((fn (string-join (cons (if (char=? (string-ref base 0) #\.)
                                     (string-append "." (slug base 1))
                                     (slug base))
                                 (map slug (cddr parts)))
                           ".")))
      (when (not (filename-valid? fn))
            (filename-error "defective filename: ~s" filename))
      (if (string=? dir ".")
          fn
          (string-append dir
                         file-name-separator-string
                         fn)))))

