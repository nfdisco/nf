(define-module (nf nf))

(export print-error progname rename)

(use-modules (nf slug))

(use-modules
 (ice-9 format)
 (srfi srfi-11))

(define progname (make-parameter "nf"))

(define (print-error msg . rest)
  "Write message to the standard error."
  (format (current-error-port) "~a: ~?~%" (progname) msg rest))

(define (file-name-split str num-ext)
  "Split file name into a base name plus zero or more extensions."
  (let* ((parts (string-split str #\.))
         (m (length parts))
         (n (if (< num-ext 0)
                m
                num-ext))
         (k (do ((i (1- m) (1- i))
                 (c 1 (1+ c)))
                ((or (< i 0)
                     (> c n)
                     (string-null? (list-ref parts i))
                     (and (= i 1)
                          (string-null? (list-ref parts 0))))
                 (1+ i)))))
    (if (< k 1) parts
        (append (list (string-join (list-head parts k) "."))
                (list-tail parts k)))))

(define (file-name-slug str num-ext)
  "Normalize a file name string, which must not contain slashes, be empty,
or consist of \"..\" or \".\"."
  (let ((filename
         (string-join (map string-slug (file-name-split str num-ext))
                      ".")))
    (if (char=? (string-ref str 0) #\.)
        (string-append "." filename)
        filename)))

(define (path-with-normalized-name path-name num-ext)
  "Return path name, with normalized file name.  The file name component of
the path name must not be \".\", \"..\" or be empty."
  (let* ((old-file-name (basename path-name))
         (new-file-name (file-name-slug old-file-name num-ext)))
    (string-append (dirname path-name)
                   file-name-separator-string
                   new-file-name)))

(define (get-valid-path-names lst)
  "Split list into lists of valid and invalid pathnames."
  (let iter ((tail lst)
             (valid '())
             (invalid '()))
    (if (null? tail)
        (values (reverse valid) invalid)
        (let* ((path-name-literal (car tail))
               (path-name (string-trim-right path-name-literal #\/))
               (file-name (basename path-name)))
          (cond
           ((string-null? path-name-literal)
            (print-error "ignoring empty pathname: ~S" path-name-literal)
            (iter (cdr tail)
                  valid
                  (cons path-name-literal invalid)))
           ((string-null? path-name)
            (print-error "ignoring root directory: ~S" path-name-literal)
            (iter (cdr tail)
                  valid
                  (cons path-name-literal invalid)))
           ((or (string=? file-name ".")
                (string=? file-name ".."))
            (print-error "ignoring: ~S" path-name-literal)
            (iter (cdr tail)
                  valid
                  (cons path-name-literal invalid)))
           ((member path-name valid)
            (print-error "ignoring repeated argument: ~S" path-name-literal)
            (iter (cdr tail)
                  valid
                  (cons path-name-literal invalid)))
           (else
            (iter (cdr tail)
                  (cons path-name valid)
                  invalid)))))))

(define (path-name-pairs lst num-ext)
  "Split list of path names into path name pairs, pairs with equal source
and destination, pairs with conflicting destinations and invalid source
pathnames."
  (let-values (((path-names fail-old)
                (get-valid-path-names lst)))
    (let iter ((tail path-names)
               (pairs '())
               (pairs-equal '())
               (fail '()))
      (if (null? tail)
          (values (reverse pairs) pairs-equal fail fail-old)
          (let* ((old (car tail))
                 (new (path-with-normalized-name old num-ext)))
            (cond
             ((string=? (basename new) (basename old))
              (iter (cdr tail)
                    pairs
                    (cons old pairs-equal)
                    fail))
             ((assoc new fail)
              (print-error "conflicting file name: ~S" old)
              (iter (cdr tail)
                    pairs
                    pairs-equal
                    (acons new old fail)))
             ((assoc new pairs) =>
              (lambda (prev)
                (print-error "conflicting file name: ~S" (cdr prev))
                (print-error "conflicting file name: ~S" old)
                (iter (cdr tail)
                      (assoc-remove! pairs new)
                      pairs-equal
                      (acons new old (cons prev fail)))))
             (else
              (iter (cdr tail)
                    (acons new old pairs)
                    pairs-equal
                    fail))))))))

(define (display-table data align)
  "Write data to the standard output."
  (let ((format-string
         (string-join
          (map (lambda (minw mod)
                 (format #f "~~~d~aa" minw mod))
               (apply map (cons
                           (lambda ( . args)
                             (apply max (map string-length args)))
                           data))
               (map (lambda (char)
                      (case char
                        ((#\l) "")
                        ((#\r) "@")
                        (else
                         (throw 'invalid-align-character
                                char))))
                    (string->list align)))
          " ")))
    (map (lambda (row)
           (format #t "~?~%" format-string row))
         data)
    #t))

(define (display-report renamed unchanged . failed)
  "Display summary to the standard output."
  (display-table
   (filter identity
           (map (lambda (elt)
                  (let ((label (car elt))
                        (count (cadr elt)))
                    (if (> count 0)
                        (list label
                              (format #f "~a" count)
                              (format #f "file~p" count))
                        #f)))
                `(("Renamed:" ,(length renamed))
                  ("Unchanged:" ,(length unchanged))
                  ("Failed:" ,(apply + (map length failed))))))
   "lrl"))

(define (try-rename old new rename?)
  "Try to rename a file without overwriting any existing files.  Return #f
if file could not be renamed."
  (if (file-exists? new)
      (begin
        (print-error
         (string-append "try-rename: " (strerror EEXIST) ": ~S") new)
        #f)
      (if rename?
          (catch 'system-error
            (lambda () (rename-file old new))
            (lambda (error proc msg args . rest)
              (apply print-error
                     (string-append "~A: " msg ": ~S")
                     (cons proc (append args (list old))))
              #f))
          (format #t "~S: ~S~%" old new))))

(define (rename lst num-ext rename?)
  "Rename files.  Return 0 if no errors occurred, > 0 otherwise."
  (let-values (((pairs pairs-equal fail-dest fail-src)
                (path-name-pairs lst num-ext)))
    (let iter ((tail pairs)
               (success '())
               (failure '()))
      (if (null? tail)
          (begin
            (display-report success pairs-equal fail-src fail-dest failure)
            (+ (length fail-src) (length fail-dest) (length failure)))
          (let* ((elt (car tail)))
            (if (try-rename (cdr elt) (car elt) rename?)
                (iter (cdr tail) (cons elt success) failure)
                (iter (cdr tail) success (cons elt failure))))))))

;;
;; LOCAL Variables:
;; mode: scheme
;; geiser-scheme-implementation: guile
;; End:
