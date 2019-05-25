(define-module (normalize-rename rename)
  #:use-module (normalize-rename message)
  #:use-module (normalize-rename filename)
  #:use-module (ice-9 format)
  #:export (normalize-rename verbosity test-run))


(define verbosity (make-parameter 1))

(define test-run (make-parameter #f))

(define (rename-error msg . args)
  "Signal a rename error."
  (scm-error 'rename-error #f msg args #f))

(define (basename=? a b)
  "True if filenames A and B are equal, basename wise."
  (string=? (delete-filename-separator-right (basename a))
            (delete-filename-separator-right (basename b))))

(define (count lst)
  "Return an alist with the number of occurrences of each element in LST."
  (define (iter lst tally)
    (cond ((null? lst)
           tally)
          (else
           (let* ((key (car lst))
                  (value (assq-ref tally key)))
             (iter (cdr lst)
                   (if value
                       (assq-set! tally key (1+ value))
                       (acons key 1 tally)))))))
  (iter lst '()))

(define (%normalize-rename orig k)
  "Rename file ORIG to a normalized file name.  Return the symbol renamed if
the file was renamed successfully, skipped if the name was already
normalized, or failure if an error occurred."
  (catch #t
         (lambda ()
           (let ((dest (filename-normalize orig k)))
             (cond ((basename=? orig dest)
                    (when (> (verbosity) 1)
                          (format #t "skipping ~s\n" orig))
                    'skipped)
                   ((file-exists? dest)
                    (msg-error "destination file exists: ~s" dest)
                    'failure)
                   (else
                    (when (> (verbosity) 1)
                          (format #t "renaming ~s to ~s\n" orig dest))
                    (when (not (test-run))
                          (rename-file orig dest))
                    'renamed))))
         (lambda (key . rest)
           (case key
             ((filename-error)
              (apply msg-error (cadr rest) (caddr rest)))
             ((system-error)
              (msg-error "cannot rename ~s: ~a"
                         orig
                         (strerror (car (cadddr rest)))))
             (else
              (msg-error "unkown error while processing: ~s" orig)))
           'failure)))

(define (normalize-rename lst k)
  "Rename files to normalized filenames and print a report."
  (let ((status (count (map (lambda (fn) (%normalize-rename fn k)) lst))))
    (when (and (not (zero? (verbosity)))
               (not (zero? (length status))))
          (let ((renamed (assq-ref status 'renamed))
                (skipped (assq-ref status 'skipped))
                (failure (assq-ref status 'failure)))
            (when (or (> (verbosity) 1)
                      failure)
                  (newline))
            (when renamed
                  (format #t "renamed: ~6d\n" renamed))
            (when skipped
                  (format #t "skipped: ~6d\n" skipped))
            (when failure
                  (format #t "errors:  ~6d\n" failure))))
    (not (assq 'failure status))))
