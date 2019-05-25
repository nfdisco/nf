(define-module (normalize-rename cli)
  #:use-module (normalize-rename program)
  #:use-module (normalize-rename message)
  #:use-module (normalize-rename rename)
  #:use-module (ice-9 getopt-long)
  #:export     (main))

(define (display-help)
  "Print help message."
  (format #t "\
Usage: ~a [OPTIONS] [--] [FILE [FILE...]]
Rename files to normalized filenames.

Options:
     -e, --extensions=N  number of extensions in file names (default=0)
     -t, --test-run      show what would be done, without doing it
     -v, --verbose       increase verbosity level
     -q, --quiet         decrease verbosity level

     -h, --help          show this help and exit
         --version       show version and exit
" (program-name)))

(define (display-version)
  "Print version string."
  (format #t "~a version ~a\n" (program-name) normalize-rename-version))

(define option-spec
  `((help        (single-char #\h) (value #f))
    (version                       (value #f))
    (extenstions (single-char #\e) (value #t))
    (test-run    (single-char #\t) (value #f))
    (verbose     (single-char #\v) (value #f))
    (quiet       (single-char #\q) (value #f))))

(define (integer-arg arg)
  "Return an integer or #f if ARG is not an integer argument."
  (let ((x (string->number arg)))
    (and (integer? x)
         x)))

(define (%main args)
  "Process command-line arguments."
  (let* ((options (getopt-long args option-spec))
         (arg-n (option-ref options 'extenstions #f))
         (k (if arg-n (integer-arg arg-n) 0))
         (arg-quiet (option-ref options 'quiet #f))
         (arg-verbose (option-ref options 'verbose #f))
         (arg-test-run (option-ref options 'test-run #f))
         (file-list (option-ref options '() #f)))
    (when (option-ref options 'help #f)
          (display-help)
          (exit 0))
    (when (option-ref options 'version #f)
          (display-version)
          (exit 0))
    (when (not k)
          (msg-error "integer argument required: ~a" arg-n)
          (exit 1))
    (when (null? file-list)
          (msg-error "non-option argument required.")
          (exit 1))
    (parameterize ((test-run arg-test-run)
                   (verbosity (cond ((or arg-verbose arg-test-run) 2)
                                    (arg-quiet 0)
                                    (else 1))))
                  (exit (normalize-rename file-list k)))))

(define (main args)
  (parameterize ((program-name (basename (car args))))
                (%main args)))

