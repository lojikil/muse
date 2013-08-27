#!/usr/bin/env vesta
(define (file->string name)
    "borrowed from Racket; takes a file name, and returns the file as lines."
    (with-exception-handler
        (lambda () "") ;; return an empty string on exception
        (lambda ()
            (let* ((fh (open name :read))
                   (data (string-join (read-lines fh) "\n")))
                (close fh)
                data))))

(define (file->lines name)
    "borrowed from Racket; takes a file name, and returns the file as lines."
    (with-exception-handler
        (lambda () "") ;; return an empty string on exception
        (lambda ()
            (let* ((fh (open name :read))
                   (data (read-lines fh)))
                (close fh)
                data))))

(define (file->object name)
    "same as the above, but READs an object from file"
    (with-exception-handler
        (lambda () "") ;; return an empty string on exception
        (lambda ()
            (let* ((fh (open name :read))
                   (data (read fh)))
                (close fh)
                data))))

(define (news-name->html-name n output)
    "takes a filename, removes the extension (if any) and appends .html"
    (let ((arr (string-tokenize-char n #\.)))
        (string-append
            output
            (string-join
                (cslice
                    arr
                    0
                    (- (length arr) 1))
                ".")
            ".html")))

(define (news->html-file n)
    "process a news name to a file, and open a port to it"
    (let ((name (news-name->html-name n)))
        (open name :write)))

(define (apply-template tmpl ctx)
    tmpl)

(define (add-news n env)
    (let ((header (file->string (nth env "header")))
          (footer (file->string (nth env "footer")))
          (news (file->string n))
          (output (news->html-file n (nth env "output-directory"))))
        (display
            (string-join (list header news footer) "\n")
            output)
        (close output)))
        
(let ((ac (length *command-line*))
      (env? (sys/stat "./config.ss"))
      (env {"header" "./header.html" "footer" "./footer.html" "output" ""}))
    (if (vector? env?) ;; can we find config.ss?
        (dict-merge env (file->object "./config.ss"))
        #v)
    (cond 
        (= ac 2)
            (add-news (nth *command-line* 1))
        else
            (begin
                (display "usage: muse.ss <file.html>\n")
                (quit 0))))
