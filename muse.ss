(define (file->string name)
    "borrowed from Racket; takes a file name, and returns the file as lines."
    (with-exception-handler
        (lambda () "") ;; return an empty string on exception
        (lambda ()
            (let* ((fh (open name :read))
                   (data (string-join (read-lines fh) "\n")))
                (close fh)
                data))))

(define (news-name->html-name n)
    "takes a filename, removes the extension (if any) and appends .html"
    (let ((arr (string-tokenize-char n #\.)))
        (string-append
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

(define (add-news n)
    (let ((header (file->string "./head.html"))
          (footer (file->string "./tail.html"))
          (news (file->string n))
          (output (news->html-file n)))
        (display
            (string-join (list header news footer) "\n")
            output)
        (close output)))
        
(let ((ac (length *command-line*)))
    (cond 
        (= ac 2)
            (add-news (nth *command-line* 1))
        else
            (begin
                (display "usage: muse.ss <file.html>\n")
                (quit 0))))
