#!/usr/bin/env vesta
(define (file->string name)
    "borrowed from Racket; takes a file name, and returns the file as a string."
    (with-exception-handler
        (lambda (x) "") ;; return an empty string on exception
        (lambda ()
            (let* ((fh (open name :read))
                   (data (string-join (read-lines fh) "\n")))
                (close fh)
                data))))

(define (file->lines name)
    "borrowed from Racket; takes a file name, and returns the file as lines."
    (with-exception-handler
        (lambda (x) "") ;; return an empty string on exception
        (lambda ()
            (let* ((fh (open name :read))
                   (data (read-lines fh)))
                (close fh)
                data))))

(define (file->object name)
    "same as the above, but READs an object from file"
    (with-exception-handler
        (lambda (x) "") ;; return an empty string on exception
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

(define (apply-template tmpl ctx (state 0) (index 0) (stack '()))
    "simple templating system; supports a Muse-like (http://mwolson.org/projects/EmacsMuse.html)
     format. I had originally based my work on a simplified Org-mode, which I, ironically, called
     muse, not realizing that Emacs *already* had a system called 'Muse' which was similar to what
     I was working on. What's here is pretty basic: hyperlinks, images, tables, headers, numerical
     and unordered liss, code escapes and text styling. Enough to get a basic notes system up and 
     working. Eventually (read: never), I'd like to get this integrated into pandoc, but I'll 
     probably stick to converting Muse (my muse that is) to Markdown & just generating Word/PDF
     documents from that."
    (cond
        (= state 0)
            (if (eq? (nth tmpl index) #\*)
                (apply-template tmpl ctx 2 (+ index 1) stack) ;; header
                (apply-template tmpl ctx 1 index stack))
        (= state 1)
            (cond 
                (eq? (nth tmpl index) #\#) ;; numerical list
                    #f ;; push down current state?
                (eq? (nth tmpl index) #\-) ;; 
                    #f
                (eq? (nth tmpl index) #\*) ;; bold 
                    #f
                (eq? (nth tmpl index) #\[) ;; URL or Image
                    #f
                (eq? (nth tmpl index) #\`) ;; Code
                    #f
                (eq? (nth tmpl index) #\{) ;; table
                    #f
                (eq? (nth tmpl index) #\\) ;; escape code
                    #f
                (eq? (nth tmpl index) #\<) ;; entity reference
                    #f
                (eq? (nth tmpl index) #\>)
                    #f
                (eq? (nth tmpl index) #\&) ;; probably should be smart, in case someone does "&copy;" or the like...
                    #f
                (eq? (nth tmpl index) #\newline) ;; paragraph closer!
                    #f
                else ;; just emit & move back to state = 1, unless newline...
                    #f)
        (= state 2)
            #f
        (= state 3)
            #f
        (= state 4)
            #f))

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
            (add-news (nth *command-line* 1) env)
        else
            (begin
                (display "usage: muse.ss <file.html>\n")
                (quit 0))))
