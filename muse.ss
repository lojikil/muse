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
    (let ((name (news-name->html-name n "")))
        (open name :write)))

(define *close-tags* 
  ["</p>"
    "</p>"
    "</h1>"
    "</h2>"
    "</h3>"])

(define (single-line-state? s)
    (or (= s 2) (= s 3) (= s 4) #f))
   
(define (apply-template tmpl ctx out (state 0) (index 0) (stack '()))
    "simple templating system; supports a Muse-like (http://mwolson.org/projects/EmacsMuse.html)
     format. I had originally based my work on a simplified Org-mode, which I, ironically, called
     muse, not realizing that Emacs *already* had a system called 'Muse' which was similar to what
     I was working on. What's here is pretty basic: hyperlinks, images, tables, headers, numerical
     and unordered liss, code escapes and text styling. Enough to get a basic notes system up and 
     working. Eventually (read: never), I'd like to get this integrated into pandoc, but I'll 
     probably stick to converting Muse (my muse that is) to Markdown & just generating Word/PDF
     documents from that."
    #;(display (format "index == ~a and tmp[index] == ~a and state == ~a~%" index (nth tmpl index) state))
    (cond
        (>= index (length tmpl)) ;; should probably close all open tags here...
            (newline out)
        (= state 0)
            (cond
                (eq? (nth tmpl index) #\*)
                    (apply-template tmpl ctx out 2 (+ index 1) stack) ;; header
                (eq? (nth tmpl index) #\#) ;; numerical list
                    #f ;; push down current state?
                (eq? (nth tmpl index) #\-) ;; 
                    #f
                else
                    (apply-template tmpl ctx out 1 index stack))
        (= state 1)
            (cond 
                (eq? (nth tmpl index) #\*) ;; bold 
                    #f
                (eq? (nth tmpl index) #\[) ;; URL or Image
                    #f
                (eq? (nth tmpl index) #\`) ;; Code
                    (begin
                        (display "<code>" out)
                        (apply-template tmpl ctx out 8 (+ index 1) (cons 8 stack)))
                (eq? (nth tmpl index) #\{) ;; table
                    #f
                (eq? (nth tmpl index) #\\) ;; escape code
                    (begin
                        (display (nth tmpl (+ index 1)) out)
                        (apply-teplate tmpl ctx out 1 (+ index 2) stack))
                (eq? (nth tmpl index) #\<) ;; entity reference
                    (begin
                        (display "&lt;" out)
                        (apply-template tmpl ctx out 1 (+ index 1) stack))
                (eq? (nth tmpl index) #\>)
                    (begin
                        (display "&gt;" out)
                        (apply-template tmpl ctx out 1 (+ index 1) stack))
                (eq? (nth tmpl index) #\&) ;; probably should be smart, in case someone does "&copy;" or the like...
                    #f
                (eq? (nth tmpl index) #\newline) ;; paragraph closer!
                    (begin
                        (cond
                            (single-line-state? (car stack))
                                (display (nth *close-tags* (car stack)) out)
                            (eq? (nth tmpl (+ index 1)) #\newline)
                                (display "<br>" out)
                            else
                                #v)
                        (newline out)
                        (apply-template tmpl ctx out 0 (+ index 1) (cdr stack)))
                else ;; just emit & move back to state = 1, unless newline...
                    (begin
                        (display (nth tmpl index) out)
                        (apply-template tmpl ctx out 1 (+ index 1) stack)))
        (= state 2)
            (if (eq? (nth tmpl index) #\*)
                (apply-template tmpl ctx out 3 (+ index 1) stack)
                (begin
                    (display "<h1>" out)
                    (apply-template tmpl ctx out 1 index (cons 2 stack))))
        (= state 3)
            (if (eq? (nth tmpl index) #\*)
                (apply-template tmpl ctx out 4 (+ index 1) stack)
                (begin
                    (display "<h2>" out)
                    (apply-template tmpl ctx out 1 index (cons 3 stack))))
        (= state 4)
            (begin
                (display "<h3>" out)
                (apply-template tmpl ctx out 1 index (cons 4 stack)))
        (= state 8)
            (cond
                (eq? (nth tmpl index) #\`)
                    (begin
                        (display "</code>" out)
                        (apply-template tmpl ctx out 1 (+ index 1) (cdr stack)))
                (eq? (nth tmpl index) #\newline)
                    (begin
                        (display "<br>" out)
                        (apply-template tmpl ctx out 8 (+ index 1) stack))
                (eq? (nth tmpl index) #\\)
                    (begin
                        (display (nth tmpl (+ index 1)) out)
                        (apply-template tmpl ctx out 8 (+ index 2) stack))
                else
                    (begin
                        (display (nth tmpl index) out)
                        (apply-template tmpl ctx out 8 (+ index 1) stack)))))

(define (add-news n env)
    (let ((header (file->string (nth env "header")))
          (footer (file->string (nth env "footer")))
          (news (file->string n))
          (output (news->html-file n (nth env "output-directory"))))
        (display header output)
        (newline output)
        (apply-template news env output)
        (newline output)
        (display footer output) ;; woah! why wasn't that complaining about out!?!?!
        (newline output)
        (close output)))
        
(let ((ac (length *command-line*))
      (env? (sys/stat "./config.ss"))
      (env {"header" "./header.html" "footer" "./footer.html" "output-directory" ""}))
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
