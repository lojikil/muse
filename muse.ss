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
    "</h3>"
    0
    0
    0
    0
    0
    "</ol>"
    "</li>"
    "</ul>"
    "</li>"])

(define (single-line-state? s)
    (or (= s 2) (= s 3) (= s 4) (= s 11) (= s 13) #f))

(define (grouped-line-state? s)
    (or (= s 10) (= s 12) #f))

(define (apply-backtick tmpl ctx out state index stack)
    (cond
        (eq? (nth tmpl index) #\`)
            (begin
                (if (= state 8)
                    (display "</code>" out)
                    (display "</pre>" out))
                (apply-template tmpl ctx out 0 (+ index 1) stack))
        (eq? (nth tmpl index) #\<)
            (begin
                (display "&lt;" out)
                (apply-backtick tmpl ctx out state (+ index 1) stack))
        (eq? (nth tmpl index) #\>)
            (begin
                (display "&gt;" out)
                (apply-backtick tmpl ctx out state (+ index 1) stack))
        (eq? (nth tmpl index) #\&)
            (begin
                (display "&amp;" out)
                (apply-backtick tmpl ctx out state (+ index 1) stack))
        (eq? (nth tmpl index) #\\)
            (begin
                (display (nth tmpl (+ index 1)) out)
                (apply-backtick tmpl ctx out state (+ index 2) stack))
        else
            (begin
                (display (nth tmpl index) out)
                (apply-backtick tmpl ctx out state (+ index 1) stack))))
   
(define (apply-template tmpl ctx out (state 0) (index 0) (stack '()))
    "simple templating system; supports a Muse-like (http://mwolson.org/projects/EmacsMuse.html)
     format. I had originally based my work on a simplified Org-mode, which I, ironically, called
     muse, not realizing that Emacs *already* had a system called 'Muse' which was similar to what
     I was working on. What's here is pretty basic: hyperlinks, images, tables, headers, numerical
     and unordered liss, code escapes and text styling. Enough to get a basic notes system up and 
     working. Eventually (read: never), I'd like to get this integrated into pandoc, but I'll 
     probably stick to converting Muse (my muse that is) to Markdown & just generating Word/PDF
     documents from that."
    #;(display (format "index == ~a and tmp[index] == ~A and state == ~a~%" index (nth tmpl index) state))
    #;(display (format "stack == ~A~%" stack))
    (cond
        (>= index (length tmpl)) ;; should probably close all open tags here...
            (newline out)
        (= state 0)
            (cond
                (eq? (nth tmpl index) #\*)
                    (apply-template tmpl ctx out 2 (+ index 1) stack) ;; header
                (eq? (nth tmpl index) #\`) ;; Longer code snippet
                    (begin
                        (display "<pre class=\"muse-code\">" out)
                        (apply-template tmpl ctx out 9 (+ index 1) stack))
                (eq? (nth tmpl index) #\#) ;; numerical list
                    ;; emit two states:
                    ;; - ordered-list-wrapper
                    ;; - list-item
                    ;; when a list item hits #\newline, state goes to 0, which
                    ;; then has to check if a 'ordered-list-wrapper' is already
                    ;; on the stack, and if not, emit a <ol>. Somewhat complex...
                    (if (not (eq? (car stack) 10))
                        (begin
                            (display "<ol>\n<li>" out)
                            (apply-template tmpl ctx out 1 (+ index 1) (append (list 11 10) stack)))
                        (begin
                            (display "<li>" out)
                            (apply-template tmpl ctx out 1 (+ index 1) (cons 11 stack))))
                (eq? (nth tmpl index) #\newline)
                    (if (grouped-line-state? (car stack))
                        (begin
                            (display (nth *close-tags* (car stack)) out)
                            (newline out)
                            (apply-template tmpl ctx out 0 (+ index 1) (cdr stack)))
                        (begin
                            (display "<br>\n" out)
                            (apply-template tmpl ctx out 0 (+ index 1) stack)))
                (eq? (nth tmpl index) #\-) ;; 
                    (if (not (eq? (car stack) 12))
                        (begin
                            (display "<ul>\n<li>" out)
                            (apply-template tmpl ctx out 1 (+ index 1) (append (list 13 12) stack)))
                        (begin
                            (display "<li>" out)
                            (apply-template tmpl ctx out 1 (+ index 1) (cons 13 stack))))
                else
                    (apply-template tmpl ctx out 1 index stack))
        (= state 1)
            (cond 
                (eq? (nth tmpl index) #\*) ;; bold 
                    (if (= (car stack) 5)
                        (begin
                            (display "</span>" out)
                            (apply-template tmpl ctx out 1 (+ index 1) (cdr stack)))
                        (begin
                            (display "<span class=\"muse-bold\">" out)
                            (apply-template tmpl ctx out 1 (+ index 1) (cons 5 stack))))
                (eq? (nth tmpl index) #\_) ;; italic 
                    (if (= (car stack) 6)
                        (begin
                            (display "</span>" out)
                            (apply-template tmpl ctx out 1 (+ index 1) (cdr stack)))
                        (begin
                            (display "<span class=\"muse-italic\">" out)
                            (apply-template tmpl ctx out 1 (+ index 1) (cons 6 stack))))
                (eq? (nth tmpl index) #\[) ;; URL or Image
                    (if (eq? (nth tmpl (+ index 1)) #\!)
                        (begin
                            (display "<img src=\"" out)
                            (apply-template tmpl ctx out 15 (+ index 2) stack))
                        (begin
                            (display "<a href=\"" out)
                            (apply-template tmpl ctx out 14 (+ index 1) stack)))
                (eq? (nth tmpl index) #\]) ;; close URL or Image
                    (begin
                        (if (eq? (car stack) 14)
                            (display "</a>" out)
                            (display "\">" out))
                        (apply-template tmpl ctx out 1 (+ index 1) (cdr stack)))
                (eq? (nth tmpl index) #\`) ;; Code
                    (begin
                        (display "<code class=\"muse-code\">" out)
                        (apply-template tmpl ctx out 8 (+ index 1) stack))
                (eq? (nth tmpl index) #\{) ;; table
                    #f
                (eq? (nth tmpl index) #\\) ;; escape code
                    (begin
                        (display (nth tmpl (+ index 1)) out)
                        (apply-template tmpl ctx out 1 (+ index 2) stack))
                (eq? (nth tmpl index) #\<) ;; entity reference
                    (begin
                        (display "&lt;" out)
                        (apply-template tmpl ctx out 1 (+ index 1) stack))
                (eq? (nth tmpl index) #\>)
                    (begin
                        (display "&gt;" out)
                        (apply-template tmpl ctx out 1 (+ index 1) stack))
                (eq? (nth tmpl index) #\&) ;; probably should be smart, in case someone does "&copy;" or the like...
                    (begin
                        (display "&amp;" out)
                        (apply-template tmpl ctx out 1 (+ index 1) stack))
                (eq? (nth tmpl index) #\newline) ;; paragraph closer!
                    (if (single-line-state? (car stack))
                        (begin
                            (display (nth *close-tags* (car stack)) out)
                            (newline out)
                            (apply-template tmpl ctx out 0 (+ index 1) (cdr stack)))
                        (begin
                            (newline out)
                            (apply-template tmpl ctx out 0 (+ index 1) stack)))
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
            (apply-backtick tmpl ctx out state index stack)
        (= state 9)
            (apply-backtick tmpl ctx out state index stack)
        (= state 14)
            (if (eq? (nth tmpl index) #\space)
                (begin
                    (display "\">" out)
                    (apply-template tmpl ctx out 1 (+ index 1) (cons 14 stack)))
                (begin
                    (display (nth tmpl index) out)
                    (apply-template tmpl ctx out 14 (+ index 1) stack)))
        (= state 15)
            (if (eq? (nth tmpl index) #\space)
                (begin
                    (display "\" alt=\"" out)
                    (apply-template tmpl ctx out 1 (+ index 1) (cons 15 stack)))
                (begin
                    (display (nth tmpl index) out) ;; a takeWhile here would mean calling display once...
                    (apply-template tmpl ctx out 15 (+ index 1) stack)))))

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
