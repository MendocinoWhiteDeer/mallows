;;; This file is part of mallows.
;;; Copyright (C) 2024 Taylor Wampler 

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(library (mallows)
  (export trie-insert! trie-completion-list no-block-repp)
  (import (chezscheme))

  (define-record-type trie-node (fields word-end? child-nodes))
  (define trie (make-trie-node #f (make-eq-hashtable 32)))

  (define trie-insert!
    (lambda (word-string)
      (letrec
	  ([word-char-list (string->list word-string)]
	   [fn!
	    (lambda (node char-list)
	      (if (and (trie-node? node) (pair? char-list))
		  (let ([c (car char-list)] [child-nodes (trie-node-child-nodes node)])
		    (unless (hashtable-contains? child-nodes c)
		      (hashtable-set! child-nodes c (make-trie-node (null? (cdr char-list)) (make-eq-hashtable 8))))
		    (fn! (hashtable-ref child-nodes c #f) (cdr char-list)))))])
	(fn! trie word-char-list))))

  ;; Return last trie node when given a prefix, #f if the prefix is not part of any word in the trie  
  (define trie-end-node
    (lambda (prefix-string)
      (letrec
	  ([prefix-char-list (string->list prefix-string)]
	   [fn
	    (lambda (node prefix-char-list)
	      (cond
	       [(not (trie-node? node)) #f]
	       [(pair? prefix-char-list)
		(let ([c (car prefix-char-list)])
		  (fn (hashtable-ref (trie-node-child-nodes node) c #f) (cdr prefix-char-list)))]
	       [else node]))])
	   (fn trie prefix-char-list))))
  
  (define trie-completion-list
    (lambda (word-prefix)
      (letrec
	  ([prefix-end-node (trie-end-node word-prefix)]
	   [completion-list '()]
	   [fn
	    (lambda (node suffix-char-list)
	      (if (trie-node? node)
		  (let-values ([(child-chars child-nodes) (hashtable-entries (trie-node-child-nodes node))])
		    (letrec* ([len (vector-length child-chars)] [i 0]
			      [iterate-nodes
			       (lambda ()
				 (when (< i len)
				   (let ([c (vector-ref child-chars i)] [n (vector-ref child-nodes i)])
				     (fn n (cons c suffix-char-list)))))])
		      (if (trie-node-word-end? node)
			  (set! completion-list (cons (string-append word-prefix (list->string (reverse suffix-char-list))) completion-list)))
		      (iterate-nodes)))))])
	(fn prefix-end-node '())
	completion-list)))
		       
  (define no-block-read
    (lambda (p)
      (letrec
	  ([fn
	    (lambda (nest-stack expr)
	      (let ([c (peek-char p)])
		(cond
		 ;; When eof return
		 [(eof-object? c)
		  (if (and (null? nest-stack) (pair? expr))
		      (reverse expr)
		      '())]
		 ;; When newline return
		 [(char=? c #\newline)
		  (read-char p)
		  (if (and (null? nest-stack) (pair? expr))
		      (reverse expr)
		      '())]
		 ;; Consume open square/round bracket and recurse pushing it on nest-stack
		 [(or (char=? c #\() (char=? c #\[))
		  (read-char p)
		  (fn (cons c nest-stack) (cons c expr))]
		 ;; Consume closed round bracket; if matched by an open round bracket, recurse popping it off nest-stack, else return empty
		 [(char=? c #\))
		  (read-char p)
		  (if (and (pair? nest-stack) (char=? (car nest-stack) #\())
		      (fn (cdr nest-stack) (cons c expr))
		      '())]
		 ;; Consume closed square bracket; if matched by an open square bracket, recurse popping it off nest-stack, else return empty
		 [(char=? c #\])
		  (read-char p)
		  (if (and (pair? nest-stack) (char=? (car nest-stack) #\[))
		      (fn (cdr nest-stack) (cons c expr))
		      '())]
		 ;; Else continue building the expr
		 [else
		  (read-char p)
		  (fn nest-stack (cons c expr))])))])
	(if (char-ready? p)
	    (let ([expr-list (fn '() '())])
	      (if (pair? expr-list)
		  (read (open-input-string [list->string expr-list]))))))))

  (define no-block-print
    (lambda (expr p)
      (letrec
	  ([output (open-output-string)]
	   [fn
	    (lambda (char-list)
	      (when (pair? char-list)
		(write-char (car char-list) p)
		(fn (cdr char-list))))])
	(write expr output)
	(fn (string->list "EVAL: "))
	(fn (string->list [get-output-string output]))
	(write-char #\newline p)
	(flush-output-port p))))

  (define no-block-repp
    (lambda (in-p out-p)
      (let ([expr (no-block-read in-p)])
	(if (pair? expr)
	    (no-block-print (eval expr) out-p))))))
