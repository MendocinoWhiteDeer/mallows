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
  (export no-block-repp)
  (import (chezscheme))
  
  (define no-block-read
    (lambda (p)
      (letrec
	  ([fn
	    (lambda (nest-stack expr)
	      (let ([c (peek-char p)])
		(cond
		 ;; when eof return
		 [(eof-object? c)
		  (if (and (null? nest-stack) (pair? expr))
		      (reverse expr)
		      '())]
		 ;; when newline return
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
	(fn (string->list [get-output-string output]))
	(write-char #\newline p)
	(flush-output-port p))))

  (define no-block-repp
    (lambda (in-p out-p)
      (let ([expr (no-block-read in-p)])
	(if (pair? expr)
	    (no-block-print (eval expr) out-p))))))
