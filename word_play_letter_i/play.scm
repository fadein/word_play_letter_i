#!/usr/bin/csi -s

(use srfi-1 irregex)

(define *words-file* "shorty")
;;;                      1    5 211 2   4   4      7 21  3 21
(define *spare-letters* "bcccccddghkkllllmmmmnnnnnnnpprsssttw")

;; make a structure mapping count of spare letters remaining
(define spares
  (let outer ((letters (string->list *spare-letters*)))

	(if (null? letters) '()
	  (let inner ((letters letters) (this (car letters)) (count 0))

		  (cond 
			((null? letters)
			 (cons (cons this count) '()))
			
			((eq? this (car letters))
			 (inner (cdr letters) this (add1 count)))

			(else
			  (cons (cons this count) (outer letters))))))))
			 

(define grid '( "i."
				"i.."
				".i.."
				".i.i."
				"i..i.."
				"i..i.i."
				".i...i.."
				"i...i..i."
				".i..i..i.."))

;; regular-expression versions of the strings in grid
(define regexen
  (list->vector
	(let ((cclass (conc "[" 
						(list->string (delete-duplicates (string->list *spare-letters*)))
						"]")))
	  (cons '()
			(cons '()
				  (map (lambda (p) (irregex-replace/all (irregex "\\.") p cclass))
					   grid))))))

;; make a vector of lists of words that *could* fit into our grid
;; the vector is indexed upon the length of the candidate word
(define i-words
  (let* ((min-len (apply min (map string-length grid)))
		 (max-len (apply max (map string-length grid)))
		 (vect (make-vector (+ 1 max-len) '())))

	(with-input-from-file
	  *words-file*
	  (lambda ()
		(let loop ((word (read-line))) ; the \n is chomped from each line
		  (when (not (eof-object? word))
			(let ((word-len (string-length word)))

			  (when (and (<= word-len max-len)
						 (>= word-len min-len)
						 (irregex-match (vector-ref regexen word-len) word))
				(printf "adding ~a to wordlist len ~a~n" word word-len)
				(vector-set! vect word-len
							 (append (vector-ref vect word-len) `(,word))))
			  (loop (read-line)))))))
	vect))


;; find all 2-letter words in the list that fit in the grid
;; this should just return all of the 2-letter words in i-words
(define (find-2-letter-words)
  (let loop ((i-words (vector-ref i-words 2)))
	(unless (null? i-words)
	  (when (irregex-match (vector-ref regexen 2) (car i-words))
		(print (car i-words))
		(loop (cdr i-words))))))
