#!/usr/bin/csi -s

(use srfi-1 irregex)

(define *words-file* "words")
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


;; find all n-letter words in the list that fit in the grid
;; this should just return all of the n-letter words in i-words
(define (find-n-letter-words n)
  (let loop ((i-words (vector-ref i-words n)))
	(unless (null? i-words)
	  (when (irregex-match (vector-ref regexen n) (car i-words))
		(print (car i-words))
		(loop (cdr i-words))))))


;; apply deductions from the alist of spare letters and their counts
;; Input: spare letters alist and a word
;; return alist of spare letters and their counts,
;;    less the letters contained within the word
;; return #f when we try to deduct from a letter which is already empty
(define (apply-deductions! alis werd)
  (let loop ((alis alis) (letters (string->list werd)))
	(cond
	  ;; end of the letters in werd
	  ((null? letters)
	   alis)

	  ;; a letter in werd that's not deductible (e.g. #\i)
	  ((not (assoc (car letters) alis))
	   (loop alis (cdr letters)))

	  (else
		(let ((spare (assoc (car letters) alis)))
		  (if (zero? (cdr spare))
			#f
			(begin
			  (set-cdr! spare (sub1 (cdr spare)))
			  (loop alis (cdr letters)))))))))

;; utility function to make a deep-copy of a list
;; In Scheme the cdr of a list is a location, which is basically like
;; a pointer for purposes of copying a list - each list points to the
;; same location, so modifying one list affects all copies
(define (deep-copy thing)
   (if (not (pair? thing))
       thing
       (cons (deep-copy (car thing))
             (deep-copy (cdr thing)))))

(define (main)
  (define (inner n spares)
	(if (= n (vector-length i-words))
	  #t
	  (begin
		;(print "DEBUG n is " n); DELETE ME
		(let loop ((w (vector-ref i-words n)))
		  (if (null? w)
			#f
			(let* ((the-word (car w))
				   (deductions (apply-deductions! (deep-copy spares) the-word)))
			  ;(printf "trying ~a word ~a~n" n the-word) ;DELETE ME
			  (if deductions
				(let ((it-worked (inner (add1 n) deductions)))
				  (if it-worked
					(cons the-word it-worked)
					(begin
					  ;(print the-word " didn't work, backtracking") ; DELETE ME
					  (loop (cdr w)))))
				(begin
				  ;(print " " the-word " didn't deduct, trying next word") ; DELETE ME
				  (loop (cdr w))))))))))
  (inner 2 spares))

(print (main))
