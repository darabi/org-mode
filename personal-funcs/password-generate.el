;; courtesy of
;;
;; https://github.com/mjago/Emacs/blob/master/make-password/make-password.el
;;
;; renamed make-password... to password-generate-... because I found myself entering
;; password<TAB> again and again

(defun* password-generate (length &optional (upper t) (lower t) (number t) (symbol nil) (ambiguous nil))
  "Return a string of LENGTH random characters.  If UPPER is non-nil,
use uppercase letters.  If lower is non-nil, use lowercase letters.
If NUMBER is non-nil, use numbers.  If SYMBOL is non-nil, use one of
\"!\"#$%&'()*+'-./:;<=>?@`{}|~\".  If AMBIGUOUS is nil, avoid
characters like \"l\" and \"1\", \"O\" and \"0\"."
  (interactive (password-generate-prompt-for-args))
  (let ((char-list (password-generate-char-list upper lower number symbol ambiguous))
	 position password)
    (random t)
  (loop for i from 1 to length 
	do (setq position (random (length char-list))
		 password (concat password (string (nth position char-list)))))
  (if (interactive-p)
      (let* ((strength (password-generate-strength length upper lower number symbol ambiguous))
	     (bits (car strength))
	     (number (cadr strength)))
	(message "The password \"%s\" is one of 10^%d possible and has a bit equivalence of %d" 
		 password (round number) (round bits)))
    password)))

(defun password-generate-char-list (upper lower number symbol ambiguous)
  (let* ((upper-chars-ambiguous '(?I ?O ?G))
	 (upper-chars (loop for i from ?A to ?Z unless 
			    (member i upper-chars-ambiguous)
			    collect i))
	 (lower-chars-ambiguous '(?l ?o))
	 (lower-chars (loop for i from ?a to ?z unless 
			    (member i lower-chars-ambiguous)
			    collect i))
	 (number-chars-ambiguous '(?0 ?1 ?6))
	 (number-chars (loop for i from ?0 to ?9 unless
			     (member i number-chars-ambiguous)
			     collect i))
	 (symbol-chars '(?! ?@ ?# ?$ ?% ?& ?* ?( ?) ?+ ?= ?/ 
			    ?{ ?} ?[ ?] ?: ?\; ?< ?>))
	 (symbol-chars-ambiguous '(?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\"))
	 char-list)
  (if upper
      (setq char-list (append char-list upper-chars)))
  (if lower
      (setq char-list (append char-list lower-chars)))
  (if number
      (setq char-list (append char-list number-chars)))
  (if symbol
      (setq char-list (append char-list symbol-chars)))
  (if ambiguous
      (setq char-list (append char-list
			      upper-chars-ambiguous 
			      lower-chars-ambiguous
			      number-chars-ambiguous
			      symbol-chars-ambiguous)))
  char-list))

(defun password-generate-prompt-for-args ()
  (interactive)
  (list
   (string-to-number (read-from-minibuffer "Number of Characters: "))
   (y-or-n-p "User uppercase: ")
   (y-or-n-p "User lowercase: ")
   (y-or-n-p "User numbers: ")
   (y-or-n-p "User symbols: ")
   (y-or-n-p "User ambiguous characters: ")))

(defun* password-generate-strength (length &optional (upper t) (lower t) (number t) (symbol nil) (ambiguous nil))
  "Calculate the number of possible passwords that could be generated
given the criteria of LENGTH and use of UPPER, LOWER, NUMBER, SYMBOL,
and AMBIGUOUS characters"
  (interactive (password-generate-prompt-for-args))
  (let* ((char-list (password-generate-char-list upper lower number symbol ambiguous))
	 (bits (/ (* length (log (length char-list))) (log 2)))
	 (number (/ (* bits (log 2)) (log 10))))
    (if (interactive-p)
	(message "number of combinations is 10^%d with a bit equivalence of %d" (round number) (round bits))
      (list bits number))))

 (require 'epa-file)
    (epa-file-enable)

(provide 'password-generate)
