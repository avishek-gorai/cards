(defvar my-hand)
(defvar colors)
(defvar all-ranks)

(setf all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(setf colors '((clubs black)
	       (diamonds red)
	       (hearts red)
	       (spades black)))

(setf my-hand '((3 hearts)
		(5 clubs)
		(2 diamonds)
		(4 diamonds)
		(ace spades)))

;;; This function is not in the exercise.
(defun valid-card (card)
  "Returns t if card is valid."
  ;; To check for a valid card,
  ;; it must be a list of length 2,
  ;; first element must be a valid rank
  ;; and second element must be a valid suit.
  (and (listp card)
       (eql (length card) 2)
       (member (first card) all-ranks)
       (let ((suit (second card))) ; This could also be done using a set of suits.
	 (or (eq suit 'spades)
	     (eq suit 'hearts)
	     (eq suit 'diamonds)
	     (eq suit 'clubs)))))

(defun rank (card)
  "Returns rank if card is valid, else returns nil."
  (if (valid-card card) (first card)))

(defun suit (card) (second card))

(defun count-suit (suit hand)
  "Returns the number of cards belonging to the given suit."
  (length (remove-if-not
	   #'(lambda (card) (eq suit (suit card)))
	   hand)))

(defun color-of (card)
  (second (assoc (second card) colors)))

(defun first-red (hand)
  "Returns the first card of a hand that is of a red
suit, or nil if none are."
  (find-if
   #'(lambda (card) (eq 'red (color-of card)))
   hand))

(defun black-cards (hand)
  "Returns a list of all black cards of the given hand."
  (remove-if-not
   #'(lambda (card) (eq 'black (color-of card)))
   hand))

(defun what-ranks (suit hand)
  "Returns the ranks of all cards belonging to that suit."
  (mapcar #'rank (remove-if-not
		  #'(lambda (card) (equal suit (suit card)))
		  hand)))

(defun beforep (x y l)
  "Returns true if X appears before Y in L."
  (member y (member x l)))

(defun higher-rank-p (card1 card2)
  "Returns t if the first card has a higher rank than the second."
  (if (and (valid-card card1) (valid-card card2))
      (not (beforep (rank card1) (rank card2) all-ranks))))
