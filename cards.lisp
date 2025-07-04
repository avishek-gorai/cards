;;; cards.lisp -- Main program.
;;; Copyright (c) 2025 Avishek Gorai <avishekgorai@myyahoo.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package cards)

(defvar my-hand
  (quote
   ((3 hearts)
    (5 clubs)
    (2 diamonds)
    (4 diamonds)
    (ace spades)))
  "Cards in hand.")

(defvar colors
  (quote
   ((clubs black)
    (diamonds red)
    (hearts red)
    (spades black)))
  "Colors of cards.")

(defvar all-ranks
  (quote (2 3 4 5 6 7 8 9 10 jack queen king ace))
  "Ranks of cards.")

(defun rank (card)
  "Returns rank of card."
  (first card))

(defun suit (card)
  "Returns suit of card."
  (second card))

(defun count-suit (suit hand)
  "Returns the number of cards belonging to the given suit."
  (length (remove-if-not
	   (function (lambda (card)
             (eq suit (suit card))))
	   hand)))

(defun color-of (card)
  "Returns color of the card."
  (second (assoc (second card) colors)))

(defun first-red (hand)
  "Returns the first card of a hand that is of a red suit."
  (find-if
   (function (lambda (card)
     (eq (quote red) (color-of card))))
   hand))

(defun black-cards (hand)
  "Returns a list of all black cards of the given hand."
  (remove-if-not
   (function (lambda (card)
     (eq (quote black) (color-of card))))
   hand))

(defun what-ranks (suit hand)
  "Returns the ranks of all cards belonging to that suit."
  (mapcar (function rank)
          (remove-if-not
	   (function (lambda (card)
             (equal suit (suit card))))
	   hand)))

(defun beforep (x y l)
  "Returns T if X appears before Y in L."
  (member y (member x l)))

(defun higher-rank-p (card1 card2)
  "Returns T if the first card has a higher rank than the second."
  (if (and card1 card2)
      (not (beforep (rank card1) (rank card2) all-ranks))))
