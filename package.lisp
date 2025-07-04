;;; package.lisp -- Package definition.
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

(defpackage cards
  (:use common-lisp)
  (:export my-hand
           colors
           all-ranks
           rank
           suit
           count-suit
           color-of
           first-red
           black-cards
           what-ranks
           beforep
           higher-rank-p)
  (:documentation "Manipulation of playing cars using applicative operators."))
