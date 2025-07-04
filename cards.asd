;;; cards.asd -- System definition.
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

(asdf:defsystem cards
  :description "Manipulation of playing cards using applicative operator."
  :author "Avishek Gorai <avishekgorai@myyahoo.com>"
  :license  "GNU General Public License version 3 or later"
  :version "1.0"
  :serial t
  :components ((:file "package")
               (:file "cards")))
