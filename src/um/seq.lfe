(defmodule um.seq
  (export all))

(defun repeat (notes count)
  (repeat notes count '()))

(defun repeat
  ((_ 0 acc)
   acc)
  ((notes count acc)
   (repeat notes (- count 1) (++ acc (list notes)))))
               
(defun play (device notes)
  'tbd)
