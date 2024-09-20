(defmodule um.chord
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun make (notes)
  'tbd)
  
(defun invert
  ((`(#m(pitch ,p velocity ,v duration ,d) . ,tail))
   (++ tail (list (um.note:make (+ 12 p) v d)))))

(defun play ()
  'tbd)
  
