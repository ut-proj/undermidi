(defmodule um.measure
  (export all))

(defun time-sig (beats-per-measure beat-value)
  `#(,beats-per-measure ,beat-value)) ; this is isomorphic to, e.g., 4/4, 3/4, 3/8, etc.

(defun make (time-sig key-sig voices)
  '#m(time-sig ,time-sig
      key-sig ,key-sig
      voices ,(add-voices)))

(defun add-voices
  (('() acc)
   acc)
  ((`(,voice . ,tail) acc)
   ;; TODO process voice
   (add-voices tail acc)))
