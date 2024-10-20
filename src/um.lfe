;;;; undermidi module for music functions specific to undermidi
(defmodule um
  (export
   (devices 0))
  ;; !!DEPRECATED!!
  (export
   (bank-select 2) (bank-select 3)
   (program-change 1)))

(include-lib "undermidi/include/notes.lfe")
(include-lib "logjam/include/logjam.hrl")

;;; New API

(defun devices ()
  (um.nif:refresh)
  `#m(inputs ,(lists:enumerate (um.nif:inputs))
      outputs ,(lists:enumerate (um.nif:outputs))))


;;; !!DANGER!! -- everything below this line is going to change,
;;;               anything from function names and args to module
;;;               of even the function's existence itself is up
;;;               for grabs ...

(defun bank-select
  ((`(,msb ,lsb) program)
   (bank-select msb lsb program)))

(defun bank-select (msb lsb program)
  (undermidi:send (midimsg:bank-select msb lsb program)))

(defun program-change (program)
  (undermidi:send (midimsg:program-change program)))
