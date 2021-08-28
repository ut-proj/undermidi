(defmodule um.chord
  (export all))

(defun pitches
  ((chord-name key) (when (is_atom chord-name))
   (pitches (erlang:apply 'uth.chord chord-name '()) key))
  ((chord key)
   (um:template->pitches chord key)))

;; XXX generalise create/3 and create/4 here and in um.scale ...
(defun create (chord start-oct oct-count)
  (create chord start-oct oct-count 0))

(defun create (chord start-oct oct-count key)
  (let ((midi-notes (pitches chord key))
        (last-oct (- (+ start-oct oct-count) 1)))
     (lists:flatten
      (list
       (list-comp ((<- oct (lists:seq start-oct last-oct)))
         (list-comp ((<- pitch midi-notes))
           (um:octave pitch oct)))
       (um:octave (car chord) (+ 1 last-oct))))))
