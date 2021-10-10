(defmodule um.scale
  (export all))

(defun pitches
  ((scale-name key) (when (is_atom scale-name))
   (pitches (erlang:apply 'uth.scale scale-name '()) key))
  ((scale key)
   (um.note:template->pitches scale key)))

(defun create (scale start-oct oct-count)
  (create scale start-oct oct-count 0))

(defun create (scale start-oct oct-count key)
  (let ((midi-notes (pitches scale key))
        (last-oct (- (+ start-oct oct-count) 1)))
     (lists:flatten
      (list
       (list-comp ((<- oct (lists:seq start-oct last-oct)))
         (list-comp ((<- pitch midi-notes))
           (um.note:octave pitch oct)))
       (um.note:octave (car scale) (+ 1 last-oct))))))
