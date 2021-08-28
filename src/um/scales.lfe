(defmodule um.scale
  (export all))

(defun pitches (scale)
  (pitches scale 0))

(defun pitches
  ((scale key) (when (is_atom key))
   (pitches scale (mref (um.notes:all) key)))
  ((scale-name key) (when (is_atom scale-name))
   (pitches (erlang:apply 'uth.scale scale-name '()) key))
  ((scale key)
   (list-comp ((<- s scale))
     (+ (um:template->pitch s) key))))

(defun create (scale start-oct oct-count)
  (create scale start-oct oct-count 0))

(defun create (scale start-oct oct-count key)
  (let ((midi-notes (pitches scale key))
        (last-oct (- (+ start-oct oct-count) 1)))
     (lists:flatten
      (list
       (list-comp ((<- oct (lists:seq start-oct last-oct)))
         (list-comp ((<- pitch midi-notes))
           (um:octave pitch oct)))
       (um:octave (car scale) (+ 1 last-oct))))))
