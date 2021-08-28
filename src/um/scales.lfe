(defmodule um.scale
  (export all))

(defun midi (scale)
  (midi scale 0))

(defun midi
  ((scale key) (when (is_atom key))
   (midi scale (mref (um.notes:all) key)))
  ((scale key)
   (list-comp ((<- s scale))
     (+ (template->pitch s) key))))

(defun get (scale-name start-oct oct-count)
  (get scale-name start-oct oct-count 0))

(defun get (scale-name start-oct oct-count key)
  (let ((scale (midi
                (erlang:apply 'uth.scale scale-name '())
                key))
        (last-oct (- (+ start-oct oct-count) 1)))
    (lists:flatten
     (list
      (list-comp ((<- oct (lists:seq start-oct last-oct)))
        (list-comp ((<- x scale))
          (um:octave x oct)))
      (um:octave (car scale) (+ 1 last-oct))))))
