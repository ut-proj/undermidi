(defmodule um.chord
  (export all))

(defun pitches
  ((chord-name key) (when (is_atom chord-name))
   (pitches (erlang:apply 'uth.chord chord-name '()) key))
  ((chord key)
   (um:template->pitches chord key)))

(defun create (chord oct)
  (create chord oct 0))

(defun create (chord oct key)
  (let ((midi-notes (pitches chord key)))
     (lists:flatten
      (list
         (list-comp ((<- pitch midi-notes))
           (um:octave pitch oct))))))

(defun create (mode index oct key)
  (create
   (mref (mref (uth:modes) mode) index)
   oct
   key))