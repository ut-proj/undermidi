;;;; undermidi module for music functions specific to undermidi
(defmodule um
  (export
   (chord 1) (chord 2)
   (play-chord 2) (play-chord 3)
   (set-device 1)
   (set-channel 1)))

(defun chord (pitches velocity)
  "Take a list of pitches that should be played at the same time, all with
  the given volume, and generate the proper MIDI messages for notes on and
  notes off."
  (let ((pitches+velocities (list-comp ((<- pitch pitches))
                              (list pitch velocity))))
    (chord pitches+velocities)))

(defun chord (pitches+velocities)
  "Take a list of pitches that should be played at the same time, each with
  their own volume, and generate the proper MIDI message for notes on and
  notes off."
  (let ((notes-on (list-comp ((<- args pitches+velocities))
                    (apply #'midimsg:note-on/2 args)))
        (notes-off (list-comp ((<- args pitches+velocities))
                     (apply #'midimsg:note-off/1 (list (car args))))))
    `#m(notes-on ,(undermidi.msg:batch notes-on 'true)
        notes-off ,(undermidi.msg:batch notes-off 'true))))

(defun play-chord (pitches velocity duration)
  (play-chord-data (chord pitches velocity) duration))

(defun play-chord (pitches+velocities duration)
  (play-chord-data (chord pitches+velocities) duration))

(defun play-chord-data
  ((`#m(notes-on ,on notes-off ,off) duration)
   (undermidi:send on)
   (timer:sleep duration)
   (undermidi:send off)))

(defun set-device (int)
  (undermidi:send (midimsg:device int)))

(defun set-channel (int)
  (undermidi:send (midimsg:channel int)))
