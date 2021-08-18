;;;; undermidi module for music functions specific to undermidi
(defmodule um
  (export
   (chord 1) (chord 2)
   (cycle-cc 4)
   (get-pitch 1)
   (play-chord 2) (play-chord 3)
   (play-note 1) (play-note 2) (play-note 3)
   (play-notes 1) (play-notes 3)
   (play-pitch 2) (play-pitch 3)
   (ramp-cc 4)
   (set-cc 2)
   (set-channel 1)
   (set-device 1)
   (soft-pedal-off 0)
   (soft-pedal-on 0)
   (stop-note 1)
   (sustain-pedal-off 0)
   (sustain-pedal-on 0)))

(include-lib "undermidi/include/notes.lfe")

(include-lib "logjam/include/logjam.hrl")

(defun chord (note-names velocity)
  "Take a list of pitches that should be played at the same time, all with
  the given volume, and generate the proper MIDI messages for notes on and
  notes off."
  (let ((note-names+velocities (list-comp ((<- note note-names))
                                 (list note velocity))))
    (chord note-names+velocities)))

(defun chord (note-names+velocities)
  "Take a list of pitches that should be played at the same time, each with
  their own volume, and generate the proper MIDI message for notes on and
  notes off."
  (let* ((pitches+velocities (get-pitches note-names+velocities #(with-velocities)))
         (notes-on (list-comp ((<- args pitches+velocities))
                     (apply #'midimsg:note-on/2 args)))
         (notes-off (list-comp ((<- args pitches+velocities))
                      (apply #'midimsg:note-off/1 (list (car args))))))
    (log-debug "Chord notes: ~p" (list notes-on))
    `#m(notes-on ,(undermidi.msg:batch notes-on 'true)
        notes-off ,(undermidi.msg:batch notes-off 'true))))

(defun play-chord (notes-names velocity duration)
  (play-chord-data (chord notes-names velocity) duration))

(defun play-chord (note-names+velocities duration)
  (play-chord-data (chord note-names+velocities) duration))

(defun play-chord-data
  ((`#m(notes-on ,on notes-off ,off) duration)
   (undermidi:send on)
   (timer:sleep duration)
   (undermidi:send off)))

(defun get-pitch (note-name)
  (erlang:apply 'um.notes note-name '()))

(defun get-pitches (notes)
  (get-pitches notes #()))

(defun get-pitches
  ((notes+velocities #(with-velocities))
   (let ((lookup (um.notes:all)))
     (list-comp ((<- `(,x ,vel) notes+velocities))
       (list (mref lookup x) vel))))
  ((notes #())
   (let ((lookup (um.notes:all)))
     (list-comp ((<- x notes))
       (mref lookup x)))))

(defun play-note
  ((`#m(note ,note-name vel ,velocity dur ,duration))
   (play-note note-name velocity duration)))

(defun play-note (note-name velocity)
  (undermidi:send (midimsg:note-on (get-pitch note-name) velocity)))

(defun play-note (note-name velocity duration)
  (let ((pitch (get-pitch note-name)))
    (undermidi:send (midimsg:note-on pitch velocity))
    (timer:sleep duration)
    (undermidi:send (midimsg:note-off pitch))))

(defun play-notes (notes-data)
  (list-comp ((<- note-data notes-data))
    (play-note note-data))
  'ok)

(defun play-notes (note-names velocity duration)
  (list-comp ((<- note-name note-names))
    (play-note note-name velocity duration))
  'ok)

(defun play-pitch (pitch velocity)
  (undermidi:send (midimsg:note-on pitch velocity)))

(defun play-pitch (pitch velocity duration)
  (undermidi:send (midimsg:note-on pitch velocity))
  (timer:sleep duration)
  (undermidi:send (midimsg:note-off pitch)))

(defun sched-cc
  ((value (= `#m(con ,controller incr ,incr acc ,acc) data))
   (let* ((sum (+ acc incr))
          (sched (timer:apply_after (round sum)
                                    'um
                                    'set-cc
                                    (list controller value))))
     (log-debug "Scheduled: ~p" `(,sched))
     (mupd data 'acc sum))))

(defun ramp-cc (controller start-value end-value duration)
  (let* ((sq (undermidi.util:seq start-value end-value))
         (steps (length sq))
         (incr (/ (* 1000 duration) steps)))
    (lists:foldl #'sched-cc/2
     `#m(con ,controller incr ,incr acc 0)
     sq))
  'ok)

(defun cycle-cc (controller start-value end-value duration)
  (let* ((up (undermidi.util:seq start-value end-value))
         (down (lists:reverse up))
         (sq (lists:append up down))
         (steps (length sq))
         (incr (/ (* 1000 duration) steps)))
    (lists:foldl #'sched-cc/2
     `#m(con ,controller incr ,incr acc 0)
     sq))
  'ok)

(defun stop-note (note-name)
  (undermidi:send (midimsg:note-off (get-pitch note-name))))

(defun set-cc (controller value)
  (undermidi:send (midimsg:cc controller value)))

(defun soft-pedal-off ()
  (set-cc 66 0))

(defun soft-pedal-on ()
  (set-cc 66 127))

(defun sustain-pedal-off ()
  (set-cc 64 0))

(defun sustain-pedal-on ()
  (set-cc 64 127))

(defun set-device (int)
  (undermidi:send (midimsg:device int)))

(defun set-channel (int)
  (undermidi:send (midimsg:channel int)))

(defun octave
  ((note-name oct) (when (is_atom note-name))
   (octave (get-pitch note-name) oct))
  ((pitch oct)
   (+ pitch (* 12 oct))))
