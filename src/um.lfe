;;;; undermidi module for music functions specific to undermidi
(defmodule um
  (export
   (chord 1) (chord 2)
   (cycle-cc 4)
   (get-pitch 1)
   (octave 2)
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

(defun template->pitch
  ((1) 0)
  (('|#1|) 1)
  (('|##1|) 2)
  (('bb2) 0)
  (('b2) 1)
  ((2) 2)
  (('|#2|) 3)
  (('|##2|) 4)
  (('bb3) 2)
  (('b3) 3)
  ((3) 4)
  (('|#3|) 5)
  (('|##3|) 6)
  (('bb4) 3)
  (('b4) 4)
  ((4) 5)
  (('|#4|) 6)
  (('|##4|) 7)
  (('bb5) 5)
  (('b5) 6)
  ((5) 7)
  (('|#5|) 8)
  (('|##5|) 9)
  (('bb6) 7)
  (('b6) 8)
  ((6) 9)
  (('|#6|) 10)
  (('|##6|) 11)
  (('bb7) 9)
  (('b7) 10)
  ((7) 11)
  (('|#7|) 12)
  (('|##7|) 13)
  (('bb8) 10)
  (('b8) 11)
  ((8) 12)
  (('|#8|) 13)
  (('|##8|) 14)
  (('bb9) 12)
  (('b9) 13)
  ((9) 14)
  (('|#9|) 15)
  (('|##9|) 16)
  (('bb10) 14)
  (('b10) 15)
  ((10) 16)
  (('|#10|) 17)
  (('|##10|) 18)
  (('bb11) 16)
  (('b11) 17)
  ((11) 18)
  (('|#11|) 19)
  (('|##11|) 20)
  (('bb12) 18)
  (('b12) 19)
  ((12) 20)
  (('|#12|) 21)
  (('|##12|) 22)
  (('bb13) 20)
  (('b13) 21)
  ((13) 22)
  (('|#13|) 23)
  (('|##13|) 24))

(defun template->pitches (template)
  (template->pitches template 0))

(defun template->pitches
  ((template key) (when (is_atom key))
   (template->pitches template (mref (um.notes:all) key)))
  ((template key)
   (list-comp ((<- note template))
     (+ (template->pitch note) key))))
