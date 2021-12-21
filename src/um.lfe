;;;; undermidi module for music functions specific to undermidi
(defmodule um
  (export
   (bank-select 2) (bank-select 3)
   (cycle-cc 4)
   (play-chord 3) (play-chord 5) (play-chord 6)
   (play-note 3)
   (play-notes 3)
   (play-pitch 3)
   (play-pitches 3)
   (program-change 1)
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

;;; Chord convenience aliases

(defun play-chord (pitches velocity duration)
  (um.chord:play pitches velocity duration)
  'ok)

(defun play-chord (chord-name key oct velocity duration)
  (um.chord:play chord-name key oct velocity duration)
  'ok)

(defun play-chord (mode index key oct velocity duration)
  (um.chord:play mode index key oct velocity duration)
  'ok)

;; Note and pitch convenience aliases

(defun play-note (note-name velocity duration)
  (um.note:play note-name velocity duration)
  'ok)

(defun play-notes (note-names velocity duration)
  (list-comp ((<- note-name note-names))
    (play-note note-name velocity duration))
  'ok)

(defun play-pitch (pitch velocity duration)
  (play-note pitch velocity duration))

(defun play-pitches (pitches velocity duration)
  (play-notes pitches velocity duration))

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
  (undermidi:send
   (midimsg:note-off (um.note:get-pitch note-name))))

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

(defun bank-select
  ((`(,msb ,lsb) program)
   (bank-select msb lsb program)))

(defun bank-select (msb lsb program)
  (undermidi:send (midimsg:bank-select msb lsb program)))

(defun program-change (program)
  (undermidi:send (midimsg:program-change program)))

(defun set-device (int)
  (undermidi:send (midimsg:device int)))

(defun set-channel (int)
  (undermidi:send (midimsg:channel int)))

;; Real-time messages

(defun rt-clock ()
  (undermidi:send (midimsg:rt-clock)))

(defun rt-continue ()
  (undermidi:send (midimsg:rt-continue)))

(defun rt-reset ()
  (undermidi:send (midimsg:rt-reset)))

(defun rt-start ()
  (undermidi:send (midimsg:rt-start)))

(defun rt-stop ()
  (undermidi:send (midimsg:rt-stop)))

(defun rt-tick ()
  (undermidi:send (midimsg:rt-tick)))
