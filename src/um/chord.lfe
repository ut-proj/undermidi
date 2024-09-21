(defmodule um.chord
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun make (names)
  (um.note:make names))

(defun make-fuzzy (names)
  (um.note:make-fuzzy names))

(defun invert
  ((`(#m(pitch ,p velocity ,v duration ,d) . ,tail))
   (++ tail (list (um.note:make (+ 12 p) v d)))))

(defun play
  ((device channel '())
   'ok)
  ((device channel (= `(,head . ,_) notes)) (when (is_atom head))
   (play device channel (um.note:make notes)))
  ((device channel `(#m(pitch ,p velocity ,v duration ,d) . ,tail))
   (let ((note-on (midimsg:note-on channel p v))
         (note-off (midimsg:note-on channel p 0)))
     (um.ml:send device note-on)
     (timer:apply_after d 'um.ml 'send `(,device ,note-off))
     (play device channel tail))))

(defun play-chords (device channel chords)
  (play-chords device channel chords 250))

(defun play-chords (device channel chords delay)
  (play-chords device channel chords delay 0))

(defun play-chords (device channel chords delay repeats)
  (play-chords device channel chords delay repeats '()))

(defun play-chords
  ((device channel '() _ 0 _)
   'ok)
  ((device channel '() delay repeats acc)
   (play-chords device channel acc delay (- repeats 1) '()))
  ((device channel `(,head . ,tail) delay repeats acc)
   (play device channel head)
   ;; TODO: this is a hack; we need to send timing data MIDI messages ...
   (timer:sleep delay)
   (play-chords device channel tail delay repeats (++ acc `(,head)))))

(defun lengthen (chord duration-multiplier)
  (um.note:lengthen chord duration-multiplier))
