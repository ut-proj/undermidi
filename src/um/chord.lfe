(defmodule um.chord
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun make (names)
  (um.note:make names))

(defun make-fuzzy (names)
  (um.note:make-fuzzy names))

(defun make-fuzzy (names variances)
  (list-comp ((<- n names)) (um.note:make-fuzzy n (um.note:default-values) variances)))

(defun invert
  ((`(#m(pitch ,p velocity ,v duration ,dur delay ,del) . ,tail))
   (++ tail (list (um.note:make (+ 12 p) v dur del)))))

(defun play
  ((device channel '())
   'ok)
  ((device channel (= `(,head . ,_) notes)) (when (is_atom head))
   (play device channel (um.note:make notes)))
  ((device channel `(#m(pitch ,p velocity ,v duration ,dur delay ,del) . ,tail))
   (let ((note-on (midimsg:note-on channel p v))
         (note-off (midimsg:note-on channel p 0)))
     (timer:apply_after del 'um.ml 'send `(,device ,note-on))
     (timer:apply_after (- dur del) 'um.ml 'send `(,device ,note-off))
     (play device channel tail))))

(defun default-opts ()
  `#m(delay 250
      repeats 0
      sustain false))

(defun play-chords (device channel chords)
  (play-chords device channel chords (default-opts)))

(defun play-chords (device channel chords opts)
  (play-chords device channel chords opts '()))

(defun play-chords
  ((device channel '() `#m(repeats 0) _)
   'ok)
  ((device channel '() (= `#m(repeats ,rs) opts) acc)
   (play-chords device channel acc (mupd opts 'repeats (- rs 1)) '()))
  ((device channel `(,head . ,tail) (= `#m(sustain ,sus delay ,del) opts) acc)
   (if sus (um.cc:sustain-on device channel))
   (play device channel (sort head))
   ;; TODO: this is a hack; we need to send timing data MIDI messages ...
   (timer:sleep del)
   (if sus (um.cc:sustain-off device channel))
   (play-chords device channel tail opts (++ acc `(,head)))))

(defun lengthen (chord duration-multiplier)
  (um.note:lengthen chord duration-multiplier))

(defun delay (chord delay-multiplier)
  (um.note:delay chord delay-multiplier))

(defun sort (chord)
  (sort chord 'delay))

(defun sort (chord sort-key)
  (sort chord sort-key 'asc))

(defun sort
  ((chord sort-key 'asc)
   (lists:sort (lambda (a b) (< (mref a sort-key) (mref b sort-key))) chord))
  ((chord sort-key 'desc)
   (lists:sort (lambda (a b) (> (mref a sort-key) (mref b sort-key))) chord)))
