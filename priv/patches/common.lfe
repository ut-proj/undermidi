(defmodule patches.common
  (export all))

(defun default-device () "midi_bus_1")
(defun default-device-pid ()
  (let ((`#(ok ,d) (undermidi.devices:new (default-device))))
    d))

(defun prep-chords (chords len-multiplier)
  (list-comp ((<- chord chords))
    (um.chord:lengthen
      (um.chord:make-fuzzy chord)
      len-multiplier)))

(defun prep-chords (chords len-multiplier delay-multiplier variances)
  (list-comp ((<- chord chords))
    (clj:-> chord
            (um.chord:make-fuzzy variances)
            (um.chord:lengthen len-multiplier)
            (um.chord:delay delay-multiplier))))
