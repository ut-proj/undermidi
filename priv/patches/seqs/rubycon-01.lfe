(defmodule patches.seqs.rubycon-01
  (export all))

#|
(progn
  (um:set-device 0)
  (um:set-channel 0)

  (set velocity 80)
  (set bpm 250)
  (set dur (trunc (* (/ 60 bpm) 1000)))
  (set pat1 '(C4 C4 Bb3 G3))
  (set pat2 '(C4 C4 C4 Bb3 G3))
  (set pat3 '(C4 C4 C4 C4 Bb3 G3))
  (set pat4 '(C4 C3 C4 C4 Bb3 G3))
  (set pat5 '(C4 C3 C4 C3 Bb3 G3))

  (set seq1 (um.notes:duplicate pat1 15))
  (set seq2 (um.notes:duplicate pat2 12))
  (set seq3 (um.notes:duplicate pat3 10))
  (set seq4 (um.notes:duplicate pat4 10))
  (set seq5 (um.notes:duplicate pat5 10))

  (set all (lists:append (list seq1 seq2 seq3 seq4 seq5)))
  (timer:sleep 1500)
  (um:set-cc (filter-cutoff-frequency) 16)
  (um:cycle-cc (filter-cutoff-frequency) 16 70 68)
  (um:cycle-cc (filter-emphasis) 0 58 68)
  (um:cycle-cc (osc-3-volume) 127 16 10)
  (um:cycle-cc (noise-volume) 0 28 20)
  (timer:sleep 500)
  (um:play-notes all velocity dur))

|#
