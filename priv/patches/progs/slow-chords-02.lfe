(defmodule patches.progs.slow-chords-02
  (export all))

(defun cd1 () '(D3 F3 A3))
(defun cd2 () '(D3 G3 B3))
(defun cd3 () '(E3 G3 C4))
(defun cd4 () '(F3 A3 C4))
(defun cd5 () '(E3 A3 C4))
(defun cd6 () '(F3 A3 D4))
(defun cd7 () '(G3 D4 B4))
(defun cd8 () '(A3 E4 C5))
(defun cd9 () '(A3 F4 C5))
(defun cd10 () '(A3 D4 F5))
(defun cd11 () '(G3 D4 B5))
(defun cd12 () '(E3 C4 A5))
(defun cd13 () '(F3 D4 A5))
(defun cd14 () '(F3 D4 A4))
(defun cd15 () '(B2 D4 G4))
(defun cd16 () '(E2 C4 G4))
(defun cd17 () '(A1 C4 F4))
(defun cd18 () '(D1 A2 F3 D4))

(defun all-chords ()  (list (cd1) (cd2) (cd3) (cd4)
                            (cd1) (cd2) (cd5) (cd4)
                            (cd6) (cd7) (cd8) (cd9)
                            (cd10) (cd11) (cd12) (cd13)
                            (cd14) (cd15) (cd16) (cd17) (cd18)))

(defun play ()
  (play (patches.common:default-device-pid)))

(defun play (device-pid)
  (undermidi:play-chords device-pid
                         (patches.common:prep-chords (all-chords)
                                                     60
                                                     8
                                                     #m(velocity 80
                                                        delay 2000))
                         #m(delay 8400
                            repeats 0
                            sustain true)))
