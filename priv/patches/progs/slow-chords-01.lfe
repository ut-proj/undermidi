(defmodule priv.patches.progs.slow-chords-01
  (export all))

(defun cd1 () '(A3 C4 E4 A4))
(defun cd2 () '(A3 C4 F4 A4))
(defun cd3 () '(G3 C4 F4 G4))
(defun cd4 () '(G3 C4 E4 G4))

(defun cd5 () '(F3 C4 E4 F4))
(defun cd6 () '(F3 C4 D4 F4))
(defun cd7 () '(G3 C4 D4 F4))
(defun cd8 () '(G3 C4 D4 G4))

(defun cd9 () '(E3 C4 G4 C5))
(defun cd10 () '(F3 C4 E4 A4))
(defun cd11 () '(A3 B3 C4 G4))
(defun cd12 () '(G3 C4 D4 G4))

(defun cd13 () '(E3 C4 G4 C5))
(defun cd14 () '(F3 C4 E4 A4))
(defun cd15 () '(A3 C4 E4 B4))
(defun cd16 () '(A3 E4 F4 C5))

(defun cd17 () '(A3 E4 B4 D5))
(defun cd18 () '(A3 C4 A4 E5))
(defun cd19 () '(A3 E4))
(defun cd20 () '(E3 C4))

(defun cd21 () '(A2 C3 E3 A3))
(defun cd22 () '(A2 C3 F3 A3))
(defun cd23 () '(G2 C3 F3 G3))
(defun cd24 () '(G2 C3 E3 G3))

(defun cd25 () '(F2 C3 E3 F3))
(defun cd26 () '(F2 C3 D3 F3))
(defun cd27 () '(G2 C3 D3 F3))
(defun cd28 () '(D2 G2 B2 G3))
(defun cd29 () '(C2 G2 E3 C4))

(defun all-chords () (list (cd1) (cd2) (cd3) (cd4)
                           (cd5) (cd6) (cd7) (cd8)
                           (cd9) (cd10) (cd11) (cd12)
                           (cd13) (cd14) (cd15) (cd16)
                           (cd17) (cd18) (cd19) (cd20)
                           (cd21) (cd22) (cd23) (cd24)
                           (cd25) (cd26) (cd27) (cd28) (cd29)
                           ))

(defun play ()
  (play (priv.patches.common:default-device-pid)))

(defun play (device-pid)
  (undermidi:play-chords device-pid
                         (priv.patches.common:prep-chords (all-chords)
                                                          40
                                                          18
                                                          #m(velocity 20
                                                             delay 500))
                         #m(delay 10800
                            repeats 0
                            sustain true)))
