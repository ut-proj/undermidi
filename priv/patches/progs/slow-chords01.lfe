(defmodule priv.patches.progs.slow-chords01
  (export all))

(defun cd1 () '(A3 C4 E4 A4))
(defun cd2 () '(A3 C4 F4 A4))
(defun cd3 () '(G3 C4 F4 G4))
(defun cd4 () '(G3 C4 E4 G4))
(defun cd5 () '(F3 C4 E4 F4))
(defun cd6 () '(F3 C4 D4 F4))
(defun cd7 () '(G3 C4 D4 G4))
(defun cd8 () '(G3 B3 A4))
(defun cd9 () '(A3 C4 B4))
(defun cd10 () '(A3 C4 A4 E5))

(defun all-chords () (list (cd1) (cd2) (cd3) (cd4) (cd5) (cd6) (cd7) (cd8) (cd9) (cd10)))

(defun play ()
  (play (priv.patches.common:default-device-pid)))

(defun play (device-pid)
  (undermidi:play-chords device-pid
                         (priv.patches.common:prep-chords (all-chords)
                                                          40
                                                          #m(velocity 20
                                                             delay 500))
                         12800
                         0))
