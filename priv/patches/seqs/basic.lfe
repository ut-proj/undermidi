(defmodule priv.patches.seqs.basic
  (export all))

(defun seq-1a ()
  '(C3 C3 Eb3 C3 C3 C3 C3 C4)) ; i

(defun seq-1b ()
  '(C3 C3 Eb3 C3 C3 Bb3 C3 C4)) ; i

(defun seq-1c ()
  '(C4 C4 Eb4 C4 C4 Bb4 C4 C5)) ; i

(defun seq-2a ()
  '(D3 D3 F3 D3 C3 A2 D3 D4)) ; i/ii

(defun seq-2b ()
  '(D3 D3 F3 D3 Bb2 A2 D3 D4)) ; i/ii

(defun seq-2c ()
  '(D3 D3 F3 D3 C3 Ab2 D3 D4)) ; ii

(defun seq-2c-hi ()
  '(D4 D4 F4 D4 C4 Ab3 D4 D5)) ; ii

(defun seq-2d ()
  '(D3 D3 F3 D3 Bb2 Ab2 D3 D4)) ; ii

(defun seq-2d-hi ()
  '(D4 D4 F4 D4 Bb3 Ab3 D4 D5)) ; ii

(defun seq-2e ()
  '(C4 C4 Eb4 C4 Bb3 A3 C4 C5)) ; i

(defun seq-3a ()
  '(E3 G3 C4 B3 E4 G4 C5 Bb4)) ; ii/ii

(defun seq-3b ()
  '(A3 C4 F4 E4 A4 C5 F5 E5)) ;; v/ii

(defun seqs-1a ()
  (++ (um.seq:repeat (seq-1a) 8)
      (um.seq:repeat (seq-1b) 4)
      (um.seq:repeat (seq-1a) 2)
      (um.seq:repeat (seq-1b) 2)
      (um.seq:repeat (seq-2d) 4)
      (um.seq:repeat (seq-1a) 2)
      (um.seq:repeat (seq-1b) 2)
      (um.seq:repeat (seq-3a) 4)
      (um.seq:repeat (seq-3b) 4)
      (um.seq:repeat (seq-2b) 2)
      (um.seq:repeat (seq-2a) 2)
      (um.seq:repeat (seq-2c-hi) 2)
      (um.seq:repeat (seq-2d-hi) 2)
      (um.seq:repeat (seq-2e) 2)
      (um.seq:repeat (seq-1c) 2)
      (um.seq:repeat (seq-1b) 4)))

(defun play (device-conn)
  (list-comp ((<- seq (seqs-1a))) (undermidi:play-notes device-conn seq))
  'done)
