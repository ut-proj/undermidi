(defmodule priv.seqs.wind-chimes01
  (export all))

;; If you use Kontakt, from Native Instruments, this is a free
;; wind-chime sampled instrument (and what I used to test this):
;; * https://pulse.audio/product/wind-chimes-by-jon-meyer-sounds/

(defun device () "midi_bus_1")
(defun device-pid ()
  (let ((`#(ok ,d) (undermidi.devices:new (device))))
    d))

(defun notes () '(A2 D3 F3 G3 A3 D4 F4 A4 D5 A5))
(defun seq-len () 100)
(defun v-variation () 20)
(defun delay-variation () 1.0)
(defun v-start () 48)
(defun delay-start () 0.5)

(defun choices ()
  (choices (notes) (seq-len)))
  
(defun choices (notes count)
  (list-comp ((<- _ (lists:seq 1 count)))
    (+ 1 (round (* (- (length notes) 1) (rand:uniform))))))

(defun new-delay (last-delay)
  (let ((d (+ last-delay (rand:normal 0 (delay-variation)))))
    (cond
     ((> d 8) 4)
     ((> d 0) d)
     ('true (* (rand:uniform) 0.1)))))

(defun new-velocity (last-v)
  (let ((v (trunc (+ last-v (rand:normal 0 (v-variation))))))
    (cond 
     ((> v 100) 100)
     ((< v 1) 1)
     ('true  v))))

(defun note-duration (delay)
  (let ((d (* delay 1000)))
    (cond
     ((>= d 100) 100)
     ((< d 10) 10)
     ((< d 100) (round (- d 10)))
     ('true 100))))
      
(defun play ()
  (play (device-pid) (choices) (v-start) (delay-start)))

(defun play
 ((_ '() _ _)
  'ok)
 ((pid `(,choice . ,choices) last-v last-delay)
  (let* ((v (new-velocity last-v))
         (delay (new-delay last-delay))
         (dur (note-duration delay))
         (n (um.note:make (lists:nth choice notes) v dur)))
    (lfe_io:format "note: ~p (delay: ~.2fs; remaining: ~p)~n"
                   (list n (float delay) (length choices)))
    (undermidi:play-note pid n)
    (timer:sleep (round (* 1000 delay)))
    (play choices v delay))))

