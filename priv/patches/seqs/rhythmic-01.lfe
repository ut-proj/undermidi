(defmodule priv.patches.seqs.rhythmic-01
  (export all))

(defun n8 () 400)           ; 8th note duration, raw
(defun n4 () (* 2 (n8)))    ; quarter note duration, raw
(defun pad () 50)           ; time padding in ms needed to not stomp on MIDI notes
(defun d8 () (- (n8) (pad))) ; eighth note, padded
(defun d4 () (- (n4) (pad))) ; quarter note, padded

;; |1      2       3       4       |1      2       3       4       
;; '   '   '   '   '   '   '   '   '   '   '   '   '   '   '   '   
;; 1/4 -  1/8 1/4  -  1/4  -  1/8  1/4 -   .   .   .   .   .   .   
(defun pattern () ; velocity and duration
  `((64 ,(n4)) (64 ,(n8)) (64 ,(n4)) (64 ,(n4)) (64 ,(n8))
    (64 ,(n4)) (0 ,(n4)) (0 ,(n4)) (0 ,(n4))))

(defun make-notes (notes)
  (lists:zipwith (lambda (x y)
                   (apply #'um.note:make/3 (++ (list x) y)))
                 notes
                 (pattern)
                 'trim))

(defun m1-notes () '(E4 E4 B4 E4 E5 E4 E0 E0 E0))
(defun m2-notes () '(E4 E4 A4 E4 E5 E4 E0 E0 E0))
(defun m3-notes () '(F#4 F#4 C#5 F#4 F#5 F#4 E0 E0 E0))
(defun m4-notes () '(F#4 F#4 D#5 F#4 F#5 F#4 E0 E0 E0))
(defun m5-notes () '(B3 B3 D#5 B3 B4 B3 E0 E0 E0))
(defun m6-notes () '(B3 B3 F#5 B3 B4 B3 E0 E0 E0))


(defun motive1 () (make-notes (m1-notes)))
(defun motive2 () (make-notes (m2-notes)))
(defun motive3 () (make-notes (m3-notes)))
(defun motive4 () (make-notes (m4-notes)))
(defun motive5 () (make-notes (m5-notes)))
(defun motive6 () (make-notes (m6-notes)))

(defun phrase1 () (list (motive1) (motive1) (motive2) (motive1)))
(defun phrase2 () (list (motive3) (motive3) (motive4) (motive3)))
(defun phrase3 () (list (motive5) (motive5) (motive6) (motive5)))

(defun phrases () (++ (phrase1) (phrase2) (phrase1) (phrase3) (phrase1)))

;; This style of playing MIDI notes is notation-based, and is a prototype
;; for a feature not yet present in undermidi ...
(defun play (device-conn)
  (list-comp ((<- phrase (phrases)))
    (list-comp ((<- n phrase))
        (progn 
          (undermidi:play-note device-conn n)
          (timer:sleep (mref n 'duration)))))
  'ok)

(defun repeat
  ((_ 0)
   'ok)
  ((device-conn count)
   (play device-conn)
   (repeat device-conn (- count 1))))

