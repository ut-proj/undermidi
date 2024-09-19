;;;; TODO: once uth.interval is released, use the logic there and just do
;;;;       pitch/note conversion here
(defmodule um.interval
  (export all))

(defun names ()
  #m(0 P0
     1 m1
     2 M2
     3 m3
     4 M3
     5 P4
     6 a4/dim5
     7 P5
     8 m6
     9 M6
     10 m7
     11 M7
     12 P8
     13 m9
     14 M9
     15 a9/dim10
     16 M10
     17 P11
     18 a11/dim12
     19 P12
     20 m13
     21 M13
     22 m14
     23 M14
     24 P15))

(defun name (note1 note2)
  (name note1 note2 1))

(defun name
  ((note1 note2 octaves) (when (andalso (is_atom note1) (is_atom note2)))
   (name (um.note:get-pitch note1) (um.note:get-pitch note2) octaves))
  ((pitch1 pitch2 octaves)
   (let* ((ps (lists:sort (list pitch1 pitch2)))
          (diff (- (cadr ps) (car ps)))
          (intv (cond
                 ((and (== octaves 2) (== diff 24)) 24)
                 ((== octaves 2) (rem diff 24))
                 ((== diff 12) 12)
                 ('true (rem diff 12)))))
     (mref (names) intv))))
