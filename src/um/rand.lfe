(defmodule um.rand
  (export
   ))

(defun play-pitch (pitch velocity)
  (undermidi:send (midimsg:note-on pitch velocity)))

(defun play-pitch (pitch velocity duration)
  (undermidi:send (midimsg:note-on pitch velocity))
  (timer:sleep duration)
  (undermidi:send (midimsg:note-off pitch)))

(play-pitch (rand:uniform 128) 40 1000)

(defun piano-range ()
  #(21 108))

(defun midi ()
  (rand:uniform 128))

(defun piano ()
  (+ 21 (rand:uniform 88)))

