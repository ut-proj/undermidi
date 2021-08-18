(defmodule um.rand
  (export all))

(defun piano-range ()
  #(21 108))

(defun midi ()
  (rand:uniform 128))

(defun piano ()
  (+ 21 (rand:uniform 88)))

(defun play (velocity duration)
  (play 'piano velocity duration))

(defun play
  (('piano velocity duration)
   (um:play-pitch (piano) velocity duration)))