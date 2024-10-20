(defmodule um.cc
  (export all))

(defun set (device msg)
  (um.ml:send device msg))

(defun sustain-on (device channel)
  (set device (midimsg:cc channel 64 127)))

(defun sustain-off (device channel)
  (set device (midimsg:cc channel 64 0)))
