(defmodule um.cc
  (export all))

(defun set (device _channel msg)
  (um.ml:send device msg))

(defun sustain-on (device channel)
  (set device channel (midimsg:cc 64 127)))

(defun sustain-off (device channel)
  (set device channel (midimsg:cc 64 0)))
