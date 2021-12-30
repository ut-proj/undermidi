;;;; This is a module which provides conventience aliases to undermidi.clock.ext.
(defmodule um.extclock
  (export all))

(defun bpm () (undermidi.clock.ext:bpm))
(defun bpm (last-n) (undermidi.clock.ext:bpm last-n))
(defun bpm-max () (undermidi.clock.ext:bpm-max))
(defun reset () (undermidi.clock.ext:reset))
(defun tick () (undermidi.clock.ext:tick))
(defun times () (undermidi.clock.ext:times))
(defun times-count () (undermidi.clock.ext:times-count))
(defun total () (undermidi.clock.ext:total))