(defmodule undermidi.errors
  (export all))

(defun action-cancelled (data) `#(error action-cancelled ,data))
(defun unknown-command (data) `#(error unknown-command ,data))
