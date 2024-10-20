;;;; This module defines wrapper functions for midilib (ml == midilib)
(defmodule um.ml
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun batch (device terms)
  ;; It is REQUIRED that the `terms` had been built with the device channel
  ;; value before being passed to this function!
  (let ((encoded (midibin:encode terms)))
    (case encoded
      (`#(error ,_) encoded)
      (_ (list-comp ((<- b encoded)) ; Let's do some error checking on
           (um.nif:send device b))   ; each binary ...
         'ok))))

(defun send (device term)
  ;; It is REQUIRED that the `term` had been built with the device channel
  ;; value before being passed to this function!
  ;;(log-debug "Sending term: ~p" (list term))
  (let ((encoded (midibin:encode term)))
    (case encoded
      (`#(error ,_) encoded)
      (_ (progn
           (um.nif:send device encoded)
           'ok)))))