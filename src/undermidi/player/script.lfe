;;;; This module holds various functions that support LFE scripts relating to
;;;; the undermidi playlist capability.
(defmodule undermidi.player.script
  (export all))

(defun parse-args (args)
  (parse-args args '()))

(defun parse-args
  (('() acc)
   (merge-defaults acc))
  ((`(,head . ,tail) acc)
   (parse-args tail (++ acc (parse-arg head)))))

(defun parse-arg (arg)
  (let ((`(,k ,v) (string:split (binary_to_list arg) ":")))
    `(#(,(list_to_atom k) ,v))))

(defun merge-defaults (parsed)
  (let* ((args (maps:merge (undermidi.player.queue:default-entry)
                           (maps:from_list parsed)))
         (source (mref args 'source)))
    (if (and (== "mod" (mref args 'type))
             (is_list source))
      (mset args 'source (list_to_atom source))
      args)))
