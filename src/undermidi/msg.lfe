(defmodule undermidi.msg
  (export
   (batch 1) (batch 2)))

(include-lib "logjam/include/logjam.hrl")

(defun batch (msgs)
  (midimsg:batch msgs))

(defun batch (msgs parallel?)
  (let ((uuid (lutil-uuid:v4)))
    (log-debug "batch id: ~p" (list uuid))
    (midimsg:batch msgs
                   `(#(id ,uuid)
                     #(parallel? ,parallel?)))))
