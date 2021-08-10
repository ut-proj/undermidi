(defmodule undermidi.msg
  (export
   (batch 1) (batch 2)))

(defun batch (msgs)
  (midimsg:batch msgs))

(defun batch (msgs parallel?)
  (midimsg:batch msgs
                 `(#(id ,(uuid:get_v4_urandom))
                   #(parallel? ,parallel?))))
