(defmacro midi body
  `(midimsg:batch (list ,@body)))

(defmacro send body
  `(undermidi:send
    (midimsg:batch (list ,@body))))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: macros --| ()
  'ok)