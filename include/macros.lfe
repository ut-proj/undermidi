(defmacro midi body
  `(undermidi:batch (list ,@body)))

(defmacro midi-parallel body
  `(undermidi:batch (list ,@body) 'true))

(defmacro send body
  `(undermidi:send
    (undermidi.msg:batch (list ,@body))))

(defmacro send-parallel body
  `(undermidi:send
    (undermidi.msg:batch (list ,@body) 'true)))

;; Same thing as send-parallel, easier to type
(defmacro cast body
  `(undermidi:send
    (undermidi.msg:batch (list ,@body) 'true)))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: macros --| ()
  'ok)