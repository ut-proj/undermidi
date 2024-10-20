#! /usr/bin/env lfescript

;;; --------------------
;;; entry point function
;;; --------------------

(include-lib "logjam/include/logjam.hrl")

(defun main (args)
  (let ((script-name (escript:script_name))
        (args (undermidi.player.script:parse-args args))
        (cfg-file "config/script.config"))
    (logjam:set-config `#(path ,cfg-file))
    (logger:debug "Running script '~s' with args ~p ..." `(,script-name ,args))
    (logger:debug "Args: ~p" `(,(init:get_arguments)))
    (shortcuts.linux:make)))
