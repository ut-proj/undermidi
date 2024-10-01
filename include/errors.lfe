(defun ERR-NO-DEVICE () #(error "no such device; see (um:list-devices) for known system MIDI devices."))
(defun ERR-ACTION-CANCELLED (data) `#(error action-cancelled ,data))
(defun ERR-UNKNOWN-COMMAND (data) `#(error unknown-command ,data))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: errors --| ()
  'ok)