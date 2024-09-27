(defun ERR_NO_DEVICE () #(error "no such device; see (um:list-devices) for known system MIDI devices."))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: errors --| ()
  'ok)