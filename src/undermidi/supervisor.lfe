(defmodule undermidi.supervisor
  (behaviour supervisor)
  (export
   (start_link 0))
  (export
   (init 1)))

(include-lib "logjam/include/logjam.hrl")

(defun SERVER () (MODULE))
(defun midi-devices () 'undermidi.devices)
(defun device-manager () 'undermidi.device.supervisor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   OTP Supervisor   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting undermidi top-level supervisor ...")
  (supervisor:start_link `#(local ,(SERVER)) (MODULE) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Supervisor Callbacks   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (_args)
  `#(ok #(#m(strategy one_for_one
             intensity 3
	           period 60)
            (,(child (device-manager))
             ,(child (midi-devices))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Internal Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun child (mod)
  `#m(id ,mod
      start #(,mod start_link ())
      restart permanent
      shutdown 2000
      type worker
      modules (,mod)))
