(defmodule undermidi.device.supervisor
  (behaviour supervisor)
  (export
   (start_link 0))
  (export
   (init 1)))

(include-lib "logjam/include/logjam.hrl")

(defun SERVER () (MODULE))
(defun NAME () "device connection supervisor")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   OTP Supervisor   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting ~s ..." (list (NAME)))
  (supervisor:start_link `#(local ,(SERVER)) (MODULE) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Supervisor Callbacks   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (_)
  `#(ok #(#m(strategy simple_one_for_one
             intensity 3
	     period 60)
            (#m(id ,(MODULE)
                start #(undermidi.device.conn start_link ())
                restart transient
                type worker
                shutdown brutal_kill
                modules (undermidi.device.conn))))))
