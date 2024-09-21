(defmodule undermidi.app
  (behaviour application)
  (export
   (start 2)
   (stop 1))
  (export
   (children 0)
   (supervisor 0)))

(include-lib "logjam/include/logjam.hrl")

(defun SUPERVISOR () 'undermidi.supervisor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   OTP Application   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (_start-type _start-args)
  (log-info "Starting OTP application ..." '())
  (um.nif:initialise)
  (undermidi.supervisor:start_link))

(defun stop (_state)
  (um.nif:deinitialise)
  (undermidi.supervisor:stop)
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun supervisor ()
  (erlang:whereis (SUPERVISOR)))

(defun children ()
  (supervisor:which_children (SUPERVISOR)))
