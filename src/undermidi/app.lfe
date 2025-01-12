(defmodule undermidi.app
  (behaviour application)
  (export
   (start 2) (start 4)
   (stop 1))
  (export
   (children 0)
   (supervisor 0)))

(include-lib "logjam/include/logjam.hrl")

(defun SUPERVISOR () 'undermidi.supervisor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   OTP Application   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (start-type start-args)
  (let* ((app 'undermidi)
         (cfg-name "config/sys.config")
         (cfg-file (lutil-file:priv app cfg-name))
         (cfg (lutil-file:read-priv-config app cfg-name)))
    (start start-type start-args cfg-file cfg)))

(defun start (_start-type _start-args cfg-file cfg)
  ;; Separating out this function allows us to start up undermidi from other
  ;; applications with more control, etc.
  (logjam:set-config `#(path ,cfg-file))
  (log-info "Starting undermidi OTP application ..." '())
  (um.nif:initialise)
  (if (undermidi.config:display-banner? cfg)
    (io:format "~s" (list (undermidi.util:banner))))
  (log-notice "Starting undermidi, version ~s ..." (list (undermidi:version)))
  (log-debug "\nVersions:\n~p\n" (list (undermidi:versions)))
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
