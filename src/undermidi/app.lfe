(defmodule undermidi.app
  (behaviour application)
  (export
   (start 2)
   (stop 1))
  (export
   (children 0)
   (info 0)
   (ports 0)
   (servers 0)
   (supervisor 0)))

(include-lib "logjam/include/logjam.hrl")

(defun SUPERVISOR () 'undermidi.supervisor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   OTP Application   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (_start-type _start-args)
  (log-info "Starting OTP application ..." '())
  (undermidi.supervisor:start_link))

(defun stop (_state)
  (undermidi.supervisor:stop)
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun supervisor ()
  (erlang:whereis (SUPERVISOR)))

(defun children ()
  (supervisor:which_children (SUPERVISOR)))

(defun servers ()
  `(#(go ,(undermidi.supervisor:child-pid))))

(defun ports ()
  `(#(go ,(undermidi.supervisor:child-port))))

(defun info ()
  `(#(app ,(erlang:process_info (self)))
    #(supervisor ,(erlang:process_info (supervisor)))
    #(go (#(server ,(erlang:process_info (undermidi.supervisor:child-pid)))
          #(port ,(erlang:port_info (undermidi.supervisor:child-port)))))))
