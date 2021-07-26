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

(defun SUPERVISOR () 'undermidi.supervisor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   OTP Application   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start (_start-type _start-args)
  (logger:set_application_level 'ports 'all)
  (logger:info "Starting application" '())
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
  `(#(go ,(undermidi.go.server:pid))))

(defun ports ()
  `(#(go ,(undermidi.go.server:port))))

(defun info ()
  `(#(app ,(erlang:process_info (self)))
    #(supervisor ,(erlang:process_info (supervisor)))
    #(go (#(server ,(erlang:process_info (undermidi.go.server:pid)))
          #(port ,(erlang:port_info (undermidi.go.server:port)))))))
