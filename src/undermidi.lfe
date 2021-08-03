(defmodule undermidi
  (export
   (start 0))
  (export
   (example 0)
   (list-devices 0)
   (ping 0)
   (quit 0)
   (send 1)
   (state 0)
   (stop-port 0)
   (version 0)
   (version-midiserver 0)
   (versions 0)))

(include-lib "logjam/include/logjam.hrl")

(defun start ()
  (logger:set_primary_config #m(level all))
  (logjam:set-handler-from-config "config/sys.config")
  (log-notice "Starting undermidi ...")
  (application:ensure_all_started 'undermidi))

(defun stop ()
  (application:stop 'undermidi))

(defun quit ()
  (log-info "Stopping OTP application ...")
  (application:stop 'undermidi)
  (timer:sleep 500)
  (log-info "Stopping exec process manager ...")
  (application:stop 'erlexec)
  (timer:sleep 500)
  (log-notice "undertone shutdown complete")
  (init:stop))

;;; Aliases

(defun example ()
  (undermidi.supervisor:example))

(defun list-devices ()
  (undermidi.supervisor:list-devices))

(defun ping ()
  (undermidi.supervisor:ping))

(defun send (msg)
  (undermidi.supervisor:send msg))

(defun state ()
  (undermidi.supervisor:state))

(defun stop-port ()
  (undermidi.supervisor:stop-port))

(defun version ()
  (undermidi.util:version))

(defun version-midiserver ()
  (undermidi.supervisor:version))

(defun versions ()
  (let ((`#(result ,go-app-vsn) (version-midiserver)))
    (++ (undermidi.util:versions)
        `(#(midiserver ,go-app-vsn)))))
