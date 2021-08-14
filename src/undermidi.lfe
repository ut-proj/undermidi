(defmodule undermidi
  (export
   (panic 0)
   (start 0)
   (stop 0))
  (export
   (example 0) (example 1)
   (list-devices 0)
   (midi 1)
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
  (let ((cfg-file "config/sys.config"))
    (io:format "~s" (list (undermidi.util:banner)))
    (logjam:set-handler-from-config cfg-file)
    (logger:set_application_level 'undermidi (logjam:read-log-level cfg-file))
    (log-notice "Starting undermidi ...")
    (application:ensure_all_started 'undermidi)
    (lfe_io:format "\nVersions:\n~p\n" (list (versions)))))

(defun stop ()
  (application:stop 'undermidi))

(defun quit ()
  (log-info "Stopping OTP application ...")
  (application:stop 'undermidi)
  (timer:sleep 500)
  (log-info "Stopping exec process manager ...")
  (application:stop 'erlexec)
  (timer:sleep 500)
  (log-notice "undermidi shutdown complete")
  (init:stop))

(defun panic ()
  (stop)
  (start))

;;; Aliases

(defun example ()
  (example #m(device 0 channel 0 pitch 48 velocity 100 duration 4)))

(defun example (opts)
  (undermidi.supervisor:example opts))

(defun list-devices ()
  (undermidi.supervisor:list-devices))

(defun midi (data)
  (undermidi.supervisor:midi data))

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
  ;;  (let ((`#(result ,go-app-vsn) (version-midiserver)))
  ;;    (++ (undermidi.util:versions)
  ;;        `(#(midiserver ,go-app-vsn))))
  (undermidi.util:versions))
