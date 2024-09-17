(defmodule undermidi
  (export
   (start 0)
   (stop 0)
   (restart 0))
  (export
   (example 0) (example 1)
   (list-devices 0)
   (version 0)
   (versions 0)))

(include-lib "logjam/include/logjam.hrl")

(defun start ()
  (let ((cfg-file "config/sys.config"))
    (io:format "~s" (list (undermidi.util:banner)))
    (logjam:set-config `#(path ,cfg-file))
    (log-notice "Starting undermidi, version ~s ..." (list (undermidi.util:version)))
    (application:ensure_all_started 'undermidi)
    (log-debug "\nVersions:\n~p\n" (list (versions)))))

(defun stop ()
  (application:stop 'undermidi))

(defun restart ()
  (stop)
  (start))

;;; Aliases

(defun example ()
  (example #m(device 0 channel 0 pitch 48 velocity 100 duration 4)))

(defun example (opts)
  (undermidi.supervisor:example opts))

(defun list-devices ()
  (um.nif:list-devices))

(defun version ()
  (undermidi.vers:version))

(defun versions ()
  (undermidi.vers:versions))
