(defmodule undermidi
  (export
   (start 0)
   (stop 0)
   (restart 0))
  (export
   (play-note 2)
   (play-notes 2) (play-notes 3) (play-notes 4)
   )
  (export
   (list-devices 0)
   (version 0)
   (versions 0)))

(include-lib "logjam/include/logjam.hrl")

(defun start ()
  (let ((cfg-file "config/sys.config"))
    (io:format "~s" (list (undermidi.util:banner)))
    (logjam:set-config `#(path ,cfg-file))
    (log-notice "Starting undermidi, version ~s ..." (list (undermidi:version)))
    (application:ensure_all_started 'undermidi)
    (log-debug "\nVersions:\n~p\n" (list (versions)))))

(defun stop ()
  (application:stop 'undermidi))

(defun restart ()
  (stop)
  (start))

;;; Notes API

(defun play-note (pid note)
  (undermidi.device.conn:apply pid 'um.note 'play-note (list (um.note:make note))))

(defun play-notes (pid notes)
  (undermidi.device.conn:apply pid 'um.note 'play-notes (list (um.note:make notes))))

(defun play-notes (pid notes delay)
  (undermidi.device.conn:apply pid 'um.note 'play-notes (list (um.note:make notes) delay)))

(defun play-notes (pid notes delay repeats)
  (undermidi.device.conn:apply pid 'um.note 'play-notes (list (um.note:make notes) delay repeats)))

;;; Aliases

(defun list-devices ()
  (um.nif:list-devices))

(defun version ()
  (undermidi.vers:version))

(defun versions ()
  (undermidi.vers:versions))
