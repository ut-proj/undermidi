(defmodule undermidi
  (export
   (start 0)
   (stop 0)
   (restart 0))
  (export
   (play-note 2)
   (play-notes 2) (play-notes 3) (play-notes 4)
   (play-chord 2)
   (play-chords 2) (play-chords 3) (play-chords 4)
   )
  (export
   (list-devices 0)
   (version 0)
   (versions 0)))

(include-lib "logjam/include/logjam.hrl")

(defun start ()
  (application:ensure_all_started 'undermidi))

(defun stop ()
  (application:stop 'undermidi))

(defun restart ()
  (stop)
  (start))

(defun list-devices ()
  ;; TODO: format this output as an actual listing
  (let ((groups (maps:to_list (um:devices))))
    (list-comp ((<- g groups))
      (progn
        (io:format "~n~s~n" (list (element 1 g)))
        (list-devices (element 2 g)))))
  'ok)

(defun list-devices (ds)
  (list-comp ((<- `#(,index ,name) ds))
    (io:format "  ~p. ~s~n" (list index name)))
  'ok)

;;; The following functions define an API that wraps functions of the gen_server
;;; that manages the state of individual "connections" to MIDI devices. Due to
;;; the state management of the gen_server, neither device name nore channel are
;;; passed in these functions. If a more functional approach is desired, then the
;;; developer should access directly the various functions that are wrapped here.

;; Notes API

(defun play-note
  ((pid note) (when (is_atom note))
   (play-note pid (um.note:make note)))
  ((pid note)
   (undermidi.device.client:apply pid
                                  'um.note
                                  'play-note
                                  (list note))))

(defun play-notes
  ((pid (= `(,head . ,_) notes)) (when (is_atom head))
   (play-notes pid (um.note:make notes)))
  ((pid notes)
   (undermidi.device.client:apply pid
                                  'um.note
                                  'play-notes
                                  (list notes))))

(defun play-notes
  ((pid (= `(,head . ,_) notes) delay) (when (is_atom head))
   (play-notes pid (um.note:make notes) delay))
  ((pid notes delay)
   (undermidi.device.client:apply pid
                                  'um.note
                                  'play-notes
                                  (list notes delay))))

(defun play-notes
  ((pid (= `(,head . ,_) notes) delay repeats) (when (is_atom head))
   (play-notes pid (um.note:make notes) delay repeats))
  ((pid notes delay repeats)
   (undermidi.device.client:apply pid
                                  'um.note
                                  'play-notes
                                  (list notes delay repeats))))

;; Chords API

(defun play-chord
  ((pid (= `(,head . ,_) notes)) (when (is_atom head))
   (play-chord pid (um.chord:make notes)))
  ((pid chord)
   (undermidi.device.client:apply pid
                                  'um.chord
                                  'play
                                  (list chord))))

(defun play-chords (pid chords)
  (undermidi.device.client:apply pid
                                 'um.chord
                                 'play-chords
                                 (list chords)))

(defun play-chords (pid chords delay)
  (undermidi.device.client:apply pid
                                 'um.chord
                                 'play-chords
                                 (list chords delay)))

(defun play-chords (pid chords delay repeats)
  (undermidi.device.client:apply pid
                                 'um.chord
                                 'play-chords
                                 (list chords delay repeats)))

;; CC

(defun sustain-on (pid)
  (undermidi.device.client:apply pid
                                 'um.cc
                                 'sustain-on
                                 '()))

(defun sustain-off (pid)
  (undermidi.device.client:apply pid
                                 'um.cc
                                 'sustain-off
                                 '()))

;;; Aliases

(defun version ()
  (undermidi.vers:version))

(defun versions ()
  (undermidi.vers:versions))
