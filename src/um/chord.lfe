(defmodule um.chord
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun pitches
  ((chord-name key) (when (is_atom chord-name))
   (pitches (mref (uth.chord:all) chord-name) key))
  ((chord key)
   (um.note:template->pitches chord key)))

(defun create (note-names)
  (um.note:get-pitches note-names))

(defun create (chord key oct)
  (let ((midi-notes (pitches chord key)))
     (lists:flatten
      (list
         (list-comp ((<- pitch midi-notes))
           (um.note:octave pitch oct))))))

(defun create (mode index key oct)
  (create
   (uth.chord:mode mode index)
   key
   oct))

(defun data
  ((pitches velocity) (when (is_integer velocity))
   (data pitches (lists:duplicate (length pitches) velocity)))
  ((pitches velocities)
   (let* ((pitches+velocities (lists:zip pitches velocities))
          (notes-on (list-comp ((<- `#(,p ,vel) pitches+velocities))
                      (midimsg:note-on p vel)))
          (notes-off (list-comp ((<- `#(,p ,_) pitches+velocities))
                       (midimsg:note-off p))))
     (log-debug "Chord notes: ~p" (list notes-on))
     `#m(notes-on ,(undermidi.msg:batch notes-on 'true)
         notes-off ,(undermidi.msg:batch notes-off 'true)))))

(defun play
  ((`#m(notes-on ,on notes-off ,off) duration)
   (undermidi:send on)
   (timer:sleep duration)
   (undermidi:send off)))

(defun play
  (((= `(,first . ,_) note-names) velocity duration) (when (is_atom first))
   (play (create note-names) velocity duration))
  ((pitches velocity duration)
   (play (data pitches velocity) duration)))

(defun play (chord-name key oct velocity duration)
  (play (create chord-name key oct) velocity duration))

(defun play (mode index key oct velocity duration)
  (play (create mode index key oct) velocity duration))
