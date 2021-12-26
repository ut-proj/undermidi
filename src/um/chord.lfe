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

(defun create (key chord oct)
  (create key chord oct #m()))

(defun create
  ((key chord-type oct opts) (when (andalso (is_map opts) (is_atom chord-type)))
   (create key (uth.chord:chord-or-mode chord-type) oct opts))
  ((key chord oct opts) (when (is_map opts))
   (let* ((midi-notes (pitches chord key))
          (inv (invert midi-notes (maps:get 'inversion opts 1))))
     (lists:flatten
      (list
       (list-comp ((<- pitch inv))
         (um.note:octave pitch oct))))))
  ((key mode index oct)
   (create key mode index oct #m())))

(defun create (key mode index oct opts)
  (create
   key
   (uth.chord:mode mode index)
   oct
   opts))

(defun invert
  ((`(,head . ,tail))
   (lists:append tail (list (+ 12 head)))))

(defun invert (midi-notes nth)
  (uth.chord:invert midi-notes nth 0 #'invert/1))

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
   ;; TODO: do a scheduled send instead of a sleep!
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
