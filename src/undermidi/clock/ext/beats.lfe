;;;; This module is for keeping track of an external clock's ticks as well
;;;; as providing ccconvenience functions that help calculate things like
;;;; measures, beats, etc.,
(defmodule undermidi.clock.ext.beats
  (behaviour gen_server)
  (export all))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   config functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SERVER () (MODULE))
(defun table-name () 'extbeats)
(defun table-desc () "External MIDI clock beats table")
(defun genserver-opts () '())
(defun default-time-sig () #(4 4))

(defun table-options ()
  '(set named_table public))

(defun initial-state ()
  `#m(name ,(table-name)
      table-name ,(table-name)
      table-desc ,(table-desc)
      controlling-process ,(MODULE)))

(defun init-track (name time-sig)
  (let ((now (erlang:timestamp)))
    `#m(name ,name
        created-at ,now
        time-sigs (#(,time-sig ,now)))))

(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting undermidi external MIDI clock beats server ...")
  (gen_server:start_link `#(local ,(SERVER))
                         (MODULE)
                         (initial-state)
                         (genserver-opts)))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;;;;::=---------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   callback implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init
  (((= `#m(table-name ,table-name) state))
   (ets:new table-name (table-options))
   (log-debug (undermidi.util:table-info table-name))
   `#(ok ,state)))

(defun handle_cast
  ((_msg state)
   `#(noreply ,state)))

(defun handle_call
  ;; Metadata
  ((#(table-info) _from state)
   `#(reply ,(undermidi.util:table-info state) ,state))
  ;; Stop
  (('stop _from (= `#m(session ,sess) state))
   (ets:delete (mref sess 'table))
   `#(stop normal ok ,state))
  ;; Testing / debugging
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
  ((#(ping) _from state)
   `#(reply PONG ,state))
  ((#(state) _from state)
   `#(reply ,state ,state))
  ;; Fall-through
  ((message _from state)
   `#(reply ,(unknown-command (io_lib:format "~p" `(,message))) ,state)))

(defun handle_info
  ((`#(EXIT ,_from normal) state)
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (log-notice "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state))
  ((_msg state)
   `#(noreply ,state)))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;;;;::=------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Beats API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bpm () (undermidi.clock.ext:bpm))
(defun bpm (last-n) (undermidi.clock.ext:bpm last-n))
(defun bpm-max () (undermidi.clock.ext:bpm-max))

(defun new-track (name)
  (new-track name (default-time-sig)))

(defun new-track (name time-sig)
  (gen_server:call (SERVER) `#(track-create ,name ,time-sig)))

(defun start-track (name)
  (gen_server:call (SERVER) `#(track-start ,name)))

(defun stop-track (name)
  (gen_server:call (SERVER) `#(track-stop ,name)))

(defun list ()
  (gen_server:call (SERVER) `#(tracks-names)))

(defun data (name)
  (let ((base (gen_server:call (SERVER) `#(track ,name))))
    (maps:merge
     base
     `#m(current-beat 0
         current-measure 0))))

(defun started-at (name)
  (mref (gen_server:call (SERVER) `#(track ,name)) 'started-at))

(defun time-sig (name)
  (lists:last (mref (gen_server:call (SERVER) `#(track ,name)) 'time-sigs)))

(defun measure (name)
  (mref (data name) 'current-measure))

(defun beat (name)
  (mref (data name) 'current-beat))

(defun time-change (name time-sig)
  (gen_server:call (SERVER) `#(track-time-change ,name ,time-sig)))

(defun export-track (name)
  ;; TODO: save term data to temp file
  'todo)

(defun dump ()
  ;; TODO: save ets table to temp file
  'todo)

;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   metadata API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun table-info ()
  (gen_server:call (SERVER) #(table-info)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

(defun pid ()
  (erlang:whereis (SERVER)))

(defun ping ()
  (gen_server:call (SERVER) #(ping)))

(defun state ()
  (gen_server:call (SERVER) #(state)))

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility / support functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

