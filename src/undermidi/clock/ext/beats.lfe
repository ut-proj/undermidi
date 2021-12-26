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
  `#m(current-track 'undefined
      name ,(table-name)
      table-name ,(table-name)
      table-desc ,(table-desc)
      controlling-process ,(MODULE)))

(defun init-row (name time-sig)
  (let ((now (erlang:timestamp)))
    `#m(name ,name
        created-at ,now
        time-sigs (#(,time-sig ,now))
        bpms (#(,(bpm) ,now)))))

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
  ;; API
  ((`#(track-create ,name ,time-sig) state)
   (add-track name time-sig)
  `#(noreply ,state))
  ((`#(track-current ,name) state)
  `#(noreply ,(maps:merge state `#m(current-track ,name))))
  ((`#(track-start) (= `#m(current-track ,name) state))
   (merge-row name `#m(started-at ,(erlang:timestamp)))
  `#(noreply ,state))
  ((`#(track-stop ,name) (= `#m(current-track ,name) state))
   (merge-row name `#m(stopped-at ,(erlang:timestamp)))
  `#(noreply ,state))
  ((`#(track-time-change ,time-sig) (= `#m(current-track ,name) state))
   (add-time-change name time-sig)
  `#(noreply ,state))
  ((`#(track-tempo-change) (= `#m(current-track ,name) state))
   (add-tempo-change name)
   `#(noreply ,state))
  ;; Fall-through
  ((_msg state)
   `#(noreply ,state)))

(defun handle_call
  ;; API
  ((`#(tracks-names) _from state)
   `#(reply ,(get-names) ,state))
  ((`#(track-data) _from (= `#m(current-track ,name) state))
   `#(reply ,(get-row name) ,state))
  ((`#(track-name) _from (= `#m(current-track ,name) state))
   `#(reply ,name ,state))
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

(defun new-track (name)
  (new-track name (default-time-sig)))

(defun new-track (name time-sig)
  (gen_server:cast (SERVER) `#(track-create ,name ,time-sig)))

(defun set-track (name)
  (gen_server:cast (SERVER) `#(track-current ,name)))

(defun start-track ()
  (gen_server:cast (SERVER) `#(track-start)))

(defun stop-track ()
  (gen_server:cast (SERVER) `#(track-stop)))

(defun list ()
  (gen_server:call (SERVER) `#(tracks-names)))

(defun data ()
  (let ((base (gen_server:call (SERVER) `#(track-data))))
    (maps:merge
     base
     `#m(current-beat ,(beat)
         current-measure ,(measure)
         duration ,(duration)))))

(defun track ()
  (gen_server:call (SERVER) `#(track-name)))

(defun started-at ()
  (mref (gen_server:call (SERVER) `#(track-data)) 'started-at))

(defun time-sig ()
  (element 1 (lists:last (mref (gen_server:call (SERVER) `#(track-data))
                               'time-sigs))))

(defun measure-length ()
  (element 1 (time-sig)))

(defun measure ()
  (+ 1 (floor (/ (beat) (measure-length)))))

(defun beat ()
  ;; TODO: take into account all tempo changes
  (let* ((now (erlang:timestamp))
         (start (started-at))
         (run-time (/ (timer:now_diff now start) 60000000)))
    (floor (* run-time (bpm)))))

(defun duration ()
  (let* ((ms (timer:now_diff (erlang:timestamp) (started-at)))
         (time (calendar:seconds_to_time (floor (/ ms 1000000)))))
    (io_lib:format "~B:~B:~B" (tuple_to_list time))))

(defun time-change (time-sig)
  (gen_server:cast (SERVER) `#(track-time-change ,time-sig)))

(defun tempo-change ()
  (gen_server:cast (SERVER) `#(track-tempo-change ,(bpm))))

(defun export-track ()
  ;; TODO: save term data to temp file
  (export-track (track)))

(defun export-track (name)
  ;; TODO: save term data to temp file
  'todo)

(defun dump ()
  ;; TODO: save ets table to temp file
  'todo)

;; Aliases to parent module

(defun bpm () (undermidi.clock.ext:bpm))
(defun bpm (last-n) (undermidi.clock.ext:bpm last-n))
(defun bpm-max () (undermidi.clock.ext:bpm-max))

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

;; Low-level

(defun add-row (data)
  (ets:insert (table-name) `#(,(mref data 'name) ,data)))

(defun get-row (track-name)
  (ets:lookup_element (table-name) track-name 2))

(defun update-row (track-name data)
  (ets:update_element (table-name) track-name `#(2 ,data)))

(defun merge-row (track-name new-data)
  (update-row track-name (maps:merge (get-row track-name) new-data)))

(defun get-rows ()
  (lists:map (lambda (row) (element 2 row))
             (ets:tab2list (table-name))))

;; Wrappers

(defun add-track (track-name time-sig)
  (add-row (init-row track-name time-sig)))

(defun track (track-name)
  (get-row track-name))

(defun get-names ()
  (lists:map (lambda (data) (mref data 'name))
             (get-rows)))

(defun add-time-change (track-name time-sig)
  (let* ((old-data (get-row track-name))
         (new-time-sigs (lists:append (mref old-data 'time-sigs)
                                      `(#(,time-sig ,(erlang:timestamp)))))
         (new-data (maps:merge old-data `#m(time-sigs ,new-time-sigs))))
    (update-row track-name new-data)))

(defun add-tempo-change (track-name)
  (let* ((old-data (get-row track-name))
         (new-tempos (lists:append (mref old-data 'bpms)
                                   `(#(,(bpm) ,(erlang:timestamp)))))
         (new-data (maps:merge old-data `#m(bpms ,new-tempos))))
    (update-row track-name new-data)))
