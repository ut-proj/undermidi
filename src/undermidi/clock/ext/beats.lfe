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
  '(set named_table public #(write_concurrency true) #(read_concurrency true)))

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
        bpms (#(,(bpm) ,now))
        transport ())))

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
   (log-debug "~p" (list (transport-start name)))
  `#(noreply ,state))
  ((`#(track-stop) (= `#m(current-track ,name) state))
   (transport-stop name)
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
  (new-track name (default-time-sig))
  (set-track name))

(defun new-track (name time-sig)
  (gen_server:cast (SERVER) `#(track-create ,name ,time-sig)))

(defun set-track (name)
  (gen_server:cast (SERVER) `#(track-current ,name)))

(defun start-track ()
  (um:rt-start)
  (gen_server:cast (SERVER) `#(track-start)))

(defun stop-track ()
  (um:rt-stop)
  (gen_server:cast (SERVER) `#(track-stop)))

(defun list ()
  (gen_server:call (SERVER) `#(tracks-names)))

(defun data ()
  (let ((base (gen_server:call (SERVER) `#(track-data))))
    (maps:merge
     base
     `#m(current-beat ,(beat)
         current-measure ,(measure)
         duration ,(duration)
         running? ,(started?)))))

(defun track ()
  (gen_server:call (SERVER) `#(track-name)))

(defun transport ()
  (mref (gen_server:call (SERVER) `#(track-data)) 'transport))

(defun started? ()
  (transport-running? (transport)))

(defun stopped? ()
  (transport-stopped? (transport)))

(defun started-at ()
  (let ((first (first-transport (transport))))
    (case first
      ('() #(error not-started))
      (_ (mref first 'time)))))
   
(defun time-sig ()
  (element 1 (lists:last (mref (gen_server:call (SERVER) `#(track-data))
                               'time-sigs))))

(defun measure-length ()
  (element 1 (time-sig)))

(defun measure ()
  (let ((b (beat)))
    (if (== b 0)
      0
      (+ 1 (floor (/ b (measure-length)))))))

(defun beat ()
  ;; TODO: take into account all tempo changes, transport stop/starts, etc.
  (let* ((tr (transport))
         (last-tr (last-transport tr)))
    (cond
     ((not (is_map last-tr))
      0)
     ((transport-stopped? tr)
      (let* ((start-time (mref (car tr) 'time))
             (end-time (mref last-tr 'time)))
        (calc-beats end-time start-time)))
     ('true
      (let ((start-time (mref last-tr 'time)))
        (calc-beats (erlang:timestamp) start-time))))))

(defun duration ()
  ;; TODO: take into account all transport stop/starts
  (case (started-at)
    (`#(error ,_) "00:00:00")
    (started (let* ((tr (transport))
                    (last-tr (last-transport tr)))
               (cond
                ((not (is_map last-tr))
                 "00:00:00")
                ((== (mref last-tr 'action) 'stop)
                 (let ((start-time (mref (car tr) 'time))
                       (end-time (mref last-tr 'time)))
                   (um.time:duration end-time start-time #(formatted))))
                ('true
                 (let ((end-time (erlang:timestamp)))
                   (um.time:duration end-time started #(formatted)))))))))

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
;;;::=-   Database functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Low-level data functions

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

;; Wrapper data functions

(defun add-track (track-name time-sig)
  (add-row (init-row track-name time-sig)))

(defun track (track-name)
  (get-row track-name))

(defun get-names ()
  (lists:map (lambda (data) (mref data 'name))
             (get-rows)))

(defun add-time-change (track-name time-sig)
  ;; TODO: don't add a time change if new == last
  (let* ((old-data (get-row track-name))
         (new-time-sigs (lists:append (mref old-data 'time-sigs)
                                      `(#(,time-sig ,(erlang:timestamp)))))
         (new-data (maps:merge old-data `#m(time-sigs ,new-time-sigs))))
    (update-row track-name new-data)))

(defun add-tempo-change (track-name)
  ;; TODO: don't add a tempo change if new == last
  (let* ((old-data (get-row track-name))
         (new-tempos (lists:append (mref old-data 'bpms)
                                   `(#(,(bpm) ,(erlang:timestamp)))))
         (new-data (maps:merge old-data `#m(bpms ,new-tempos))))
    (update-row track-name new-data)))

(defun transport-start (track-name)
  (let* ((old-data (get-row track-name))
         (old-transport (mref old-data 'transport)))
    (log-debug "old-data: ~p" (list old-data))
    (log-debug "old-transport: ~p" (list old-transport))
    (if (andalso (=/= (length old-transport) 0)
                 (== (last-transport-action old-transport) 'start))
      (let ((err #(error already-started)))
        (log-error "couldn't start transport: ~p" (list err))
        err)
      (let* ((new-transport (lists:append (mref old-data 'transport)
                                          `(#m(action start
                                               time ,(erlang:timestamp)))))
             (new-data (maps:merge old-data `#m(transport ,new-transport))))
        (log-debug "new-data: ~p" (list new-data))
        (update-row track-name new-data)))))

(defun transport-stop (track-name)
  (let* ((old-data (get-row track-name))
         (old-transport (mref old-data 'transport)))
    (log-debug "old-data: ~p" (list old-data))
    (log-debug "old-transport: ~p" (list old-transport))
    (cond
     ((=/= (last-transport-action old-transport) 'start) #(error already-stopped))
     ((== old-data '()) #(error not-started))
     ('true (let* ((new-transport (lists:append old-transport
                                                `(#m(action stop
                                                     time ,(erlang:timestamp)))))
                   (new-data (maps:merge old-data `#m(transport ,new-transport))))
              (update-row track-name new-data))))))

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility / support functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun first-transport (transport)
  (if (== transport '())
    '()
    (car transport)))

(defun last-transport (transport)
  (if (== transport '())
    '()
    (lists:last transport)))

(defun transport-stopped? (transport)
  (cond
   ((== transport '()) 'true)
   ((== (mref (lists:last transport) 'action) 'stop) 'true)
   ('true 'false)))

(defun transport-running? (transport)
  (cond
   ((== transport '()) 'false)
   ((== (mref (lists:last transport) 'action) 'start) 'true)
   ('true 'false)))

(defun last-transport-action ()
  (last-transport-action (mref (data) 'transport)))

(defun last-transport-action (transport)
  (if (== transport '())
    #(error no-transport-data)
    (mref (lists:last transport) 'action)))

(defun calc-beats (end-ts start-ts)
  (floor (* (um.time:duration end-ts start-ts #(minutes)) (bpm))))
