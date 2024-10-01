;;;; This gen_server is for keeping track of which devices a user is writing
;;;; MIDI to, and to what channels on those devices.
(defmodule undermidi.player.queue
  (behaviour gen_server)
  ;; gen_server implementation
  (export
   (start_link 0)
   (stop 0))
  ;; callback implementation
  (export
   (code_change 3)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (init 1)
   (terminate 2))
  ;; playlist API
  (export
   (make-entry 1) (make-entry 2) (make-entry 3)
   (default-entry 0)
   (add 1) (add 2) (add 3)
   (device 1)
   (channel 1)
   (clear 0)
   (opt 1) (opt 2)
   (now-playing 0) (now-playing 1)
   (playing? 0) (playing? 1)
   (play-next 1) (play-next 2))
  ;; debug API
  (export
   (dump 0)
   (echo 1)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "undermidi/include/errors.lfe")

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   config functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SERVER () (MODULE))
(defun DELIMITER () #"\n")
(defun NAME () "playlist manager")
(defun genserver-opts () '())
(defun playlist-opts () #m(auto-play? true
                           repeat? false
                           shuffle? false
                           allow-skipping? false))

(defun initial-state () `#m(queue ()
                            played ()
                            now-playing #m()
                            repeats-count 0
                            device ""
                            channel 1
                            opts ,(playlist-opts)))

(defun make-entry (source)
  (make-entry "" source))

(defun make-entry (name source)
  (make-entry name source 'mod))

(defun make-entry (name source type)
  `#m(name ,name
      type ,type
      source ,source
      play-count 0
      skip-count 0))

(defun default-entry ()
  (make-entry "playlist entry"
              'priv.seqs.basic
              "mod"))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting ~s ..." (list (NAME)))
  (gen_server:start_link `#(local ,(SERVER))
                         (MODULE)
                         (initial-state)
                         (genserver-opts)))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;;;;::=---------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   callback implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (state)
  (log-debug "Initialising ~s ..." (list (NAME)))
  (erlang:process_flag 'trap_exit 'true)
  `#(ok ,state))

(defun handle_call
  (('#(state) _from state)
   `#(reply ,state ,state))
  ((`#(add-entry ,entry) _from (= `#m(queue ,q) state))
   (let ((state (mset state 'queue (++ q (list entry)))))
     (if (and (auto-play? state) (not (playing? state)))
       (progn
         (log-debug "Nothing's currently playing and auto-play is enabled; playing added song ...")
         (let ((state (play-next state)))
           `#(reply ,state ,state)))
       `#(reply ,state ,state))))
  ((`#(get-opt ,k) _from (= `#m(opts ,opts) state))
   `#(reply ,(mref opts k) ,state))
  ;; Stop
  (('stop _from state)
   (log-notice "Stopping ~s ..." (list (NAME)))
   `#(stop normal ok ,state))
  ;; Testing / debugging
  (('#(dump) _from state)
   `#(reply ,state ,state))
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
  ;; Fall-through
  ((msg _from state)
   `#(reply ,(ERR-UNKNOWN-COMMAND msg) ,state)))

(defun handle_cast
  ;; Command support
  ((`#(set ,k ,v) state)
   `#(noreply ,(mset state k v)))
  ((`#(add-entry ,entry) (= `#m(queue ,q) state))
   `#(noreply ,(mset state 'queue (++ q (list entry)))))
  (('#(clear-queue) state)
   `#(noreply ,(mset state 'queue ())))
  ((`#(set-opt ,k ,v) (= `#m(opts ,opts) state))
   `#(noreply ,(mset state 'opts (mset opts k v))))
  ((`#(finished ,song) state)
   (log-debug "Got 'finished' message from worker")
   (let ((state (clj:->> state
                         (finish-song song)
                         (play-next))))
     `#(noreply ,state)))
  (((= `(#(command ,_)) cmd) state)
   (log-warn "Unsupported server command: ~p" `(,cmd))
   `#(noreply ,state))
  ((msg state)
   (log-warn "Got undexected cast msg: ~p" (list msg))
   `#(noreply ,state)))

(defun handle_info
  ;; Exit-handling
  ((`#(EXIT ,_from normal) state)
   (logger:info "~s is exiting (normal)." (list (NAME)))
   `#(noreply ,state))
  ((`#(EXIT ,_from shutdown) state)
   (logger:info "~s is exiting (shutdown)." (list (NAME)))
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (log-notice "Process ~p exited! (Reason: ~p)" `(,pid ,reason))
   `#(noreply ,state))
  ;; Fall-through
  ((msg state)
   (log-debug "Unknwon info: ~p" `(,msg))
   `#(noreply ,state)))

(defun terminate
  ((_reason _state)
   (log-notice "Terminating ~s ..." (list (NAME)))
   'ok))

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Playlist API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add
  (((= `#m(name ,name) entry)) (when (is_map entry))
   (log-info "Received playlist request; adding song '~s' ..." (list name))
   (log-debug "Entry: ~p" (list entry))
   (gen_server:call (SERVER) `#(add-entry ,entry)))
  ((source)
   (add "" source)))

(defun add (name source)
  (add (make-entry name source)))

(defun add (name source type)
  (add (make-entry name source type)))

(defun device (name)
  (gen_server:cast (SERVER) `#(set device ,name)))

(defun channel (number)
  (gen_server:cast (SERVER) `#(set channel ,number)))

(defun clear ()
  (gen_server:cast (SERVER) '#(clear-queue)))
  
(defun now-playing ()
  (now-playing (gen_server:call (SERVER) '#(now-playing))))

(defun playing? ()
  (playing? (now-playing)))

(defun opt (k)
  (gen_server:call (SERVER) `#(get-opt ,k)))

(defun opt (k v)
  (gen_server:cast (SERVER) `#(set-opt ,k ,v)))

(defun state ()
  (gen_server:call (SERVER) '#(state)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dump ()
  (gen_server:call (SERVER) '#(dump)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

;;;;;::=------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   supporting functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reset-queue
  (((= `#m(played ,played repeats-count ,count) state))
   (clj:-> state
           (mset 'queue played)
           (mset 'played ())
           (mset 'repeats-count (+ 1 count)))))

(defun finish-song
  (((= `#m(play-count ,count) song) (= `#m(played ,played) state))
   (let* ((song (mset song 'play-count (+ 1 count)))
          (played (++ played (list song))))
     (clj:-> state
             (mset 'now-playing #m())
             (mset 'played played)))))

(defun pop-queue
  (((= `#m(queue () played ,played opts #m(repeat? true)) state))
   (log-debug "Queue is empty and repeat? is true; resetting ...")
   (pop-queue (reset-queue state)))
  (((= `#m(queue (,song . ,tail)) state))
   (let ((state (clj:-> state
                        (mset 'queue tail)
                        (mset 'now-playing song))))
     `#m(song ,song
              state ,state)))
  ((state)
   `#m(song #m() state ,state)))

(defun now-playing
  (((= `#m(name ,name) _state))
   name)
  ((_)
   ""))

(defun playing?
  (((= `#m(name ,_name) _state))
   'true)
  ((_)
   'false))

(defun auto-play?
  (((= `#m(opts #m(auto-play? ,val)) _state))
   val))

(defun play-next (state)
  (let ((`#m(song ,song state ,state) (pop-queue state)))
    (log-debug "Got song from queue: ~p" (list song))
    (play-next song state)))

(defun play-next
  ((song state) (when (== (map_size song) 0))
   (log-warn "Not enough song data to play anything ...")
   state)
  ((next-song (= `#m(device "") state))
   (play-next next-song (mset state
                              'device
                              (proplists:get_value 1 (mref (um:devices) 'outputs)))))
  ((next-song (= `#m(device ,device channel ,channel) state))
   (if (playing? state)
     (progn
       (log-warn "Playing skipped; something is already being played")
       state)
     (progn
       (log-debug "Playing next song ...")
       (let ((`#(ok ,device-pid) (undermidi.devices:new device channel))
             (`#(ok ,worker-pid) (supervisor:start_child
                                  'undermidi.player.supervisor
                                  `(,next-song))))
         (undermidi.player.worker:play worker-pid device-pid)
         state)))))
