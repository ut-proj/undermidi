;;;; This gen_server is for keeping track of an individual device a that is writing
;;;; MIDI to, and the current MIDI channel in use for communications to that device.
(defmodule undermidi.player.worker
  (behaviour gen_server)
  ;; gen_server implementation
  (export
    (start_link 1)
    (stop 1))
  ;; callback implementation
  (export
    (code_change 3)
    (handle_call 3)
    (handle_cast 2)
    (handle_info 2)
    (init 1)
    (terminate 2))
  ;; device API
  (export
   (play 2)
   (state 1))
  ;; debug API
  (export
    (echo 2)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "undermidi/include/errors.lfe")

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   config functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun DELIMITER () #"\n")
(defun NAME () "player queue song/sequence handler")
(defun QUEUE-SERVER () 'undermidi.player.queue)
(defun genserver-opts () '())
(defun play-timeout () 600000) ; 10 minutes, for super-long longs ...

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link (song)
  (log-info "Starting ~s for ~p ..." (list (NAME) song))
  (gen_server:start_link (MODULE)
                         song
                         (genserver-opts)))

(defun stop (pid)
  (gen_server:call pid 'stop))

;;;;;::=---------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   callback implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (song)
   (log-debug "Initialising ~s ..." (list (NAME)))
   `#(ok ,song))

(defun handle_call
  ;; Stop
  (('stop _from song)
   (log-notice "Stopping ~s ..." (list (NAME)))
   `#(stop normal ok ,song))
  ;; Testing / debugging
  ((`#(state) _from song)
   `#(reply ,song ,song))
  ((`#(echo ,msg) _from song)
   `#(reply ,msg ,song))
  ;; Fall-through
  ((msg _from song)
   `#(reply ,(ERR-UNKNOWN-COMMAND msg) ,song)))

(defun handle_cast
  ;; API
  ((`#(play ,worker-pid ,device-pid) (= `#m(source ,source) song))
   (call source 'play device-pid)
   ;; TODO: change this to really play the sequence
   ;(log-info "Getting ready to sleep ...")
   ;(timer:sleep 10000)
   (gen_server:cast (QUEUE-SERVER) `#(finished ,song))
   `#(noreply ,song))
  ;; Command support
  (((= `(#(command ,_)) cmd) song)
   (log-warn "Unsupported server command: ~p" `(,cmd))
   `#(noreply ,song))
  ((msg song)
   (log-warn "Got undexected cast msg: ~p" (list msg))
   `#(noreply ,song)))

(defun handle_info
  ;; Exit-handling
  ((`#(EXIT ,_from normal) song)
   (logger:info "~s is exiting (normal)." (list (NAME)))
   `#(noreply ,song))
  ((`#(EXIT ,_from shutdown) song)
   (logger:info "~s is exiting (shutdown)." (list (NAME)))
   `#(noreply ,song))
  ((`#(EXIT ,pid ,reason) song)
   (log-notice "Process ~p exited! (Reason: ~p)" `(,pid ,reason))
   `#(noreply ,song))
  ;; Fall-through
  ((msg song)
   (log-debug "Unknwon info: ~p" `(,msg))
   `#(noreply ,song)))

(defun terminate
  ((_reason _song)
   (log-notice "Terminating ~s ..." (list (NAME)))
   'ok))

(defun code_change (_old-version song _extra)
  `#(ok ,song))

;;;;;::=------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Player Worker API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun play (worker-pid device-pid)
  (gen_server:cast worker-pid `#(play ,worker-pid ,device-pid)))

(defun state (pid)
  (gen_server:call pid `#(state)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun echo (pid msg)
  (gen_server:call pid `#(echo ,msg)))
