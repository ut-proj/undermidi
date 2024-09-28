;;;; This gen_server is for keeping track of which devices a user is writing
;;;; MIDI to, and to what channels on those devices.
(defmodule undermidi.playlist
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
  ;; management API
  (export
   (add 1) (add 2)
   (opt 2))
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
(defun initial-state () '#m(queue () played () opts #m(repeat f)))
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))

(defun make-entry (file)
  (make-entry "" file))

(defun make-entry (name file)
  `#m(name ,name file ,file))

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
  (log-debug "Initialising ...")
  (erlang:process_flag 'trap_exit 'true)
  `#(ok ,state))

(defun handle_call
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
  ((message _from state)
   `#(reply ,(unknown-command (io_lib:format "~p" `(,message))) ,state)))

(defun handle_cast
  ;; Command support
  ((`#(add-entry ,entry) (= `#m(queue ,q) state))
   `#(noreply ,(mset state 'queue (++ q (list entry)))))
  ((`#(set-opt ,k ,v) (= `#m(opts ,opts) state))
   `#(noreply ,(mset state 'opts (mset opts k v))))
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

(defun add (file)
  (add "" file))

(defun add (name file)
  (gen_server:cast (SERVER) `#(add-entry ,(make-entry name file))))

(defun opt (k v)
  (gen_server:cast (SERVER) `#(set-opt ,k ,v)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dump ()
  (gen_server:call (SERVER) '#(dump)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))
