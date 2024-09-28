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

(defun genserver-opts () '())

(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link (device-name)
  (log-info "Starting ~s ..." (list (NAME)))
  (gen_server:start_link (MODULE)
                         device-name
                         (genserver-opts)))

(defun stop (pid)
  (gen_server:call pid 'stop))

;;;;;::=---------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   callback implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (device-name)
   (log-debug "Initialising ...")
   `#(ok #m(device ,device-name
            channel ,(undermidi.devices:read device-name 'channel))))

(defun handle_call
  ;; Management
  ((`#(state) _from state)
   `#(reply ,state ,state))
  ;; Stop
  (('stop _from state)
   (log-notice "Stopping ~s ..." (list (NAME)))
   `#(stop normal ok ,state))
  ;; Testing / debugging
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
  ;; Fall-through
  ((message _from state)
   `#(reply ,(unknown-command (io_lib:format "~p" `(,message))) ,state)))

(defun handle_cast
  ;; Command support
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

;;;;;::=------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Player Worker API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun state (pid)
  (gen_server:call pid `#(state)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun echo (pid msg)
  (gen_server:call pid `#(echo ,msg)))
