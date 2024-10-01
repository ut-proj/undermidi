;;;; This gen_server is for keeping track of an individual device a that is writing
;;;; MIDI to, and the current MIDI channel in use for communications to that device.
(defmodule undermidi.device.client
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
   (apply 4)
   (channel 1) (channel 2)
   (device 1)
   (state 1)
   (send 2) (send 3)
   (batch 2) (batch 3))
  ;; debug API
  (export
    (echo 2)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "undermidi/include/errors.lfe")

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   config functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun DELIMITER () #"\n")
(defun NAME () "MIDI device connection")

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
   (log-debug "Initialising device client ...")
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
  ((`#(set-channel ,num) state)
   `#(noreply (mset state 'channel num)))
  (((= `(#(command ,_)) cmd) state)
   (log-warn "Unsupported server command: ~p" `(,cmd))
   `#(noreply ,state))
  ((msg state)
   (log-warn "Got undexected cast msg: ~p" (list msg))
   `#(noreply ,state)))

(defun handle_info
  ;; Standard-output messages
  ((`#(stdout ,_pid ,msg) state)
   (io:format "~s" (list (binary_to_list msg)))
   `#(noreply ,state))
  ;; Standard-error messages
  ((`#(stderr ,_pid ,msg) state)
   (io:format "~s" (list (binary_to_list msg)))
   `#(noreply ,state))
  ;; Exit-handling
  ((`#(,port #(exit_status ,exit-status)) state) (when (is_port port))
   (log-warn "~p: exited with status ~p" `(,port ,exit-status))
   `#(noreply ,state))
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

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Device API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun channel (pid)
  (mref (state pid) 'channel))

(defun channel (pid num)
  (let ((`#m(device ,name) (state pid)))
    (undermidi.devices:update-device name num)
    (gen_server:call pid `#(set-channel ,num))))

(defun device (pid)
  (mref (state pid) 'devicel))

(defun apply (pid m f a)
  (let ((`#m(device ,device
             channel ,channel) (state pid)))
    (apply m f (++ (list device channel) a))))

(defun state (pid)
  (gen_server:call pid `#(state)))

(defun send (pid msg)
  (let ((`#m(device ,device
             channel ,channel) (state pid)))
    (um.ml:send device channel msg)))

(defun send (pid channel msg)
  (channel pid channel)
  (um.ml:send (device pid) channel msg))

(defun batch (pid msgs)
  (let ((`#m(device ,device
             channel ,channel) (state pid)))
    (um.ml:batch device channel msgs)))

(defun batch (pid channel msgs)
  (channel pid channel)
  (um.ml:batch (device pid) channel msgs))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun echo (pid msg)
  (gen_server:call pid `#(echo ,msg)))
