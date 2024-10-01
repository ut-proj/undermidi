;;;; This gen_server is for keeping track of which devices a user is writing
;;;; MIDI to, and to what channels on those devices.
(defmodule undermidi.devices
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
   (new 1) (new 2)
   (read 0) (read 1) (read 2)
   (write 2) (write 3))
  ;; data API
  (export
   (select-all 0)
   (select-device 1)
   (select-value 2))
  ;; debug API
  (export
   (echo 1)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "undermidi/include/errors.lfe")

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   config functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SERVER () (MODULE))
(defun DELIMITER () #"\n")
(defun NAME () "MIDI devices manager")
(defun table-name () 'devices)
(defun genserver-opts () '())

(defun ets ()
  `#m(name ,(table-name)
      description "An ETS table for maintaining device state"
      opts (set named_table public)))

(defun initial-state () `#m(ets ,(ets)))

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

(defun init
  (((= `#m(ets #m(name ,table-name opts ,table-opts)) state))
   (log-debug "Initialising ...")
   (ets:new table-name table-opts)
   (log-debug (undermidi.util:table-info table-name))
   (erlang:process_flag 'trap_exit 'true)
   `#(ok ,state)))

(defun handle_call
  ;; Data
  ((`#(state) _from state)
   `#(reply ,state ,state))
  ((`#(devices) _from state)
   `#(reply ,(select-all) ,state))
  ((`#(device ,name) _from state)
   `#(reply ,(select-device name) ,state))
  ((`#(value ,name ,key) _from state)
   `#(reply ,(select-value name key) ,state))
  
  ;; Stop
  (('stop _from state)
   (log-notice "Stopping ~s ..." (list (NAME)))
   `#(stop normal ok ,state))
  ;; Testing / debugging
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
  ;; Fall-through
  ((msg _from state)
   `#(reply ,(ERR-UNKNOWN-COMMAND msg) ,state)))

(defun handle_cast
  ;; Command support
  ((`#(set-device #(,name #(channel ,channel))) state)
   (update-device name channel)
   `#(noreply ,state))
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

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Devices API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-device (name channel)
  `#(,name (#(channel ,channel))))

(defun new (midi-device-name)
  (new midi-device-name 1))

(defun new
  ((midi-device-name midi-channel) (when (is_atom midi-device-name))
   (new (atom_to_list midi-device-name) midi-channel))
  ((midi-device-name midi-channel)
   (if (not (lists:member midi-device-name (++ (um.nif:inputs) (um.nif:outputs))))
     (ERR-NO-DEVICE)
     (progn
       (add-device midi-device-name midi-channel)
       (supervisor:start_child 'undermidi.device.supervisor `(,midi-device-name))))))

(defun read ()
  (gen_server:call (SERVER) '#(devices)))

(defun read (device-name)
  (gen_server:call (SERVER) `#(device ,device-name)))

(defun read (device-name key)
  (gen_server:call (SERVER) `#(value ,device-name ,key)))

(defun state ()
  (gen_server:call (SERVER) `#(state)))

(defun write (device new-device-data)
  'tbd)

(defun write (device key new-value)
  'tbd)

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   ETS Data API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-all ()
  (ets:select (table-name) (ets-ms (((tuple a b))
                                    (tuple a b)))))

(defun select-device (name)
  (let ((result (ets:select (table-name) (ets-ms (((tuple a b))
                                                  (when (== a name))
                                                  (tuple a b))))))
    (case result
      ('() result)
      (`(,head . ,_) head))))

(defun select-value (name key)
  (case (select-device name)
    ('() 'undefined)
    (`#(,_ ,plist) (proplists:get_value key plist))))

(defun add-device (device channel)
  (ets:insert (table-name)
              (make-device device channel)))
  
(defun update-device (name channel)
  (ets:insert (table-name)
              (make-device name channel)))