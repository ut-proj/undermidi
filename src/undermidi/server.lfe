(defmodule undermidi.server
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
   (state 0))
  ;; debug API
  (export
    (pid 0)
    (echo 1)))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   config functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SERVER () (MODULE))
(defun DELIMITER () #"\n")

(defun initial-state ()
  `#m(opts ()
      current-channel 1
      pid undefined))

(defun genserver-opts () '())
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting midiserver controller ...")
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
  ;; Management
  ((`#(state) _from state)
   `#(reply ,state ,state))
  ;; Health
  ((`#(status midiserver) _from state)
   `#(reply not-implemented ,state))
  ;; Stop
  (('stop _from state)
   (log-notice "Stopping Go MIDI server ...")
   `#(stop normal ok ,state))
  ;; Testing / debugging
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
  ;; Fall-through
  ((message _from state)
   `#(reply ,(unknown-command (io_lib:format "~p" `(,message))) ,state)))

(defun handle_cast
  ;; Simple command (new format)
  (((= `(#(command ,_)) cmd) state)
   (log-warn "Unsupported server command: ~p" `(,cmd))
   `#(noreply ,state))
  ;; MIDI data
  (((= `#(midi ,_) midi) state)
   (log-debug "Sending MIDI message: ~s" `(,(lfe_io_format:fwrite1 "~p" `(,midi))))
   (log-warn "Unsupported MIDI message: ~p" `(,midi))
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
   (logger:info "midiserver controller is exiting (normal).")
   `#(noreply ,state))
  ((`#(EXIT ,_from shutdown) state)
   (logger:info "midiserver controller is exiting (shutdown).")
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (log-notice "Process ~p exited! (Reason: ~p)" `(,pid ,reason))
   `#(noreply ,state))
  ;; Fall-through
  ((msg state)
   (log-debug "Unknwon info: ~p" `(,msg))
   `#(noreply ,state)))

(defun terminate
  ((_reason `#m(os-pid ,os-pid))
   (log-notice "Terminating midiserver controller ...")
   (catch (exec:stop os-pid))
   'ok))

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   management API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun state ()
  (gen_server:call (SERVER) #(state)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pid ()
  (erlang:whereis (SERVER)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility / support functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun has-str? (string pattern)
  (case (string:find string pattern)
    ('nomatch 'false)
    (_ 'true)))
