(defmodule undermidi.go.execserver
  (behaviour gen_server)
  ;; gen_server implementation
  (export
    (start_link 0)
    (stop 0))
  ;; callback implementation
  (export
    (init 1)
    (handle_call 3)
    (handle_cast 2)
    (handle_info 2)
    (terminate 2)
    (code_change 3))
  ;; Go server API
  (export
   (send 1))
  ;; management API
  (export
   (state 0))
  ;; health API
  (export
   (midiserver-responsive? 0)
   (healthy? 0)
   (os-process-alive? 0)
   (status 0))
  ;; debug API
  (export
    (pid 0)
    (echo 1)))

(include-lib "logjam/include/logjam.hrl")

(defun SERVER () (MODULE))
(defun DELIMITER () #"\n")

(defun initial-state ()
  `#m(opts ()
      args ()
      binary ,(os:getenv "MIDISERVER")
      pid undefined
      os-pid undefined))

(defun genserver-opts () '())
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))
(defun unknown-info (data)
  `#(error ,(lists:flatten (++ "Unknown info: " data))))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting OTP gen_server ...")
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
  (log-debug "Initialising OTP gen_server ...")
  (erlang:process_flag 'trap_exit 'true)
  (let ((start-state (start-midiserver (self) state)))
    (log-debug "Start state: ~p" (list start-state))
    `#(ok ,(maps:merge state start-state))))

(defun handle_cast
  ;; Go server commands
  (((= `#(command ,_) cmd) (= `#m(os-pid ,os-pid) state))
   (let* ((cmd (erlang:term_to_binary cmd))
          (delim (DELIMITER))
          (msg (binary (cmd binary) (delim binary)))
          (hex-msg (binary ((undermidi.util:bin->hex cmd) binary) (delim binary))))
     (log-debug "Sending hex-msg: ~p ..." (list hex-msg))
     (exec:send os-pid hex-msg)
     `#(noreply ,state)))
  ((_msg state)
   `#(noreply ,state)))

(defun handle_call
  ;; Management
  ((`#(state) _from state)
   `#(reply ,state ,state))
  ;; Health
  ((`#(status midiserver) _from state)
   `#(reply not-implemented ,state))
  ((`#(status os-process) _from (= `#m(os-pid ,os-pid) state))
   `#(reply ,(ps-alive? os-pid) ,state))
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

(defun handle_info
  ;; Standard-output messages
  ((`#(stdout ,_pid ,msg) state)
   (io:format "~s" (list (binary_to_list msg)))
   `#(noreply ,state))
  ;; Standard-error messages
  ((`#(stderr ,_pid ,msg) state)
   (io:format "~s" (list (binary_to_list msg)))
   `#(noreply ,state))
  ;; Port EOL-based messages
  ((`#(,port #(data #(eol ,msg))) state) (when (is_port port))
   (log-info (sanitize-midiserver-msg msg))
   `#(noreply ,state))
  ;; Port line-based messages
  ((`#(,port #(data #(,line-msg ,msg))) state) (when (is_port port))
   (log-info "Unknown line message:~p~s" `(,line-msg ,(sanitize-midiserver-msg msg)))
   `#(noreply ,state))
  ;; General port messages
  ((`#(,port #(data ,msg)) state) (when (is_port port))
   (log-info "Message from midiserver port:~n~s" `(,(sanitize-midiserver-msg msg)))
   `#(noreply ,state))
  ;; Exit-handling
  ((`#(,port #(exit_status ,exit-status)) state) (when (is_port port))
   (log-warn "~p: exited with status ~p" `(,port ,exit-status))
   `#(noreply ,state))
  ((`#(EXIT ,_from normal) state)
   (logger:info "midiserver server is exiting (normal).")
   `#(noreply ,state))
  ((`#(EXIT ,_from shutdown) state)
   (logger:info "midiserver server is exiting (shutdown).")
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
   (log-notice "Terminating midiserver server ...")
   (catch (exec:stop os-pid))
   'ok))

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Go server API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun send (msg)
  (gen_server:cast (MODULE) msg))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   management API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-midiserver
  ((mgr-pid (= `#m(args ,args binary ,bin) state))
   (log-debug "Starting Go midiserver executable ...")
   (maps:merge state (run mgr-pid bin args))))

(defun state ()
  (gen_server:call (SERVER) #(state)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   health API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun midiserver-responsive? ()
  (gen_server:call (SERVER) #(status midiserver)))

(defun healthy? ()
  (let ((vals (maps:values (status))))
    (not (lists:member 'false vals))))

(defun os-process-alive? ()
  (gen_server:call (SERVER) #(status os-process)))

(defun status ()
  (gen_server:call (SERVER) #(status all)))

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

(defun sanitize-midiserver-msg (msg)
  ;;(log-debug "Binary message: ~p" `(,msg))
  (clj:-> msg
          (binary_to_list)
          (string:replace "\\" "")
          (string:trim)))

(defun midi-hash->map (hash)
  (let* ((note-state (band hash #b1))
         (channel (band (bsr hash 1) #b1111))
         (pitch (band (bsr hash 5) #b111111))
         (velocity (band (bsr hash 12) #b111111))
         (time (band (bsr hash 19) #b11111111111111111111)))
    `#m(note-state ,(if (== note-state 1) 'on 'off)
        channel ,channel
        pitch ,pitch
        velocity ,velocity
        time ,time)))

(defun stdout-map-regex ()
  "(^\\\[\\\")*(?<MAP>['`]*#[mM][^\\\"\\\]]+)(\"\])*$")

(defun match-stdout-map (string)
  (re:run string (stdout-map-regex) '(#(capture 'MAP list))))

(defun handle-stdout-data
  (((= `#m(type sequence-step
           data #m(beat ,beat
                   note ,note
                   note-duration ,dur)) map-data))
   (log-notice `#m(data ,map-data))
   (log-warn "XXX: Save to ETS table!"))
  ((msg)
   (log-warn "Unhandled data format: ~p" `(,msg))))

(defun join-cmd-args (cmd args)
  (clj:-> (list cmd)
          (lists:append args)
          (string:join " ")))

(defun default-run-opts (mgr-pid)
  `(stdin
    pty
    #(stdout ,mgr-pid)
    #(stderr ,mgr-pid)
    monitor))

(defun run-opts (mgr-pid opts)
  (if (maps:is_key 'run-opts opts)
    (mref opts 'run-opts)
    (default-run-opts mgr-pid)))

(defun has-str? (string pattern)
  (case (string:find string pattern)
    ('nomatch 'false)
    (_ 'true)))

(defun ps-alive? (os-pid)
  (has-str? (ps-pid os-pid) (integer_to_list os-pid))) ;

(defun ps-pid (pid-str)
  (os:cmd (++ "ps -o pid -p" pid-str)))

(defun run (mgr-pid cmd args)
  (run mgr-pid cmd args #m()))

(defun run (mgr-pid cmd args opts)
  (let ((opts (run-opts mgr-pid opts)))
    (log-debug "Starting OS process ~s with args ~p and opts ~p"
               (list cmd args opts))
    (let ((exec-str (join-cmd-args cmd args)))
      (log-debug "Using exec string: ~s" (list exec-str))
      (let ((`#(ok ,pid ,os-pid) (exec:run_link exec-str opts)))
        `#m(pid ,pid os-pid ,os-pid)))))
