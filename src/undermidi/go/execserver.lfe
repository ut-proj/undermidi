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
  (let ((log-level (logjam:read-log-level "config/sys.config")))
    `#m(opts ()
        args ("-loglevel" ,(go-log-level log-level) "-daemon")
        binary ,(os:getenv "MIDISERVER")
        pid undefined
        os-pid undefined)))

(defun genserver-opts () '())
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))

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
  ;; Simple command (new format)
  (((= `(#(command ,_)) cmd) (= `#m(os-pid ,os-pid) state))
   (let ((hex-msg (hex-encode cmd)))
     (exec:send os-pid hex-msg)
     `#(noreply ,state)))
  ;; Command with args
  (((= `(#(command ,_) #(args ,_)) cmd) (= `#m(os-pid ,os-pid) state))
   (let ((hex-msg (hex-encode cmd)))
     (exec:send os-pid hex-msg)
     `#(noreply ,state)))
  ;; MIDI data
  (((= `#(midi ,_) midi) (= `#m(os-pid ,os-pid) state))
   (log-debug "Sending MIDI message: ~s" `(,(lfe_io_format:fwrite1 "~p" `(,midi))))
   (let ((hex-msg (hex-encode midi)))
     (exec:send os-pid hex-msg)
     `#(noreply ,state)))
  ;; Go server commands - old format, still used
  (((= `#(command ,_) cmd) (= `#m(os-pid ,os-pid) state))
   (let ((hex-msg (hex-encode cmd)))
     (exec:send os-pid hex-msg)
     `#(noreply ,state)))
  ((msg state)
   (log-warn "Got undexected cast msg: ~p" (list msg))
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
  (erlang:process_flag 'trap_exit 'true)
  (try
      (gen_server:cast (MODULE) msg)
    (catch
      ((tuple 'exit `#(noproc ,_) _stack)
       (log-err "Go server not running"))
      ((tuple type value stack)
       (log-err "Unexpected port error.~ntype: ~p~nvalue: ~p~nstacktrace: ~p"
                (list type value stack))))))

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
  (has-str? (ps-pid os-pid) (integer_to_list os-pid)))

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

(defun hex-encode (data)
  (let* ((bin (erlang:term_to_binary data))
         (delim (DELIMITER))
         (hex-msg (binary ((undermidi.util:bin->hex bin) binary) (delim binary))))
    (log-debug "Created hex msg: ~p" (list hex-msg))
    hex-msg))

(defun go-log-level (lfe-level)
  (case lfe-level
    ('all "trace")
    ('debug "debug")
    ('info "info")
    ('notice "warning")
    ('warning "warning")
    ('error "error")
    (_ "fatal")))
