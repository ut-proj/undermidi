(defmodule undermidi.util
  (export
   (create-port 2)
   (priv-dir 0)
   (receive-line 2)
   (version 0)
   (versions 0)))

(include-lib "logjam/include/logjam.hrl")

(defun APPNAME () 'undermidi)

;;; Version functions

(defun version ()
  (version 'undermidi))

(defun version (app-name)
  (application:load app-name)
  (case (application:get_key app-name 'vsn)
    (`#(ok ,vsn) vsn)
    (default default)))

(defun version-arch ()
  `#(architecture ,(erlang:system_info 'system_architecture)))

(defun version+name (app-name)
  `#(,app-name ,(version app-name)))

(defun versions-rebar ()
  `(,(version+name 'rebar)
    ,(version+name 'rebar3_lfe)))

(defun versions-langs ()
  `(,(version+name 'lfe)
    #(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver ,(erlang:system_info 'driver_version))))

(defun versions ()
  (lists:append `((,(version+name 'undermidi))
                  ,(versions-langs)
                  ,(versions-rebar)
                  (,(version-arch)))))

;;; General support functions

(defun create-port (cmd args)
  (let ((prog (io_lib:format "~s ~s" `(,cmd ,args))))
    (erlang:open_port `#(spawn ,prog) '(binary exit_status #(line 1)))))

(defun priv-dir ()
  (case (code:priv_dir (APPNAME))
    (`#(error ,_)
     (log-critical "~w priv dir not found~n" `(,(APPNAME)))
     (exit 'error))
    (dir dir)))

(defun receive-line (port timeout)
    (receive-line port timeout '()))

(defun receive-line (port timeout buffer)
  (receive
    (`#(,port #(exit_status ,exit-status))
     (log-error "Port unexpectedly exited with status ~p" `(exit-status))
     (erlang:term_to_binary '#(error port_exit)))
    (`#(,port #(data #"\n"))
     (log-debug "Skipping newline ...")
     (receive-line port timeout buffer))
    (`#(,port #(data #(,_flag ,data)))
     (log-debug "Got data: ~p" `(,data))
     (receive-line port timeout `(,data . ,buffer)))
    (`#(,port ,msg)
     (log-debug "Got message: ~p" `(,msg))
     (receive-line port timeout `(,msg . ,buffer)))
    (after timeout
      (log-debug "Buffer: ~p" `(,buffer))
      (let* ((rev (lists:reverse buffer))
             (bin (binary:list_to_bin rev)))
        (log-debug "Reversed: ~p" `(,rev))
        (log-debug "Binary: ~p" `(,bin))
        bin))))
