(defmodule undermidi.util
  (export
   (create-port 2)
   (priv-dir 0)
   (receive-line 2)))

(defun APPNAME () 'ports)

(defun create-port (cmd args)
  (let ((prog (io_lib:format "~s ~s" `(,cmd ,args))))
    (erlang:open_port `#(spawn ,prog) '(binary exit_status #(line 1)))))

(defun priv-dir ()
  (case (code:priv_dir (APPNAME))
    (`#(error ,_)
     (logger:critical "~w priv dir not found~n" `(,(APPNAME)))
     (exit 'error))
    (dir dir)))

(defun receive-line (port timeout)
    (receive-line port timeout '()))

(defun receive-line (port timeout buffer)
  (receive
    (`#(,port #(exit_status ,exit-status))
     (logger:error "Port unexpectedly exited with status ~p" `(exit-status))
     (erlang:term_to_binary '#(error port_exit)))
    (`#(,port #(data #"\n"))
     (logger:debug "Skipping newline ...")
     (receive-line port timeout buffer))
    (`#(,port #(data #(,_flag ,data)))
     (logger:debug "Got data: ~p" `(,data))
     (receive-line port timeout `(,data . ,buffer)))
    (`#(,port ,msg)
     (logger:debug "Got message: ~p" `(,msg))
     (receive-line port timeout `(,msg . ,buffer)))
    (after timeout
      (logger:debug "Buffer: ~p" `(,buffer))
      (let* ((rev (lists:reverse buffer))
             (bin (binary:list_to_bin rev)))
        (logger:debug "Reversed: ~p" `(,rev))
        (logger:debug "Binary: ~p" `(,bin))
        bin))))
