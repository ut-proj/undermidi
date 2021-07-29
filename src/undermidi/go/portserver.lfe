(defmodule undermidi.go.portserver
  (behaviour gen_server)
  (export
   (start_link 0)
   (stop 0))
  (export
   (code_change 3)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (init 1)
   (terminate 2))
  (export
   (pid 0)
   (port 0)
   (port-info 0)
   (send 1)))

(include-lib "logjam/include/logjam.hrl")

(defun SERVER () (MODULE))
(defun DELIMITER () '(10))
(defun GO-TIMEOUT () 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   gen_server API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stop ()
  (gen_server:call (MODULE) 'stop))

(defun start_link ()
  (gen_server:start_link `#(local ,(SERVER)) (MODULE) '() '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Supervisor Callbacks   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (_args)
  (erlang:process_flag 'trap_exit 'true)
  `#(ok ,(create-port)))

(defun handle_call
  (('stop _from port)
   (stop-port port)
   `#(stop normal ok ,port))
  (('port _from port)
   `#(reply ,port ,port))
  ((msg from port)
   (let ((msg-bin (erlang:term_to_binary msg)))
     (log-debug "Sending data: ~p" `(,msg-bin))
     (! port `#(,(self) #(command (,msg-bin ,(DELIMITER)))))
     (let ((data (undermidi.util:receive-line port (GO-TIMEOUT))))
       (log-debug "Got data: ~p" `(,data))
       (case data
         (#b()
           (log-error "Got empty data from ~p; continuing ..." `(,from))
           `#(reply 'nodata ,port))
         (_
           (log-debug "Data: ~p" `(,data))
           `#(reply ,(erlang:binary_to_term data '(safe)) ,port)))))))

(defun handle_cast (_msg state)
  `#(noreply ,state))

(defun handle_info
  ((`#(EXIT ,_from normal) port)
   (log-debug "The Go echo server is exiting (normal).")
   (stop)
   `#(noreply ,port))
  ((`#(EXIT ,_from shutdown) port)
   (log-debug "The Go echo server is exiting (shutdown).")
   (stop)
   `#(noreply ,port))
  ((`#(EXIT ,from ,reason) port)
   (log-error "Go echo process ~p exited! (Reason: ~p)" `(,from ,reason))
   (stop)
   `#(noreply ,port))
  ((msg port)
   (log-debug "The Go echo server is handling info of type: ~p." `(,msg))
   `#(noreply ,port)))

(defun terminate
  ((normal _port)
   (log-info "The Go echo server is terminating."))
  ((shutdown _port)
   (log-info "The supervisor is shutting down the Go echo server."))
  ((reason _port)
   (log-info "The Go echo server is terminating for reason: ~p." `(,reason))))

(defun code_change (_old-version port _extra)
  `#(ok ,port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pid ()
  (erlang:whereis (MODULE)))

(defun port ()
  (gen_server:call (MODULE) 'port))

(defun port-info ()
  (erlang:port_info (port)))

(defun send (msg)
  (erlang:process_flag 'trap_exit 'true)
  (try
      (gen_server:call (MODULE) msg)
    (catch
      ((tuple 'exit `#(noproc ,_) _stack)
       (log-err "Go server not running"))
      ((tuple type value stack)
       (log-err "Unexpected port error.~ntype: ~p~nvalue: ~p~nstacktrace: ~p"
                (list type value stack))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Internal Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-port ()
  (let* ((midiserver (os:getenv "MIDISERVER")))
    (log-debug "Creating port for ~s ..." (list midiserver))
    (undermidi.util:create-port midiserver '())))

(defun stop-port (port)
  (gen_server:call (MODULE) #(command stop))
  (! port `#(,(self) 'close)))
