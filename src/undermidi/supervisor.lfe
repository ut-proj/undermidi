(defmodule undermidi.supervisor
  (behaviour supervisor)
  (export
   (start_link 0)
   (stop 0))
  (export
   (init 1))
  ;; Child API
  (export
   (child-pid 0)
   (child-port 0)
   (example 1)
   (list-devices 0)
   (midi 1)
   (pid 0)
   (ping 0)
   (send 1)
   (state 0)
   (stop-port 0)
   (version 0)))

(include-lib "logjam/include/logjam.hrl")

(defun SERVER () (MODULE))
;; XXX make the following configurable
;;(defun go-server () 'undermidi.go.portserver)
(defun go-server () 'undermidi.go.execserver)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   OTP Supervisor   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting OTP supervisor ...")
  (supervisor:start_link `#(local ,(SERVER)) (MODULE) '()))

(defun stop ()
  (call (go-server) 'stop)
  (exit (pid) 'shutdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Supervisor Callbacks   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (_args)
  `#(ok #(#m(strategy one_for_one
             intensity 3
	           period 60)
          (,(child (go-server))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun child-pid ()
  (call (go-server) 'pid))

(defun child-port ()
  (call (go-server) 'port))

(defun example
  ((`#m(device ,dev channel ,ch pitch ,p velocity ,v duration ,dur))
   (send `(#(command example) #(args (#(device ,dev)
                                      #(channel ,ch)
                                      #(pitch ,p)
                                      #(velocity ,v)
                                      #(duration ,dur)))))))

(defun list-devices ()
  (send #(command list-devices)))

(defun midi (midi-data)
  (send `#(midi ,midi-data)))

(defun pid ()
  (erlang:whereis (MODULE)))

(defun ping ()
  (send #(command ping)))

(defun send (msg)
  (call (go-server) 'send msg))

(defun state ()
  (call (go-server) 'state))

(defun stop-port ()
  (send #(command stop)))

(defun version ()
  (send #(command version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Internal Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun child (mod)
  `#m(id ,mod
      start #(,mod start_link ())
      restart permanent
      shutdown 2000
      type worker
      modules (,mod)))
