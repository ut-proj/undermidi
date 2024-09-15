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
(defun midi-server () 'undermidi.server)
(defun liveplay () 'undermidi.liveplay)
(defun beatracker () 'undermidi.beatracker)
(defun extclock () 'undermidi.clock.ext)
(defun extbeats () 'undermidi.clock.ext.beats)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   OTP Supervisor   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting OTP supervisor ...")
  (supervisor:start_link `#(local ,(SERVER)) (MODULE) '()))

(defun stop ()
  (call (midi-server) 'stop)
  (exit (pid) 'shutdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Supervisor Callbacks   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (_args)
  `#(ok #(#m(strategy one_for_one
             intensity 3
	           period 60)
            (,(child (liveplay))
             ,(child (beatracker))
             ,(child (extclock))
             ,(child (extbeats))
             ,(child (midi-server))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun child-pid ()
  (call (midi-server) 'pid))

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
  (call (midi-server) 'send msg))

(defun state ()
  (call (midi-server) 'state))

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
