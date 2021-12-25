;;;; This module is for keeping track of an external clock's ticks as well
;;;; as providing ccconvenience functions that help calculate things like
;;;; measures, beats, etc.,
(defmodule undermidi.clock.ext
  (behaviour gen_server)
  (export all))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   config functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SERVER () (MODULE))
(defun initial-state ()
  #m(total 0
     times ()))
(defun max-times () 200)
(defun genserver-opts () '())
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting undermidi external MIDI clock server ...")
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
  `#(ok ,state))

(defun handle_cast
  ((`#(tick) state)
   `#(noreply ,(update-state state)))
  ((`#(reset) state)
   `#(noreply ,(initial-state)))
  ;; Fall-through
  ((msg state)
   (log-debug "Unhandled handle_cast message: ~p" `(,msg))
   `#(noreply ,state)))

(defun handle_call
  ;; API
  ((#(bpm) _from (= `#m(times ,times) state))
   `#(reply ,(bpm times 1) ,state))
  ((`#(bpm ,n) _from (= `#m(times ,times) state))
   `#(reply ,(bpm times n) ,state))
  ((#(times) _from state)
   `#(reply ,(mref state 'times) ,state))
  ((#(total) _from state)
   `#(reply ,(mref state 'total) ,state))
  ;; Stop
  (('stop _from state)
   `#(stop normal ok ,state))
  ;; Testing / debugging
  ((#(ping) _from state)
   `#(reply PONG ,state))
  ((#(state) _from state)
   `#(reply ,state ,state))
  ;; Fall-through
  ((message _from state)
   `#(reply ,(unknown-command (io_lib:format "~p" `(,message))) ,state)))

(defun handle_info
  ((`#(EXIT ,_from normal) state)
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (log-notice "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state))
  ((_msg state)
   `#(noreply ,state)))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;;;;::=------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   Clock API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bpm ()
  (gen_server:call (SERVER) #(bpm)))

(defun bpm (last-n)
  (gen_server:call (SERVER) `#(bpm ,last-n)))

(defun bpm-max ()
  (gen_server:call (SERVER) `#(bpm ,(max-times))))

(defun reset ()
  (gen_server:cast (SERVER) #(reset)))

(defun tick ()
  (gen_server:cast (SERVER) #(tick)))

(defun times ()
  (gen_server:call (SERVER) #(times)))

(defun times-count ()
  (length (times)))

(defun total ()
  (gen_server:call (SERVER) #(total)))


;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ping ()
  (gen_server:call (SERVER) #(ping)))

(defun state ()
  (gen_server:call (SERVER) #(state)))

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility / support functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-state
  ((`#m(total ,total times ,times))
   (let ((new-total (+ 1 total))
         (new-times (lists:append (list (erlang:timestamp)) times)))
     `#m(total ,new-total
         times ,(lists:sublist new-times 1 (max-times))))))

(defun bpm
  ((times _) (when (< (length times) 2))
   0)
  ((times last-n)
   (let* ((upper (lists:sublist times 1 (- (+ 1 last-n) 1)))
          (lower (lists:sublist times 2 (+ 1 last-n)))
          (shortest (min (length upper) (length lower)))
          (diffs (lists:zipwith #'timer:now_diff/2
                                (lists:sublist upper 1 shortest)
                                (lists:sublist lower 1 shortest)))
          (avg-diff (/ (lists:sum diffs) shortest 1000000)))
     (erlang:round (bpm-adjust (/ 60 avg-diff))))))

(defun bpm-adjust (x)
  "This was calculated by curve-fitting actual BPM vs. meausured and
  might account for the overhead in reporting each beat over the loopback
  device through the services. Or maybe not."
  (+ 1.36 (* 1.14 x) (* 6.34e-5 (math:pow x 2))))
