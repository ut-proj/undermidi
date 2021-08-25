;;;; This module is for running the state server which keeps track of all
;;;; MIDI phrases, sequences, etc., entered by a user in the REPL.
(defmodule undermidi.liveplay
  (export all))

(include-lib "logjam/include/logjam.hrl")

;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   config functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=--------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SERVER () (MODULE))
(defun table-name () 'liveplay-history)
(defun table-desc () "Liveplay session table")
(defun table-options ()
  '(bag named_table public))
(defun initial-state ()
  `#m(name liveplay
      table-name ,(table-name)
      table-desc ,(table-desc)))
(defun genserver-opts () '())
(defun unknown-command (data)
  `#(error ,(lists:flatten (++ "Unknown command: " data))))

;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   gen_server implementation   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start_link ()
  (log-info "Starting undertone Extempore REPL server ...")
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
  (((= `#m(table-name ,table-name) state))
   (ets:new table-name (table-options))
   (log-debug (table-info table-name))
   `#(ok ,state)))

(defun handle_cast
  ((_msg state)
   `#(noreply ,state)))

(defun handle_call
  ;; Session history
  ((#(history) _from state)
   ;; XXX add impl for query, sorting, etc.
   `#(reply tbd ,state))
  ;; Metadata
  ((#(table-info) _from state)
   `#(reply ,(table-info state) ,state))
  ;; Stop
  (('stop _from (= `#m(session ,sess) state))
   (ets:delete (mref sess 'table))
   `#(stop normal ok ,state))
  ;; Testing / debugging
  ((`#(echo ,msg) _from state)
   `#(reply ,msg ,state))
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
;;;::=-   XXX API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TBD

;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   history API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=---------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun history-data ()
  (gen_server:call (SERVER) #(history)))

(defun show-history ()
  (lfe_io:format "~p~n" (list (history-data))))

(defun table-info ()
  (gen_server:call (SERVER) #(table-info)))

;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   debugging API   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-----------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

(defun pid ()
  (erlang:whereis (SERVER)))

(defun ping ()
  (gen_server:call (SERVER) #(ping)))

(defun state ()
  (gen_server:call (SERVER) #(state)))

;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;::=-   utility / support functions   -=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;::=-------------------------------=::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun table-info
  ((`#m(table-name ,table-name table-desc ,table-desc))
   (maps:merge `#m(controlling-process ,(MODULE)
                   name ,table-name
                   description ,table-desc)
               (maps:from_list (ets:info table-name)))))