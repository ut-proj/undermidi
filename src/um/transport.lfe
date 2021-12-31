;;;; A tranport is a sequence of annotated start/stop actions. This module
;;;; provides functions for working with this abstraction.
(defmodule um.transport
  (export all))

(defun new-action (action)
  `#m(action ,action
      time ,(erlang:timestamp)))

(defun append
  ((old action) (when (is_atom action))
   (append old (new-action action)))
  ((old new) (when (is_map new))
   (append old (list new)))
  ((old new) (when (is_list new))
   (lists:append old new)))

(defun stop (tr)
  (cond
   ((empty? tr) tr)
   ((last-action-stop? tr) tr)
   ('true (append tr 'stop))))

(defun start (tr)
  (if (last-action-start? tr)
    tr
    (append tr 'start)))

(defun first (tr)
  (if (empty? tr)
    '()
    (car tr)))

(defun first-start (tr) (first tr))

(defun first-start-time (tr)
  (mref (first tr) 'time))

(defun last (tr)
  (if (empty? tr)
    '()
    (lists:last tr)))

(defun last-action (tr)
  (if (empty? tr)
    #(error no-transport-data)
    (mref (last tr) 'action)))

(defun last-time (tr)
  (if (empty? tr)
    #(error no-transport-data)
    (mref (last tr) 'time)))

(defun last-action-stop? (tr)
  (== 'stop (last-action tr)))

(defun last-action-start? (tr)
  (== 'start (last-action tr)))

(defun last-kv (tr key val)
  (if (empty? tr)
    #(error no-transport-data)
    (car (lists:filter (lambda (x) (== (mref tr key) val))
                       (lists:nth-last 2 tr)))))

(defun last-stop (tr)
  (last-kv tr 'action 'stop))

(defun last-start (tr)
  (last-kv tr 'action 'start))

(defun last-start-time (tr)
  (case (last-start tr)
    ((= `#(error ,_) err) err)
    (result (mref result 'time))))

(defun last-stop-time (tr)
  (case (last-stop tr)
    ((= `#(error ,_) err) err)
    (result (mref result 'time))))

(defun running? (tr)
  (if (empty? tr)
    'false
    (last-action-start? tr)))

(defun stopped? (tr)
  (not (running? tr)))

(defun empty? (tr)
  (== '() tr))
