(defmodule um.transport
  (export all))

(defun new (action)
  `#m(action ,action
      time ,(erlang:timestamp)))

(defun append
  ((old action) (when (is_atom action))
   (append (new action)))
  ((old new) (when (is_map new))
   (append old (list new)))
  ((old new) (when (is_list new))
   (lists:append old new)))

(defun stop (tr)
  (if (last-action-stop? tr)
    tr
    (append tr 'stop)))

(defun start (tr)
  (if (last-action-start? tr)
    tr
    (append tr 'start)))

(defun first (tr)
  (if (== tr '())
    '()
    (car tr)))

(defun first-start (tr) (first tr))

(defun first-start-time (tr)
  (mref (first tr) 'time))

(defun last (tr)
  (if (== tr '())
    '()
    (lists:last tr)))

(defun last-action (tr)
  (if (== transport '())
    #(error no-transport-data)
    (mref (last tr) 'action)))

(defun last-time (tr)
  (if (== transport '())
    #(error no-transport-data)
    (mref (last tr) 'time)))

(defun last-action-stop? (tr)
  (== 'stop (last-action tr)))

(defun last-action-start? (tr)
  (== 'start (last-action tr)))

(defun last-kv (tr key val)
  (if (== transport '())
    #(error no-transport-data)
    (car (lists:filter (lambda (x) (== (mref tr key) val))
                       (lists:nth-last 2 tr)))))

(defun last-stop (tr)
  (last-kv tr 'action 'stop))

(defun last-start (tr)
  (last-kv tr 'action 'start))

(defun last-start-time (tr)
  (case (last-start)
    ((= `#(error ,_) err) err)
    (result (mref result) 'time)))

(defun last-stop-time (tr)
  (case (last-stop)
    ((= `#(error ,_) err) err)
    (result (mref result) 'time)))

(defun running? (tr)
  (if (== tr '())
    'false
    (last-action-start? tr)))

(defun stopped? (tr)
  (not (running? tr)))
