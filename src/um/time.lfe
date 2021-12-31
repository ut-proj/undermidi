(defmodule um.time
  (export all))

(defun micro->
  ((ms #(seconds))
   (/ ms 1000000))
  ((ms #(minutes))
   (/ (micro-> ms #(seconds)) 60))
  ((ms #(hours))
   (/ (micro-> ms #(seconds)) 3600)))

(defun ->seconds
  ((ms #(micro))
   (micro-> ms #(seconds)))
  ((mm #(minutes))
   (* 60 mm))
  ((hh #(hours))
   (* 60 60 hh))
  ((dd #(days))
   (* 24 60 60 dd)))

(defun modulo-24h
  ((`#(,hh ,mm ,ss))
   `#(,(rem hh 24) ,mm ,ss)))

(defun modulo-24h
  ((ss #(seconds))
   (rem ss (->seconds 1 #(days)))))

(defun duration
  "Given two timestamps (e.g., as returned by erlang:timestamp), calculate the
  time duration represented by their difference, in the given units or format."
  ((ts2 ts1 #(micro))
   (timer:now_diff ts2 ts1))
  ((ts2 ts1 #(time))
   ;; This returns a result in the time format of #(hh mm ss), the Erlang
   ;; 'time' data type.
   (calendar:seconds_to_time (modulo-24h (floor (duration ts2 ts1 #(seconds)))
                                         #(seconds))))
  ((ts2 ts1 #(formatted))
   (lists:flatten
    (io_lib:format "~2..0w:~2..0w:~2..0w"
                   (tuple_to_list (duration ts2 ts1 #(time))))))
  ((ts2 ts1 opts)
   (micro-> (duration ts2 ts1 #(micro)) opts)))
