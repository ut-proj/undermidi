(defmodule um.cc
  (export all))

(defun set (device channel control value)
  (um.ml:send device (midimsg:cc channel control value)))

(defun schedule (device channel control value ms)
  (timer:apply_after (abs (round ms))
                     'um.cc
                     'set
                     (list device channel control value)))

(defun ramp (device channel control start-val end-val dur)
  (ramp device
        channel
        control
        (undermidi.util:linear-ramp start-val end-val dur)))

(defun ramp
  ((device channel control `#m(values ,vals ms-per-value ,time-incr))
   (list-comp ((<- `#(,index ,val) (lists:enumerage 0 vals)))
     (schedule device channel control val (* index time-incr)))))

(defun cycle (device channel control start-val end-val dur)
  (ramp device
        channel
        control
        (undermidi.util:linear-cycle start-val end-val dur)))

(defun sustain-on (device channel)
  (set device channel 64 127))

(defun sustain-off (device channel)
  (set device channel 64 0))

(defun soft-pedal-on (device channel)
  (set device channel 66 127))

(defun soft-pedal-off (device channel)
  (set device channel 66 0))
