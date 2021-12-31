(defmodule um-time-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

;;; Test data

(defun timestamp1 () #(1640 889112 155492))
(defun timestamp2 () #(1640 889127 904799))
(defun timestamp3 () #(1640 889505 518183))
(defun timestamp4 () #(1640 900000 518183))
(defun timestamp5 () #(1650 900000 518183))

;;; Tests

(deftest micro->seconds
  (is-equal 3.0
            (um.time:micro-> 3000000 #(seconds)))
  (is-equal 3.0
            (um.time:->seconds 3000000 #(micro))))

(deftest micro->minutes
  (is-equal 0.6
            (um.time:micro-> 36000000 #(minutes)))
  (is-equal 6.0
            (um.time:micro-> 360000000 #(minutes)))
  (is-equal 60.0
            (um.time:micro-> 3600000000 #(minutes)))
  (is-equal 600.0
            (um.time:micro-> 36000000000 #(minutes))))

(deftest micro->hours
  (is-equal 10.0
            (um.time:micro-> 36000000000 #(hours))))

(deftest minutes->seconds
  (is-equal (* 5 60)
            (um.time:->seconds 5 #(minutes)))
  (is-equal (* 3 60 60)
            (um.time:->seconds 3 #(hours)))
  (is-equal (* 2 24 60 60)
            (um.time:->seconds 2 #(days))))

(deftest modulo-24h
  (is-equal #(0 0 0)
            (um.time:modulo-24h #(0 0 0)))
  (is-equal #(1 0 0)
            (um.time:modulo-24h #(1 0 0)))
  (is-equal #(23 0 0)
            (um.time:modulo-24h #(23 0 0)))
  (is-equal #(0 0 0)
            (um.time:modulo-24h #(24 0 0)))
  (is-equal #(1 0 0)
            (um.time:modulo-24h #(25 0 0)))
  (is-equal #(23 0 0)
            (um.time:modulo-24h #(47 0 0)))
  (is-equal #(0 0 0)
            (um.time:modulo-24h #(48 0 0)))
  (is-equal 10888
            (um.time:modulo-24h (floor (um.time:duration (timestamp4)
                                                         (timestamp1)
                                                         #(seconds)))
                                #(seconds)))
  (is-equal 74888
            (um.time:modulo-24h (floor (um.time:duration (timestamp5)
                                                         (timestamp1)
                                                         #(seconds)))
                                #(seconds))))

(deftest duration->micro
  (is-equal 15749307
            (um.time:duration (timestamp2)
                              (timestamp1)
                              #(micro))))

(deftest duration->seconds
  (is-equal 16
            (round (um.time:duration (timestamp2)
                                     (timestamp1)
                                     #(seconds))))
  (is-equal 393
            (round (um.time:duration (timestamp3)
                                     (timestamp1)
                                     #(seconds)))))
(deftest duration->minutes
  (is-equal 7
            (round (um.time:duration (timestamp3)
                                     (timestamp1)
                                     #(minutes))))
  (is-equal 181
            (round (um.time:duration (timestamp4)
                                     (timestamp1)
                                     #(minutes)))))

(deftest duration->hours
  (is-equal 3
            (round (um.time:duration (timestamp4)
                                     (timestamp1)
                                     #(hours)))))

(deftest duration->time
  (is-equal #(3 1 28)
            (um.time:duration (timestamp4)
                              (timestamp1)
                              #(time)))
  (is-equal #(20 48 8)
            (um.time:duration (timestamp5)
                              (timestamp1)
                              #(time))))

(deftest duration->formatted
  (is-equal "03:01:28"
            (um.time:duration (timestamp4)
                              (timestamp1)
                              #(formatted)))
  (is-equal "20:48:08"
            (um.time:duration (timestamp5)
                              (timestamp1)
                              #(formatted))))
