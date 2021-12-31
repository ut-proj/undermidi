(defmodule um-transport-tests
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
