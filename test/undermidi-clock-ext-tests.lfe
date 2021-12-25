(defmodule undermidi-clock-ext-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun data-times-0 ()
  '())

(defun data-times-1 ()
  '(#(1640 454223 102638)))

(defun data-times-2 ()
  '(#(1640 455735 504794)
    #(1640 455734 778061)))

(defun data-times-3 ()
  '(#(1640 455986 548341)
 #(1640 455985 899442)
 #(1640 455985 254824)))

(defun data-times-many ()
  '(#(1640 455589 533148)
    #(1640 455588 935408)
    #(1640 455588 348230)
    #(1640 455587 760865)
    #(1640 455587 180894)
    #(1640 455586 600048)
    #(1640 455586 16081)
    #(1640 455585 436066)
    #(1640 455584 858904)
    #(1640 455584 280599)
    #(1640 455583 692120)
    #(1640 455583 107741)
    #(1640 455582 519676)
    #(1640 455581 939485)
    #(1640 455581 360297)
    #(1640 455580 776109)
    #(1640 455580 181810)
    #(1640 455579 609507)
    #(1640 455579 37186)
    #(1640 455578 466627)
    #(1640 455577 871465)
    #(1640 455577 301421)
    #(1640 455576 697328)
    #(1640 455576 117178)
    #(1640 455575 539192)
    #(1640 455574 949892)
    #(1640 455574 364854)))

(deftest bpm-empty-times
  (is-equal 0
            (undermidi.clock.ext:bpm (data-times-0) 1)))

(deftest bpm-1-time
  (is-equal 0
            (undermidi.clock.ext:bpm (data-times-1) 1))
  (is-equal 96
            (undermidi.clock.ext:bpm (data-times-2) 1))
  (is-equal 107
            (undermidi.clock.ext:bpm (data-times-3) 1))
  (is-equal 116
            (undermidi.clock.ext:bpm (data-times-many) 1)))

(deftest bpm-2-times
  (is-equal 0
            (undermidi.clock.ext:bpm (data-times-1) 2))
  (is-equal 96
            (undermidi.clock.ext:bpm (data-times-2) 2))
  (is-equal 108
            (undermidi.clock.ext:bpm (data-times-3) 2))
  (is-equal 117
            (undermidi.clock.ext:bpm (data-times-many) 2)))

(deftest bpm-3-times
  (is-equal 0
            (undermidi.clock.ext:bpm (data-times-1) 3))
  (is-equal 96
            (undermidi.clock.ext:bpm (data-times-2) 3))
  (is-equal 108
            (undermidi.clock.ext:bpm (data-times-3) 3))
  (is-equal 118
            (undermidi.clock.ext:bpm (data-times-many) 3)))

(deftest bpm-max-times
  (let ((max (undermidi.clock.ext:max-times)))
    (is-equal 0
              (undermidi.clock.ext:bpm (data-times-1) max))
    (is-equal 96
              (undermidi.clock.ext:bpm (data-times-2) max))
    (is-equal 108
              (undermidi.clock.ext:bpm (data-times-3) max))
    (is-equal 119
              (undermidi.clock.ext:bpm (data-times-many) max))))
