;;;;;;;;;;;;;;;;;
;;; Constants ;;;
;;;;;;;;;;;;;;;;;

;; These CC take values in the range of 0-127

(defun modulation () 1)
(defun wave-voice-a () 24)
(defun wave-voice-b () 25)
(defun wave-voice-c () 26)
(defun wave-voice-d () 27)
(defun filter-env-amount () 47)
(defun lfo-1-waveform () 54)
(defun lfo-1-destination () 56)
(defun lfo-2-waveform () 55)

;; The device-versions of these CC take values in the range of 0-99, however
;; they have been adapted here to take the more standard range of 0-127 in LFE
;; and then output values scaled for the device in the range of 0-99.

(defun lfo-1-amount () 70)
(defun lfo-1-rate() 72)
(defun lfo-2-amount () 28)
(defun lfo-2-rage () 73)
(defun amp-attack () 81)
(defun amp-decay () 82)
(defun amp-sustain () 83)
(defun amp-release () 84)
(defun filter-resonance () 71)
(defun filter-cutoff () 74)
(defun filter-attack () 85)
(defun filter-decay () 86)
(defun filter-sustain () 87)
(defun filter-release () 88)
(defun chorus-depth () 91)
(defun chorus-rate () 92)
(defun fine-turning-voice-a () 111)
(defun fine-turning-voice-b () 112)
(defun fine-turning-voice-c () 113)
(defun fine-turning-voice-d () 114)
(defun course-turning-voice-a () 115)
(defun course-turning-voice-b () 116)
(defun course-turning-voice-c () 117)
(defun course-turning-voice-d () 118)

;;;;;;;;;;;;;;;;;;;
;;; Controllers ;;;
;;;;;;;;;;;;;;;;;;;

;; These CC take values in the range of 0-127

(defun modulation (value) (midimsg:cc (modulation) value))
(defun wave-voice-a (value) (midimsg:cc (wave-voice-a) value))
(defun wave-voice-b (value) (midimsg:cc (wave-voice-b) value))
(defun wave-voice-c (value) (midimsg:cc (wave-voice-c) value))
(defun wave-voice-d (value) (midimsg:cc (wave-voice-d) value))
(defun filter-env-amount (value) (midimsg:cc (filter-env-amount) value))
(defun lfo-1-waveform (value) (midimsg:cc (lfo-1-waveform) value))
(defun lfo-1-destination (value) (midimsg:cc (lfo-1-destination) value))
(defun lfo-2-waveform (value) (midimsg:cc (lfo-2-waveform) value))

;; The device-versions of these CC take values in the range of 0-99, however
;; they have been adapted here to take the more standard range of 0-127 in LFE
;; and then output values scaled for the device in the range of 0-99.

(defun scale (value) (lutil-math:midi-scale value #(0 99)))

(defun lfo-1-amount (value) (midimsg:cc (lfo-1-amount) (scale value)))
(defun lfo-1-rate (value) (midimsg:cc (lfo-1-rate) (scale value)))
(defun lfo-2-amount (value) (midimsg:cc (lfo-2-amount) (scale value)))
(defun lfo-2-rate (value) (midimsg:cc (lfo-2-rate) (scale value)))
(defun attack (value) (midimsg:cc (attack) (scale value)))
(defun decay (value) (midimsg:cc (decay) (scale value)))
(defun sustain (value) (midimsg:cc (sustain) (scale value)))
(defun release (value) (midimsg:cc (release) (scale value)))
(defun filter-resonance (value) (midimsg:cc (filter-resonance) (scale value)))
(defun filter-cutoff (value) (midimsg:cc (filter-cutoff) (scale value)))
(defun filter-attack (value) (midimsg:cc (filter-attack) (scale value)))
(defun filter-decay (value) (midimsg:cc (filter-decay) (scale value)))
(defun filter-sustain (value) (midimsg:cc (filter-sustain) (scale value)))
(defun filter-release (value) (midimsg:cc (filter-release) (scale value)))
(defun chorus-depth (value) (midimsg:cc (chorus-depth) (scale value)))
(defun chorus-rate (value) (midimsg:cc (chorus-rate) (scale value)))
(defun fine-turning-voice-a (value) (midimsg:cc (fine-turning-voice-a) (scale value)))
(defun fine-turning-voice-b (value) (midimsg:cc (fine-turning-voice-b) (scale value)))
(defun fine-turning-voice-c (value) (midimsg:cc (fine-turning-voice-c) (scale value)))
(defun fine-turning-voice-d (value) (midimsg:cc (fine-turning-voice-d) (scale value)))
(defun course-turning-voice-a (value) (midimsg:cc (course-turning-voice-a) (scale value)))
(defun course-turning-voice-b (value) (midimsg:cc (course-turning-voice-b) (scale value)))
(defun course-turning-voice-c (value) (midimsg:cc (course-turning-voice-c) (scale value)))
(defun course-turning-voice-d (value) (midimsg:cc (course-turning-voice-d) (scale value)))

;; This function is for display purposes when used in the REPL
;; and needs to be the last function in the include file.
(defun |-- loaded include: behringer/pro-vs-mini --| ()
  'ok)