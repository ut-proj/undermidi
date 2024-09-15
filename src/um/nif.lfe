(defmodule um.nif
  (export all))

(include-lib "sp_midi.hrl")

(defun lib-file () (LIB))

(defun nif-loaded? () (sp_midi:is_nif_loaded))
(defun nif-initialised? () (sp_midi:is_nif_initialized))

(defun initialise () (sp_midi:midi_init))
(defun deinitialise () (sp_midi:midi_deinit))

(defun send (a b) (sp_midi:midi_send a b))
(defun flush () (sp_midi:midi_flush))

(defun inputs () (sp_midi:midi_ins))
(defun outputs () (sp_midi:midi_outs))
(defun refresh () (sp_midi:midi_refresh_devices))

(defun have-pid? () (sp_midi:have_my_pid))

(defun current-ms () (sp_midi:get_current_time_microseconds))
(defun set-log-level (a) (sp_midi:set_log_level a))
(defun set-pid (a) (sp_midi:set_this_pid a))

(defun devices ()
  (refresh)
  `#m(inputs ,(lists:enumerate (inputs))
      outputs ,(lists:enumerate (outputs))))

#|

Example send or raw binary MIDI data in LFE REPL:

lfe> (ms.nif:initialise)
ok
lfe> (set device (car (ms.nif:outputs)))
("midi_bus_1")
lfe> (ms.nif:send device
                  (binary (1 (size 1))     ; Byte 1 - 1xxxxxxx -
                          (1 (size 3))     ; Byte 1 - x001xxxx - Note On
                          (0 (size 4))     ; Byte 1 - xxxx0000 - Channel 1 (0-15)
                          (0 (size 1))     ; Byte 2 - 0xxxxxxx -
                          (24 (size 7))    ; Byte 2 - x0011000 - Pitch (0-127)
                          (0 (size 1))     ; Byte 3 - 0xxxxxxx -
                          (100 (size 7)))) ; Byte 3 - x1100100 - Velocity (0-127)

Eventually, something like this should be possible:

lfe> (ms.nif:send device
                  (midibin:encode #(midi #(note_on (#(pitch 24)
                                                    #(velocity 100))))))

Reset all controllers:

lfe> (ms.nif:send device
                  (binary (1 (size 1))     ; Byte 1 - 1xxxxxxx -
                          (3 (size 3))     ; Byte 1 - x011xxxx - Control Change
                          (0 (size 4))     ; Byte 1 - xxxx0000 - Channel 1 (0-15)
                          (0 (size 1))     ; Byte 2 - 0xxxxxxx -
                          (121 (size 7))   ; Byte 2 - x1111001 - Controller Number (0-127)
                          (0 (size 1))     ; Byte 3 - 0xxxxxxx -
                          (0 (size 7))))   ; Byte 3 - x0000000 - Controller Value (0-127)

All notes off:

lfe> (ms.nif:send device
                  (binary (1 (size 1))     ; Byte 1 - 1xxxxxxx -
                          (3 (size 3))     ; Byte 1 - x011xxxx - Control Change
                          (0 (size 4))     ; Byte 1 - xxxx0000 - Channel 1 (0-15)
                          (0 (size 1))     ; Byte 2 - 0xxxxxxx -
                          (123 (size 7))   ; Byte 2 - x1111011 - Controller Number (0-127)
                          (0 (size 1))     ; Byte 3 - 0xxxxxxx -
                          (0 (size 7))))   ; Byte 3 - x0000000 - Controller Value (0-127)

|#