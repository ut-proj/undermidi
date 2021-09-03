(defmodule undermidi.go.shared
  (export all))

(defun use-go-src? ()
  (list_to_atom (os:getenv "USE_GO_SRC" "false")))

(defun midiserver ()
  (if (use-go-src?)
    (os:getenv "MIDISERVER")
    (os:getenv "PRECOMPILED_MIDISERVER")))
