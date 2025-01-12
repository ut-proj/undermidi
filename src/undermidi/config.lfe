(defmodule undermidi.config
  (export all))

(defun display-banner? (cfg)
  (proplists:get_value
   'display_banner
   (proplists:get_value 'undermidi cfg)))
