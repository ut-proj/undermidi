(defmodule undermidi.vers
  (export
   (version 0)
   (versions 0)))

(defun version ()
  (version 'undermidi))

(defun version (app-name)
  (application:load app-name)
  (case (application:get_key app-name 'vsn)
    (`#(ok ,vsn) vsn)
    (default default)))

(defun version-arch ()
  `#(architecture ,(erlang:system_info 'system_architecture)))

(defun version+name (app-name)
  `#(,app-name ,(version app-name)))

(defun versions-rebar ()
  `(,(version+name 'rebar)
    ,(version+name 'rebar3_lfe)))

(defun versions-langs ()
  `(,(version+name 'lfe)
    #(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver ,(erlang:system_info 'driver_version))))

(defun sonic-pi ()
  (let* ((filename (filename:join (undermidi.util:priv-dir) "sp_metadata.conf"))
         (`#(ok ,data) (file:consult filename))
         (sp (proplists:get_value 'sonic_pi data)))
    (proplists:get_value 'version sp)))
    
(defun versions ()
  (lists:append `((,(version+name 'undermidi)
                   ,(version+name 'undertheory)
                   ,(version+name 'midilib)
                   #(sonic-pi ,(sonic-pi)))
                  ,(versions-langs)
                  ,(versions-rebar)
                  (,(version-arch)))))
