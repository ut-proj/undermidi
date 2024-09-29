#! /usr/bin/env lfescript

;;; --------------------
;;; entry point function
;;; --------------------

(include-lib "logjam/include/logjam.hrl")

(defun main (args)
  (net_kernel:start 'undermidiclient@localhost #m(name_domain shortnames dist_listen true hidden false))
  (let ((script-name (escript:script_name))
        (`#m(#"name" ,n #"file" ,f) (maps:from_list
                                     (list-comp ((<- `(,k ,v) (lutil-list:chunks args 2)))
                                       `#(,k ,v))))
        (cfg-file "config/script.config"))
    (logjam:set-config `#(path ,cfg-file))
    (logger:debug "Running script '~s' with args ~p ..." `(,script-name ,args))
    (logger:debug "Args: ~p" `(,(init:get_arguments)))
    (logger:debug "Remote node ping: ~p" (list (net_adm:ping 'undermidi@localhost)))
    (logger:debug "Node: ~p" (list (node)))
    (logger:debug "Local node is alive? ~p" (list (erlang:is_alive)))
    (logger:debug "Local node ping: ~p" (list (net_adm:ping 'undermidiclient@localhost)))
    (logger:debug "Node state: ~p" (list (net_kernel:get_state)))
    (rpc:cast 'undermidi@localhost
              'undermidi.player.queue
              'add
              (list (binary_to_list n) (binary_to_list f)))
    (lfe_io:format "Playlist state: ~p~n"
                   (list (rpc:call 'undermidi@localhost
                                   'undermidi.player.queue
                                   'dump
                                   '())))))
