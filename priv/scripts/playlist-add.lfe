#! /usr/bin/env lfescript

;;; --------------------
;;; entry point function
;;; --------------------

(defun main (args)
  (net_kernel:start 'undermidiclient@localhost #m(name_domain shortnames dist_listen true hidden false))
  (let ((script-name (escript:script_name))
        (`#m(#"name" ,n #"file" ,f) (maps:from_list
                                     (list-comp ((<- `(,k ,v) (lutil-list:chunks args 2)))
                                       `#(,k ,v)))))
    (lfe_io:format "Running script '~s' with args ~p ...~n" `(,script-name ,args))
    (lfe_io:format "Args: ~p~n" `(,(init:get_arguments)))
    (lfe_io:format "Remote node ping: ~p~n" (list (net_adm:ping 'undermidi@localhost)))
    (lfe_io:format "Node: ~p~n" (list (node)))
    (lfe_io:format "Local node is alive? ~p~n" (list (erlang:is_alive)))
    (lfe_io:format "Local node ping: ~p~n" (list (net_adm:ping 'undermidiclient@localhost)))
    (lfe_io:format "Node state: ~p~n" (list (net_kernel:get_state)))
    (rpc:cast 'undermidi@localhost
              'undermidi.playlist
              'add
              (list (binary_to_list n) (binary_to_list f)))
    (lfe_io:format "Playlist state: ~p~n"
                   (list (rpc:call 'undermidi@localhost
                                   'undermidi.playlist
                                   'dump
                                   '())))))