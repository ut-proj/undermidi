(defmodule undermidi.util
  (export
   (banner 0)
   (bin->hex 1)
   (create-port 2)
   (priv-dir 0)
   (receive-line 2)
   (seq 2)
   (table-info 1)))

(include-lib "logjam/include/logjam.hrl")

(defun APPNAME () 'undermidi)

;;; General support functions

(defun create-port (cmd args)
  (let ((prog (io_lib:format "~s ~s" `(,cmd ,args))))
    (erlang:open_port `#(spawn ,prog) '(binary exit_status #(line 1)))))

(defun priv-dir ()
  (case (code:priv_dir (APPNAME))
    (`#(error ,_)
     (log-critical "~w priv dir not found~n" `(,(APPNAME)))
     (exit 'error))
    (dir dir)))

(defun receive-line (port timeout)
    (receive-line port timeout '()))

(defun receive-line (port timeout buffer)
  (receive
    (`#(,port #(exit_status ,exit-status))
     (log-error "Port unexpectedly exited with status ~p" `(exit-status))
     (erlang:term_to_binary '#(error port_exit)))
    (`#(,port #(data #"\n"))
     (log-debug "Skipping newline ...")
     (receive-line port timeout buffer))
    (`#(,port #(data #(,_flag ,data)))
     (log-debug "Got data: ~p" `(,data))
     (receive-line port timeout `(,data . ,buffer)))
    (`#(,port ,msg)
     (log-debug "Got message: ~p" `(,msg))
     (receive-line port timeout `(,msg . ,buffer)))
    (after timeout
      (log-debug "Buffer: ~p" `(,buffer))
      (let* ((rev (lists:reverse buffer))
             (bin (binary:list_to_bin rev)))
        (log-debug "Reversed: ~p" `(,rev))
        (log-debug "Binary: ~p" `(,bin))
        bin))))

(defun bin->hex (bin)
  (if (>= (list_to_integer (erlang:system_info 'otp_release)) 24)
    ;; Can we get around the linter for older versions?
    (let ((mod 'binary)
          (func 'encode_hex))
      (call mod func bin))
    (progn
      (log-debug "Getting hex for: ~s" `(,(lfe_io_format:fwrite1 "~p" (list bin))))
      (list_to_binary
       (lists:flatten
        (list-comp ((<- x (binary_to_list bin)))
          (io_lib:format "~2.16.0B" `(,x))))))))

(defun banner ()
  "Colour sequence:
   - A series of blues for the mushroom and spores
   - The yellow 'welcome'
   - 3 clumps of grass
   - Top of the 'd'
   - 1 clump of grass
   - Top of the 't'
   - 3 clumps of grass
   - Top row of 'undertone'
  "
  (let ((prompt "lfe> ")
        (data (binary_to_list (read-priv "text/banner.ascii")))
        (lcyan "\e[1;36m")
        (cyan "\e[36m")
        (lblue "\e[1;34m")
        (blue "\e[34m")
        (lyellow "\e[1;33m")
        (yellow "\e[33m")
        (magenta "\e[35m")
        (lgreen "\e[1;32m")
        (green "\e[32m")
        (white "\e[1;37m")
        (lgrey "\e[37m")
        (grey "\e[1;30m")
        (end "\e[0m"))
    ;; XXX using VERSION below (for latter replace) is a hack; should we
    ;;     instead return this whole thing as a format template, with ~s
    ;;     instead of VERSION?
    (io_lib:format data `(,lcyan ,end
                          ,blue  ,end
                          ,lcyan ,end

                          ,blue  ,end
                          ,lblue ,end
                          ,blue  ,end
                          ,lblue ,end

                          ,blue  ,end
                          ,lblue ,end
                          ,blue  ,end
                          ,lblue ,end

                          ,blue  ,end
                          ,cyan  ,end
                          ,blue  ,end
                          ,lblue ,end
                          ,blue  ,end

                          ,blue  ,end
                          ,cyan  ,end
                          ,blue  ,end
                          ,lblue ,end
                          ,blue  ,end

                          ,cyan  ,end
                          ,blue  ,end
                          ,lblue ,end
                          ,blue  ,end

                          ,cyan    ,end
                          ,blue    ,end
                          ,magenta ,end
                          ,blue    ,end
                          ,cyan    ,end
                          
                          ,cyan  ,end

                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end
                          ,lgreen ,end
                          ,cyan   ,end
                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end

                          ,white ,end

                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end

                          ,green  ,end

                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end
                          ,lgreen ,end
                          ,green  ,end
                          ,white ,end
                          ,green  ,end

                          ,white ,end
                          ,lgrey ,end
                          ,grey  ,end
                          ,(++ lyellow (undermidi:version) end)
                          ,(++ "Docs: "
                               lblue
                               "https://cnbbooks.github.io/lfe-music-programming/"
                               end
                               "\n"
                               "Bug report: "
                               lblue
                               "https://github.com/ut-proj/undermidi/issues/new"
                               end)
                          ,prompt))))

(defun read-priv (priv-rel-path)
  (lutil-file:read-priv 'undermidi priv-rel-path))

(defun seq (start end)
  (if (> end start)
    (lists:seq start end)
    (lists:seq end start)))

(defun table-info
  ((table-name) (when (is_atom table-name))
   (let ((info (ets:info table-name)))
     (case info
       ('undefined '())
       (_ info))))
  ((`#m(table-name ,table-name
        table-desc ,table-desc
        controlling-process ,controlling-process))
   (maps:merge `#m(controlling-process ,controlling-process
                   name ,table-name
                   description ,table-desc)
               (maps:from_list (table-info table-name)))))

(defun linear-ramp (start-val end-val dur)
  (let* ((vals (seq start-val end-val))
         (steps (length vals))
         (time-incr (/ (* 1000 dur) steps)))
    `#m(values ,vals
        ms-per-value ,time-incr)))

(defun linear-cycle (start-val end-val dur)
  (let* ((up (seq start-val end-val))
         (down (lists:reverse up))
         (vals (++ up down))
         (steps (length vals))
         (time-incr (/ (* 1000 dur) steps)))
    `#m(values ,vals
        ms-per-value ,time-incr)))
