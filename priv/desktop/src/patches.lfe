(defmodule patches
  (export all))

(defun dir ()
  (filename:join (code:priv_dir 'undermidi) "patches"))

(defun make-data ()
  (make-data ""))

(defun make-data (patch-path)
  (make-data patch-path "" '() "" "" ""))

(defun make-data (abs-path rel-path segs cat short long)
  `#m(abs-path ,abs-path
      rel-path ,rel-path
      segments ,segs
      category ,cat
      short-name ,short
      long-name ,long))

(defun list-all ()
  (filelib:fold_files (dir) ".*lfe$" 'true #'file-filter/2 '()))

(defun parse-path (abs-path)
  (let* ((`(,_ ,rel-path) (string:split abs-path (++ (dir) "/")))
         (`(,no-ext ,_) (string:split rel-path ".lfe" 'trailing))
         (segs (filename:split no-ext)))
    (if (< (length segs) 2)
      #(error "no category")
      (let* ((`(,cat ,short-name) segs)
             (cat (parse-category cat))
             (long-name (parse-name short-name)))
        (make-data abs-path rel-path segs cat short-name long-name)))))

(defun parse-category (cat)
  (case cat
    ("seqs" "Sequence")
    ("progs" "Progression")
    (_ cat)))

(defun parse-name (short-name)
  (let* ((words (string:split short-name "-" 'all))
         (caps (list-comp ((<- w words)) (string:titlecase w))))
    (string:join caps " ")))

(defun file-filter (filename acc)
  (let ((parsed (parse-path filename)))
    (if (is-error parsed)
      acc
      (cons parsed acc))))

(defun is-error
  ((`#(error ,_)) 'true)
  ((_) 'false))
