(defmodule shortcuts.linux
  (export all))

(defun template ()
  (filename:join (code:priv_dir 'undermidi) "desktop/templates/linux.mustache"))

(defun outdir ()
  "some/path")

(defun load ()
  (let ((`#(ok ,data) (file:read_file (template))))
    data))

(defun render (data-map)
  (bbmustache:render (load) data-map))

(defun write (file-name data-map)
  (file:write_file file-name (render data-map)))

(defun make ()
  (make (patches:list-all)))

(defun make
  (('())
   'ok)
  (((cons (= `#m(segments ,segs) patch-data) tail))
   (let* ((name (++ (string:join segs "-") ".desktop"))
          (path (filename:join (outdir) name)))
     (write path patch-data)
     (make tail))))
