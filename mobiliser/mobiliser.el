;;; mobiliser.el -- a mode for useful bits and pieces when working with the mobiliser
;;
;; (c) 2012 m-creations gmbh
;;

(require 'cl)

(defvar mobiliser-dbmaintain-basic t)

(defun mobiliser-dbmaintain-find-next-index (directory)
  (let* ((lastfile (first (last (directory-files directory nil ".*\.sql" nil))))
         (numstring (car (split-string lastfile "_")))
         (len (length numstring)))
    (format (concat "%0" (number-to-string len) "d") (1+ (string-to-number numstring)))))

(defun mobiliser-dbmaintain-find-next-index-for-buffer ()
  (mobiliser-dbmaintain-find-next-index (file-name-directory (buffer-file-name))))

(defun mobiliser-dbmaintain-save-region-as-script (scriptname)
  (interactive "sScript name (use underscores as DBs are dumb): ")
  (let* ((index (mobiliser-dbmaintain-find-next-index-for-buffer))
         (name (concat index
                       (if mobiliser-dbmaintain-basic "_#Basic" nil)
                       "_" scriptname ".sql")))
    (if (region-active-p)
        (progn
          (write-region (region-beginning) (region-end) name t)
          (kill-region (region-beginning) (region-end))
          (message (concat "Killed region was written to file " name)))
        (message "No active region. Cannot save region to sql script."))))

(provide 'mobiliser)
