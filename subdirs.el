(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path))

(push (expand-file-name "m-creations/common/") load-path)

(push (expand-file-name "org-mode/lisp") load-path)
(add-to-list 'Info-default-directory-list (expand-file-name "org-mode/doc"))

(push (expand-file-name "magit/lisp") load-path)
(add-to-list 'Info-default-directory-list (expand-file-name "magit/Documentation"))

(push (expand-file-name "bbdb/lisp") load-path)
(add-to-list 'Info-default-directory-list (expand-file-name "bbdb/doc"))
