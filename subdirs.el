(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path))

(push (expand-file-name "org-mode/lisp") load-path)

(push (expand-file-name "hu.dwim.environment/emacs") load-path)
