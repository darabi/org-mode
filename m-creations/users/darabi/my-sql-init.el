;; -*- lexical-binding: t; mode: emacs-lisp -*-

(setq sql-connection-alist
      '((horreum-test (sql-product 'mysql)
         (sql-port 3306)
         (sql-server "test-horreum.prime-research.local")
         (sql-user "pam")
         (sql-database "pam"))
        (horreum-prod (sql-product 'mysql)
         (sql-port 3306)
         (sql-server "192.168.0.51")
         (sql-user "horreum")
         (sql-database "pam"))
        (metadb-test (sql-product 'mysql)
         (sql-port 3306)
         (sql-server "test-metadata.prime-research.local")
         (sql-user "metadata")
         (sql-database "metadata"))
        (metadb-prod (sql-product 'mysql)
         (sql-port 3306)
         (sql-server "metadb.prime.adns.de")
         (sql-user "metadata")
         (sql-database "metadata"))
        (blubb-test (sql-product 'mysql)
         (sql-port 3306)
         (sql-server "test-metadata.prime-research.local")
         (sql-user "metadata")
         (sql-database "metadata"))))

(defun connection-product (connection)
  (cadadr (assoc 'sql-product (cdr (assoc connection sql-connection-alist)))))

(defun my-sql-connect (connection)
  (require 'my-passwords "my-passwords.el.gpg")
  (let ((connection-info (assoc connection sql-connection-alist))
        (sql-password (car (last (assoc connection my-sql-password)))))
    (delete sql-password connection-info)
    (nconc connection-info `((sql-password ,sql-password)))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info))

  ;; connect to database
  (setq sql-product (connection-product connection))
  (sql-connect connection))

;; create interactive access functions for the connections
(dolist (conn sql-connection-alist)
  (let ((name (intern (concat "my-sql-" (symbol-name (car conn))))))
    ;; TODO: where does my-sql-nil come from?
    (unless (eql name 'my-sql-nil)
      (defalias name
          (lambda ()
            (interactive)
            (my-sql-connect (car conn)))))))

(provide 'my-sql-init)
