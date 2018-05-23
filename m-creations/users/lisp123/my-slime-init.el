(require 'slime-docker)

(setq slime-docker-implementations '((sbcl ("--eval" "(ql:quickload :swank)" "--eval" "(progn (swank-loader:init) (setf swank::*loopback-interface* \"0.0.0.0\") (swank:create-server))")
					   :image-name "mcreations/sbcl"
					   :image-tag "1.4.1-mc-2017-10-23")))


(provide 'my-slime-init)
