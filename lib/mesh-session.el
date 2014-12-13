;;; session -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-class "lib/mesh-class")
(require 'mesh-tab "lib/mesh-tab")


(cl-defun mesh:session--new (session-name sessions)
  (if-let ((found (cl-find-if (lambda (s) (cl-equalp (mesh:get-name s)
                                                session-name))
                              sessions)))
      found
    (cl-letf ((new-tab (mesh:tab--new
                        mesh:default-tab-name
                        session-name)))
      (make-instance 'mesh:session
                     :name session-name
                     :current-tab new-tab
                     :tabs (list new-tab)))))

;; (mesh:session--new "test" mesh:*session-list*)

(provide 'mesh-session)

;;; session.el ends here
