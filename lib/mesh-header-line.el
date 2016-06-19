;;; mesh-header-line -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'glof)
(require 'glof-thread)

(require 'mesh-core "lib/mesh-core")

(defcustom mesh:header-line-format
  `((:eval
     (mesh:header-line mesh:*state*)))
  "header line for mesh")

(cl-defun mesh:header-line (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (cursessions (glof:get state :sessions)))
    (string-join
     (seq-map
      (lambda (session)
        (if (cl-equalp session cursession)
            (glof:-> session
              :name
              (propertize 'face `( :foreground ,(face-foreground 'font-lock-type-face)
                                               :background ,(face-background 'default))))
          (glof:-> session
            :name
            (propertize 'face 'font-lock-comment-face))))
      cursessions)
     " ")))

(provide 'mesh-header-line)

;;; mesh-header-line.el ends here
