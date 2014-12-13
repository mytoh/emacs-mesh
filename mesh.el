;;; mesh -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'mesh-command "lib/mesh-command")

(defvar mesh-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-minor-mode mesh-mode
    "global minor mode for mesh"
  :global t
  :init-value nil
  :lighter " mesh"
  :keymap mesh-mode-map)

(cl-defun turn-on-mesh-mode ()
  (mesh-mode 1))

(provide 'mesh)

;;; mesh.el ends here
