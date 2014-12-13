;;; mesh-core -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'mesh-class "lib/mesh-class")

(defcustom mesh:default-tab-name "eshell"
  "default tab name")

(defcustom mesh:default-session-name "main"
  "default session name")

(defvar mesh:*session-list* nil)

(cl-defun mesh:session-list ()
  mesh:*session-list*)

(defvar mesh:*inside-session* nil)

(cl-defun mesh:inside-session-p ()
  (if (and mesh:*inside-session*
           (eq major-mode 'eshell-mode))
      t nil))

(cl-defun mesh:set-inside-session ()
  (setq mesh:*inside-session* t))

(cl-defun mesh:unset-inside-session ()
  (setq mesh:*inside-session* nil))

(defvar mesh:*current-session* nil)

(cl-defun mesh:current-session ()
  mesh:*current-session*)

(defmethod mesh:set-current-session ((session mesh:session))
  (setq mesh:*current-session* session))

(cl-defmacro mesh:defcommand (name &rest body)
  (declare (debug t)
           (indent 1))
  `(cl-defun ,name ()
     (interactive)
     (when (mesh:inside-session-p)
       ,@body)))

(cl-defmacro mesh:set-slots (object &rest body)
  (declare (debug t)
           (indent 1))
  (cl-labels ((rec (obj lst res)
                (if lst
                    (rec
                     obj
                     (cddr lst)
                     (cons
                      `(oset ,obj ,(car lst) ,(cadr lst))
                      res))
                  (reverse res))))
    `(cl-locally
         ,@(rec object body '()))))

(provide 'mesh-core)

;;; mesh-core.el ends here
