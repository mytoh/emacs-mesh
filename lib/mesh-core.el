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

(defgeneric mesh:find-next ()
  "find next thing")

(defgeneric mesh:find-prev ()
  "find prev thing")

(defmethod mesh:find-next ((current-tab mesh:tab) tabs)
  (cl-letf* ((current-tab-pos (cl-position
                               current-tab
                               tabs)))
    (cond ((eq (length tabs) 1)
           nil)
          ((eq (- (length tabs) 1) current-tab-pos)
           (car tabs))
          (t
           (cl-nth-value (+ current-tab-pos 1) tabs)))))


(defmethod mesh:find-prev ((current-tab mesh:tab) tabs)
  (cl-letf* ((current-tab-pos (cl-position
                               current-tab
                               tabs)))
    (cond ((eq (length tabs) 1)
           nil)
          ((eq 0 current-tab-pos)
           (car (last tabs)))
          (t
           (cl-nth-value (- current-tab-pos 1) tabs)))))


(defmethod mesh:find-next ((current-session mesh:session) sessions)
  (cl-letf* ((current-session-pos (cl-position
                                   current-session
                                   sessions)))
    (cond ((eq (length sessions) 1)
           nil)
          ((eq (- (length sessions) 1) current-session-pos)
           (car sessions))
          ((< current-session-pos (- (length sessions) 1))
           (cl-nth-value (+ current-session-pos 1) sessions))
          (t nil))))


(defmethod mesh:find-prev ((current-session mesh:session) sessions)
  (cl-letf* ((current-session-pos (cl-position
                                   current-session
                                   sessions)))
    (cond ((eq (length sessions) 1)
           nil)
          ((eq 0 current-session-pos)
           (car (last sessions)))
          (t
           (cl-nth-value (- current-session-pos 1) sessions)))))


(provide 'mesh-core)

;;; mesh-core.el ends here
