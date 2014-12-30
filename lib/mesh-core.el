;;; mesh-core -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'seq)
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


(defmethod mesh:find-next ((pane mesh:pane) panes)
  (cl-letf* ((current-position
              (cl-position pane panes)))
    (cond ((eq (length panes) 1)
           nil)
          ((eq (- (length panes) 1) current-position)
           (car panes))
          ((< current-position (- (length panes) 1))
           (cl-nth-value (+ current-position 1) panes))
          (t nil))))

(defmethod mesh:find-next ((current-tab mesh:tab) tabs)
  (cl-letf* ((current-tab-pos (cl-position
                               current-tab
                               tabs)))
    (pcase (- (length tabs) 1)
      (`0 nil)
      ((pred (eq  current-tab-pos))
       (car tabs))
      (_
       (seq-elt tabs (+ current-tab-pos 1))))))


(defmethod mesh:find-prev ((current-tab mesh:tab) tabs)
  (cl-letf* ((current-tab-pos (cl-position
                               current-tab
                               tabs)))
    (pcase (length tabs)
      (`1 nil)
      ((guard (eq 0 current-tab-pos))
       (car (last tabs)))
      (_
       (seq-elt tabs (- current-tab-pos 1))))))


(defmethod mesh:find-next ((current-session mesh:session) sessions)
  (cl-letf* ((current-session-pos (cl-position
                                   current-session
                                   sessions)))
    (pcase (- (length sessions) 1)
      (`0 nil)
      ((pred (eq current-session-pos))
       (car sessions))
      ((pred (< current-session-pos))
       (seq-elt sessions (+ current-session-pos 1)))
      (_ nil))))


(defmethod mesh:find-prev ((current-session mesh:session) sessions)
  (cl-letf* ((current-session-pos (cl-position
                                   current-session
                                   sessions)))
    (pcase (length sessions)
      (`1 nil)
      ((guard (eq 0 current-session-pos))
       (car (last sessions)))
      (_ (seq-elt sessions (- current-session-pos 1))))))

(cl-defun mesh:find-missing-index (fn lst)
  (cl-letf ((indices (seq-map fn lst)))
    (cl-labels
        ((rec (lst res)
           (cond ((and lst
                       (<= 2 (length lst)))
                  (append
                   (if (eq (+ 1 (car lst))
                           (cadr lst))
                       '()
                     (list (+ 1 (car lst))))
                   (rec (cdr lst) res)))
                 (t res))))
      (rec indices '()))))

(provide 'mesh-core)

;;; mesh-core.el ends here
