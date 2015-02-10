;;; mesh-core -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'seq)
(require 'mesh-class "lib/mesh-class")

(defcustom mesh:default-tab-name "eshell"
  "default tab name")

(defcustom mesh:default-session-name "main"
  "default session name")

(defconst mesh:window-configuration-name :mesh-winconf)

(defvar mesh:*session-list* nil)

(cl-defun mesh:session-list ()
  mesh:*session-list*)

(cl-defun mesh:unset-session-list ()
  (setq mesh:*session-list* nil))

(defvar mesh:*inside-session* nil)

(cl-defun mesh:inside-session-p ()
  (and mesh:*inside-session*
       (eq major-mode 'eshell-mode)))

(cl-defun mesh:set-inside-session ()
  (setq mesh:*inside-session* t))

(cl-defun mesh:unset-inside-session ()
  (setq mesh:*inside-session* nil))

(defvar mesh:*current-session* nil)

(cl-defun mesh:current-session ()
  mesh:*current-session*)

(cl-defmethod mesh:set-current-session ((session mesh:session))
  (setq mesh:*current-session* session))

(cl-defun mesh:unset-current-session ()
  (setq mesh:*current-session* nil))

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
                      `(setf (slot-value ,obj ,(car lst)) ,(cadr lst))
                      res))
                  (reverse res))))
    `(cl-locally
         ,@(rec object body '()))))

(cl-defgeneric mesh:find-next ()
  "find next thing")

(cl-defgeneric mesh:find-prev ()
  "find prev thing")


(cl-defmethod mesh:find-next ((pane mesh:pane) panes)
  (cl-letf* ((current-position
              (cl-position pane panes)))
    (pcase (1- (length panes))
      (0 nil)
      ((pred (eq current-position))
       (car panes))
      ((pred (< current-position))
       (cl-nth-value (cl-incf current-position) panes))
      (_ nil))))

(cl-defmethod mesh:find-next ((current-tab mesh:tab) tabs)
  (cl-letf* ((indices (seq-map #'mesh:get-index tabs))
             (max-index (apply #'max indices))
             (min-index (apply #'min indices))
             (current-index (mesh:get-index current-tab))
             (next-index (mesh:find-next-index current-index
                                               max-index
                                               indices)))
    (if next-index
        (cl-find-if
         (lambda (tab)
           (eq next-index (mesh:get-index tab)))
         tabs)
      (cl-find-if
       (lambda (tab)
         (eq min-index (mesh:get-index tab)))
       tabs))))

(cl-defun mesh:find-next-index-rec (index max-index lst)
  (if (or (null lst)
          (eq index max-index))
      nil
    (cl-letf* ((target-index (cl-incf index))
               (next (cl-find target-index lst)))
      (if next
          next
        (mesh:find-next-index-rec index max-index (cdr lst))))))

(cl-defun mesh:find-next-index (index max-index lst)
  (if (eq index max-index)
      nil
    (cl-letf ((next (mesh:find-next-index-rec index max-index lst)))
      (if next
          next
        (mesh:find-next-index-rec (cl-incf index) max-index lst)))))


(cl-defmethod mesh:find-prev ((current-tab mesh:tab) tabs)
  (cl-letf* ((current-tab-pos (cl-position
                               current-tab
                               tabs)))
    (pcase (length tabs)
      (1 nil)
      ((guard (eq 0 current-tab-pos))
       (car (last tabs)))
      (_
       (seq-elt tabs (cl-decf current-tab-pos))))))


(cl-defmethod mesh:find-next ((current-session mesh:session) sessions)
  (cl-letf* ((current-session-pos (cl-position
                                   current-session
                                   sessions)))
    (pcase (1- (length sessions))
      (0 nil)
      ((pred (eq current-session-pos))
       (car sessions))
      ((pred (< current-session-pos))
       (seq-elt sessions (cl-incf current-session-pos)))
      (_ nil))))


(cl-defmethod mesh:find-prev ((current-session mesh:session) sessions)
  (cl-letf* ((current-session-pos (cl-position
                                   current-session
                                   sessions)))
    (pcase (length sessions)
      (1 nil)
      ((guard (eq 0 current-session-pos))
       (car (last sessions)))
      (_ (seq-elt sessions (cl-decf current-session-pos))))))

(cl-defun mesh:find-missing-index (fn lst)
  (cl-letf ((indices (seq-sort #'< (seq-map fn lst))))
    (cl-labels
        ((rec (lst res)
           (cond ((and lst
                       (<= 2 (length lst)))
                  (append
                   (if (eq (cl-incf (car lst))
                           (cadr lst))
                       '()
                     (list (cl-incf (car lst))))
                   (rec (cdr lst) res)))
                 (t res))))
      (rec indices '()))))

(cl-defmethod mesh:find-last ((lst list))
  (cl-letf* ((indices (seq-map #'mesh:get-index lst))
             (max-index (apply #'max indices)))
    max-index))

(provide 'mesh-core)

;;; mesh-core.el ends here
