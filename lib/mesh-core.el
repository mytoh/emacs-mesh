;;; mesh-core -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'seq)
(require 'mesh-class "lib/mesh-class")

(defcustom mesh:default-tab-name "eshell"
  "default tab name")

(defcustom mesh:default-session-name "main"
  "default session name")

(defconst mesh:*window-configuration-name*
  :mesh-winconf)

(defvar mesh:*session-list* nil)

(cl-defun mesh:session-list ()
  mesh:*session-list*)

(cl-defun mesh:unset-session-list ()
  (setq mesh:*session-list* nil))

(defvar mesh:*inside-session-p* nil)

(cl-defun mesh:inside-session-p ()
  (and mesh:*inside-session-p*
       (eq major-mode 'eshell-mode)))

(cl-defun mesh:set-inside-session ()
  (setq mesh:*inside-session-p* t))

(cl-defun mesh:unset-inside-session ()
  (setq mesh:*inside-session-p* nil))

(defvar mesh:*current-session* nil)

(cl-defun mesh:current-session ()
  mesh:*current-session*)

(cl-defun mesh:set-current-session (session)
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
                (pcase lst
                  (`()
                    (reverse res))
                  (_
                   (rec
                    obj
                    (cddr lst)
                    (cons
                     `(setf (slot-value ,obj ,(cl-first lst)) ,(cl-second lst))
                     res))))))
    `(cl-locally
         ,@(rec object body '()))))

(cl-defun mesh:find-next (thing lst)
  (pcase thing
    (`[:pane ,pane] (mesh:find-next-pane pane lst))
    (`[:tab ,tab] (mesh:find-next-tab tab lst))
    (`[:session ,session] (mesh:find-next-session session lst))))

(cl-defun mesh:find-prev (thing lst)
  (pcase thing
    (`[:tab ,tab] (mesh:find-prev-tab tab lst))
    (`[:session ,session] (mesh:find-prev-session session lst))))


(cl-defun mesh:find-next-pane (pane panes)
  (cl-letf* ((current-position
              (cl-position pane panes)))
    (pcase (1- (length panes))
      (0 nil)
      ((pred (eq current-position))
       (cl-first panes))
      ((pred (< current-position))
       (seq-elt panes (1+ current-position)))
      (_ nil))))

(cl-defun mesh:find-next-tab (current-tab tabs)
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
    (cl-letf* ((target-index (1+ index))
               (next (cl-find target-index lst)))
      (if next
          next
        (mesh:find-next-index-rec index max-index (cl-rest lst))))))

(cl-defun mesh:find-next-index (index max-index lst)
  (if (eq index max-index)
      nil
    (cl-letf ((next (mesh:find-next-index-rec index max-index lst)))
      (if next
          next
        (mesh:find-next-index-rec (1+ index) max-index lst)))))


(cl-defun mesh:find-prev-tab (current-tab tabs)
  (cl-letf* ((current-tab-pos (cl-position
                               current-tab
                               tabs)))
    (pcase (length tabs)
      (1 nil)
      ((guard (zerop current-tab-pos))
       (cl-first (last tabs)))
      (_
       (seq-elt tabs (1- current-tab-pos))))))


(cl-defun mesh:find-next-session (current-session sessions)
  (cl-letf* ((current-session-pos (cl-position
                                   current-session
                                   sessions)))
    (pcase (1- (length sessions))
      (0 nil)
      ((pred (eq current-session-pos))
       (cl-first sessions))
      ((pred (< current-session-pos))
       (seq-elt sessions (1+ current-session-pos)))
      (_ nil))))


(cl-defun mesh:find-prev-session (current-session sessions)
  (cl-letf* ((current-session-pos (cl-position
                                   current-session
                                   sessions)))
    (pcase (length sessions)
      (1 nil)
      ((guard (zerop current-session-pos))
       (cl-first (last sessions)))
      (_ (seq-elt sessions (1- current-session-pos))))))

(cl-defun mesh:find-missing-index (fn lst)
  (cl-letf ((indices (seq-sort #'< (seq-map fn lst))))
    (cl-labels
        ((rec (lst res)
           (pcase lst
             (`(,_ ,_ . ,_)
               (append
                (if (eq (1+ (cl-first lst))
                        (cl-second lst))
                    '()
                  (list (1+ (cl-first lst))))
                (rec (cl-rest lst) res)))
             (_ res))))
      (rec indices '()))))

(cl-defun mesh:find-last (lst)
  (cl-letf* ((indices (seq-map #'mesh:get-index lst))
             (max-index (apply #'max indices)))
    max-index))

(provide 'mesh-core)

;;; mesh-core.el ends here
