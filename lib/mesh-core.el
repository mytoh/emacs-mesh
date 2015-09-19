;;; mesh-core -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'seq)
(require 'glof)
(require 'mesh-class "lib/mesh-class")

(defcustom mesh:default-tab-name "eshell"
  "default tab name")

(defcustom mesh:default-session-name "main"
  "default session name")

(defconst mesh:*window-configuration-name*
  :mesh-winconf)

(defvar mesh:*sessions* nil)

(cl-defun mesh:sessions ()
  mesh:*sessions*)

(cl-defun mesh:unset-sessions ()
  (setq mesh:*sessions* nil))

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

(cl-defun mesh:find-next-pane (current-pane panes)
  (cl-letf* ((indices (seq-sort #'< (seq-map (lambda (p) (glof:get p :index)) panes)))
             (max-index (seq-max indices))
             (mix-index (seq-min indices))
             (current-index (glof:get current-pane :index))
             (next-index (mesh:find-next-index current-index
                                               max-index
                                               indices)))
    (if next-index
        (seq-find
         (lambda (pane)
           (eq next-index (glof:get pane :index)))
         panes)
      (seq-find
       (lambda (pane)
         (eq mix-index (glof:get pane :index)))
       panes))))

(cl-defun mesh:find-next-tab (current-tab tabs)
  (cl-letf* ((indices (seq-sort #'< (seq-map (lambda (tab) (glof:get tab :index)) tabs)))
             (max-index (seq-max indices))
             (min-index (seq-min indices))
             (current-index (glof:get current-tab :index))
             (next-index (mesh:find-next-index current-index
                                               max-index
                                               indices)))
    (if next-index
        (seq-find
         (lambda (tab)
           (eq next-index (glof:get tab :index)))
         tabs)
      (seq-find
       (lambda (tab)
         (eq min-index (glof:get tab :index)))
       tabs))))

(cl-defun mesh:find-next-session (current-session sessions)
  (cl-letf* ((current-session-pos (cl-position-if
                                   (lambda (s)
                                     (cl-equalp (mesh:get-name current-session)
                                                (mesh:get-name s)))
                                   sessions)))
    (pcase (1- (length sessions))
      (0 nil)
      ((pred (eq current-session-pos))
       (cl-first sessions))
      ((pred (< current-session-pos))
       (seq-elt sessions (1+ current-session-pos)))
      (_ nil))))

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
  (cl-letf* ((indices (seq-sort #'< (seq-map (lambda (p) (glof:get p :index)) tabs)))
             (min-index (seq-min indices))
             (max-index (seq-max indices))
             (current-index (glof:get current-tab :index))
             (prev-index (mesh:find-prev-index current-index
                                               min-index indices)))
    (if prev-index
        (seq-find
         (lambda (tab)
           (eq prev-index (glof:get tab :index)))
         tabs)
      (seq-find
       (lambda (tab)
         (eq max-index (glof:get tab :index)))
       tabs))))

(cl-defun mesh:find-prev-index (index min-index lst)
  (if (eq index min-index)
      nil
    (cl-letf ((prev (mesh:find-prev-index-rec index min-index lst)))
      (if prev prev
        (mesh:find-prev-index-rec (1- index) min-index lst)))))

(cl-defun mesh:find-prev-index-rec (index min-index lst)
  (if (or (null lst)
          (eq index min-index))
      nil
    (cl-letf* ((target-index (1- index))
               (prev (cl-find target-index lst)))
      (if prev
          prev
        (mesh:find-prev-index-rec index min-index (cl-rest lst))))))


;; (cl-defun mesh:find-next-session (current-session sessions)
;;   (cl-letf* ((current-session-pos (cl-position
;;                                    current-session
;;                                    sessions)))
;;     (pcase (1- (length sessions))
;;       (0 nil)
;;       ((pred (eq current-session-pos))
;;        (cl-first sessions))
;;       ((pred (< current-session-pos))
;;        (seq-elt sessions (1+ current-session-pos)))
;;       (_ nil))))


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
    (seq-difference (number-sequence 0 (seq-max indices))
                    indices)))

;; (cl-defun mesh:find-missing-index (fn lst)
;;   (cl-letf ((indices (seq-sort #'< (seq-map fn lst))))
;;     (cl-labels
;;         ((rec (lst res)
;;            (pcase lst
;;              (`(,head ,snd . ,_)
;;                (append
;;                 (if (eq (1+ head)
;;                         snd)
;;                     '()
;;                   (list (1+ head)))
;;                 (if (seq-find
;;                      (lambda (i) (eq i (1- head)))
;;                      res)
;;                     '()
;;                   (list (1- head)))
;;                 (rec (cl-rest lst) res)))
;;              (_ res))))
;;       (rec indices
;;            (pcase indices
;;              (`(0 . ,_)
;;                '())
;;              (_ '(0)))))))

(cl-defun mesh:find-last (lst)
  (cl-letf* ((indices (seq-map (lambda (thing) (glof:get thing :index)) lst))
             (max-index (seq-max indices)))
    max-index))

(provide 'mesh-core)

;;; mesh-core.el ends here
