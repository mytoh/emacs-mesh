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

(defvar mesh:*state* ())

(cl-defun mesh:initial-state ()
  (glof:assoc ()
              :current-session nil
              :sessions []
              :inside-session-p nil))

(cl-defmacro mesh:defcommand (name &rest body)
  (declare (debug t)
           (indent 1))
  `(cl-defun ,name ()
     (interactive)
     (when (glof:get mesh:*state* :inside-session-p)
       ,@body)))

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
    (pcase (seq-length panes)
      (1 nil)
      ((let `nil next-index)
       (seq-find
        (lambda (pane)
          (eq mix-index (glof:get pane :index)))
        panes))
      ((let _ next-index)
       (seq-find
        (lambda (pane)
          (eq next-index (glof:get pane :index)))
        panes)))))

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
  (cl-letf* ((current-session-pos (seq-position
                                   sessions
                                   current-session
                                   (lambda (a b)
                                     (cl-equalp (glof:get a :name)
                                                (glof:get b :name))))))
    (pcase (1- (seq-length sessions))
      ((let `nil current-session-pos) nil)
      ((pred (eq current-session-pos))
       (mesh:first sessions))
      ((pred (< current-session-pos))
       (seq-elt sessions (1+ current-session-pos)))
      (_ nil))))

(cl-defun mesh:find-prev-session (current-session sessions)
  (cl-letf* ((current-session-pos (seq-position
                                   sessions
                                   current-session
                                   (lambda (a b)
                                     (cl-equalp (glof:get a :name)
                                                (glof:get b :name))))))
    (pcase (seq-length sessions)
      (1 nil)
      ((guard (zerop current-session-pos))
       (mesh:last sessions))
      (_ (seq-elt sessions (1- current-session-pos))))))

(cl-defun mesh:find-next-index-rec (index max-index lst)
  (if (or (seq-empty-p lst)
          (eq index max-index))
      nil
    (cl-letf* ((target-index (1+ index))
               (next (seq-find (lambda (e) (cl-equalp e target-index))  lst)))
      (if next
          next
        (mesh:find-next-index-rec index max-index (mesh:rest lst))))))

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
  (if (or (seq-empty-p lst)
          (eq index min-index))
      nil
    (cl-letf* ((target-index (1- index))
               (prev (seq-find (lambda (e) (cl-equalp e target-index)) lst)))
      (if prev
          prev
        (mesh:find-prev-index-rec index min-index (mesh:rest lst))))))


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

(cl-defun mesh:first (x)
  (seq-elt x 0))

(cl-defun mesh:second (x)
  (seq-elt x 1))

(cl-defun mesh:rest (x)
  (seq-drop x 1))

(cl-defun mesh:conj (a b)
  (pcase (list (type-of a) (type-of b))
    (`(cons vector)
      (seq-concatenate 'vector
                       (vector a) b))
    (`(vector vector)
      (seq-concatenate 'vector
                       (vector a) b))
    (`(,_ cons)
      (cons a b))
    (`(,_ nil)
      (cons a nil))
    (`(,_ vector)
      (seq-concatenate 'vector
                       (vector a) b))))

(cl-defun mesh:removev (f seq)
  (seq-into (seq-remove f seq) 'vector))

(cl-defun mesh:substitute-if (new f x)
  (seq-map
   (lambda (e)
     (if (funcall f e)
         new
       e))
   x))

(cl-defun mesh:substitute-if-v (new f x)
  (seq-into (seq-map
             (lambda (e)
               (if (funcall f e)
                   new
                 e))
             x)
            'vector))

(cl-defun mesh:last (seq)
  (seq-elt seq (1- (seq-length seq))))

(cl-defun mesh::handle-command (f state &rest args)
  (setq mesh:*state*
        (apply #'funcall f state args)))

(provide 'mesh-core)

;;; mesh-core.el ends here
