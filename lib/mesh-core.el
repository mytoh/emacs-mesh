;;; mesh-core -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'seq)
(require 'glof)
(require 'tupper)
(require 'colle)
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

(cl-defun mesh:find-next-pane (curpane panes)
  (cl-letf* ((indices (seq-sort #'< (colle:map (lambda (p) (glof:get p :index)) panes)))
             (maxindex (seq-max indices))
             (minindex (seq-min indices))
             (curindex (glof:get curpane :index))
             (nextindex (mesh:find-next-index curindex
                                            maxindex
                                            indices)))
    (pcase (seq-length panes)
      (1 nil)
      ((let `nil nextindex)
       (seq-find
        (lambda (pane)
          (eq minindex (glof:get pane :index)))
        panes))
      ((let _ nextindex)
       (seq-find
        (lambda (pane)
          (eq nextindex (glof:get pane :index)))
        panes)))))

(cl-defun mesh:find-next-tab (curtab tabs)
  (cl-letf* ((indices (seq-sort #'< (colle:map (lambda (tab) (glof:get tab :index)) tabs)))
             (maxindex (seq-max indices))
             (minindex (seq-min indices))
             (curindex (glof:get curtab :index))
             (nextindex (mesh:find-next-index curindex
                                            maxindex
                                            indices)))
    (if nextindex
        (seq-find
         (lambda (tab)
           (eq nextindex (glof:get tab :index)))
         tabs)
      (seq-find
       (lambda (tab)
         (eq minindex (glof:get tab :index)))
       tabs))))

(cl-defun mesh:find-next-session (cursession sessions)
  (cl-letf* ((cursessionpos (seq-position
                             sessions
                             cursession
                             (lambda (a b)
                               (cl-equalp (glof:get a :name)
                                          (glof:get b :name))))))
    (pcase (1- (seq-length sessions))
      (0 nil)
      ((let `nil cursessionpos) nil)
      ((pred (eq cursessionpos))
       (mesh:first sessions))
      ((pred (< cursessionpos))
       (seq-elt sessions (1+ cursessionpos)))
      (_ nil))))

(cl-defun mesh:find-prev-session (cursession sessions)
  (cl-letf* ((cursessionpos (seq-position
                             sessions
                             cursession
                             (lambda (a b)
                               (cl-equalp (glof:get a :name)
                                          (glof:get b :name))))))
    (pcase (seq-length sessions)
      (1 nil)
      ((guard (zerop cursessionpos))
       (mesh:last sessions))
      (_ (seq-elt sessions (1- cursessionpos))))))

(cl-defun mesh:find-next-index-rec (index maxindex lst)
  (if (or (seq-empty-p lst)
         (eq index maxindex))
      nil
    (cl-letf* ((target (1+ index))
               (next (seq-find (lambda (e) (cl-equalp e target))  lst)))
      (if next
          next
        (mesh:find-next-index-rec index maxindex (mesh:rest lst))))))

(cl-defun mesh:find-next-index (index maxindex lst)
  (if (eq index maxindex)
      nil
    (cl-letf ((next (mesh:find-next-index-rec index maxindex lst)))
      (if next
          next
        (mesh:find-next-index-rec (1+ index) maxindex lst)))))


(cl-defun mesh:find-prev-tab (curtab tabs)
  (cl-letf* ((indices (seq-sort #'< (colle:map (lambda (p) (glof:get p :index)) tabs)))
             (minindex (seq-min indices))
             (maxindex (seq-max indices))
             (curindex (glof:get curtab :index))
             (previndex (mesh:find-prev-index curindex
                                            minindex indices)))
    (if previndex
        (seq-find
         (lambda (tab)
           (eq previndex (glof:get tab :index)))
         tabs)
      (seq-find
       (lambda (tab)
         (eq maxindex (glof:get tab :index)))
       tabs))))

(cl-defun mesh:find-prev-index (index minindex lst)
  (if (eq index minindex)
      nil
    (cl-letf ((prev (mesh:find-prev-index-rec index minindex lst)))
      (if prev prev
        (mesh:find-prev-index-rec (1- index) minindex lst)))))

(cl-defun mesh:find-prev-index-rec (index minindex lst)
  (if (or (seq-empty-p lst)
         (eq index minindex))
      nil
    (cl-letf* ((target (1- index))
               (prev (seq-find (lambda (e) (cl-equalp e target)) lst)))
      (if prev
          prev
        (mesh:find-prev-index-rec index minindex (mesh:rest lst))))))


(cl-defun mesh:find-missing-index (fn lst)
  (cl-letf ((indices (seq-sort #'< (colle:map fn lst))))
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
  (cl-letf* ((indices (colle:map (lambda (thing) (glof:get thing :index)) lst))
             (maxindex (seq-max indices)))
    maxindex))

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
  (colle:map
   (lambda (e)
     (if (funcall f e)
         new
       e))
   x))

(cl-defun mesh:substitute-if-v (new f x)
  (colle:map
   (lambda (e)
     (if (funcall f e)
         new
       e))
   x))

(cl-defun mesh:last (seq)
  (seq-elt seq (1- (seq-length seq))))

(cl-defun mesh::update (f &rest args)
  (apply #'tupper:update! 'mesh:*state* f args))

(provide 'mesh-core)

;;; mesh-core.el ends here
