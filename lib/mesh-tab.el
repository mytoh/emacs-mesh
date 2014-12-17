;;; mesh-tab -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)

(require 'mesh-class "lib/mesh-class")
(require 'mesh-pane "lib/mesh-pane")

(cl-defun mesh:tab--new (tab-name session-name &optional (index 0))
  (cl-letf* ((new-pane (mesh:pane--new session-name
                                       tab-name
                                       index)))
    (make-instance 'mesh:tab
                   :name tab-name
                   :conf nil
                   :index index
                   :current-pane new-pane
                   :session session-name
                   :panes (list new-pane))))

(cl-defun mesh:tab--command-new ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-session-name (mesh:get-name current-session)))
    (cl-letf* ((new-tab-name mesh:default-tab-name)
               (last-tab (car (last (mesh:get-tabs current-session))))
               (new-tab-index
                (if last-tab
                    (+ 1 (mesh:get-index last-tab))
                  0)))
      (cl-letf* ((new-tab (mesh:tab--new new-tab-name current-session-name
                                         new-tab-index))
                 (current-tab (mesh:get-current-tab current-session))
                 (new-current-tab current-tab)
                 (current-tabs (mesh:get-tabs current-session))
                 (new-session current-session))

        (oset new-current-tab :conf (current-window-configuration))
        (oset new-session :tabs
              (cl-subst new-current-tab current-tab current-tabs))

        (oset new-session :tabs
              (append (mesh:get-tabs new-session)
                      (list new-tab)))
        (oset new-session :current-tab new-tab)
        (setq mesh:*session-list*
              (cl-subst new-session current-session
                        mesh:*session-list*))

        (cl-letf ((new-pane-buffer
                   (thread-first new-tab
                     mesh:get-panes
                     car
                     mesh:get-buffer)))
          (delete-other-windows)
          (switch-to-buffer new-pane-buffer))))))

(cl-defun mesh:tab--command-split ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (thread-first current-session
                            mesh:get-current-tab))
             (pane (mesh:get-current-pane current-tab))
             (pane-last-index
              (apply #'max (seq-map
                            (lambda (p) (mesh:get-index p))
                            (mesh:get-panes current-tab))))
             (pane-missing-indices
              (mesh:pane--find-missing-index (mesh:get-panes current-tab))))
    (cl-letf* ((new-session current-session)
               (new-tab current-tab)
               (new-pane (mesh:pane--create
                          current-tab
                          (mesh:get-name current-session)
                          (if pane-missing-indices
                              (car pane-missing-indices)
                            (+ 1 pane-last-index)))))
      (cl-letf ((new-window (split-window nil nil 'below)))
        (set-window-buffer new-window (mesh:get-buffer new-pane))
        (select-window new-window))
      (oset new-tab :current-pane new-pane)
      (oset new-tab :panes
            (append (mesh:get-panes current-tab)
                    (list new-pane)))
      (oset new-tab :conf (current-window-configuration))
      (oset new-session :tabs
            (cl-subst new-tab current-tab
                      (mesh:get-tabs current-session)))
      (mesh:tab--subst-session-list
       new-session current-session))))

(cl-defun mesh:tab--command-vsplit ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (thread-first current-session
                            mesh:get-current-tab))
             (pane (mesh:get-current-pane current-tab))
             (pane-last-index
              (apply #'max (seq-map
                            (lambda (p) (mesh:get-index p))
                            (mesh:get-panes current-tab))))
             (pane-missing-indices
              (mesh:pane--find-missing-index (mesh:get-panes current-tab))))
    (cl-letf* ((new-session current-session)
               (new-tab current-tab)
               (new-pane (mesh:pane--create
                          current-tab
                          (mesh:get-name current-session)
                          (if pane-missing-indices
                              (car pane-missing-indices)
                            (+ 1 pane-last-index)))))
      (cl-letf ((new-window (split-window nil nil 'right)))
        (set-window-buffer new-window (mesh:get-buffer new-pane))
        (select-window new-window))
      (mesh:set-slots new-tab
        :current-pane new-pane
        :panes
        (append (mesh:get-panes current-tab)
                (list new-pane))
        :conf (current-window-configuration))
      (oset new-session :tabs
            (cl-subst new-tab current-tab
                      (mesh:get-tabs current-session)))
      (mesh:tab--subst-session-list
       new-session current-session))))

(cl-defun mesh:tab--subst-session-list (new-session old-session)
  (setq mesh:*session-list*
        (cl-subst new-session old-session
                  mesh:*session-list*)))

(cl-defun mesh:tab--command-next ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (mesh:get-current-tab current-session))
             (current-tabs (mesh:get-tabs current-session))
             (next-tab (mesh:tab--find-next current-tab current-tabs)))
    (when next-tab
      (cl-letf* ((new-current-tab current-tab)
                 (new-current-session current-session))
        (oset new-current-tab :conf (current-window-configuration))
        (oset new-current-session :tabs
              (cl-subst new-current-tab current-tab (mesh:get-tabs current-session)))
        (oset new-current-session :current-tab next-tab)
        (setq mesh:*session-list*
              (cl-subst new-current-session current-session mesh:*session-list*))
        (set-window-configuration (mesh:get-conf next-tab))))))

(defmethod mesh:tab--find-next ((current-tab mesh:tab) tabs)
  (cl-letf* ((current-tab-pos (cl-position
                               current-tab
                               tabs)))
    (cond ((eq (length tabs) 1)
           nil)
          ((eq (- (length tabs) 1) current-tab-pos)
           (car tabs))
          (t
           (cl-nth-value (+ current-tab-pos 1) tabs)))))

(cl-defun mesh:tab--command-prev ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (mesh:get-current-tab current-session))
             (current-tabs (mesh:get-tabs current-session))
             (prev-tab (mesh:tab--find-prev current-tab current-tabs)))
    (when prev-tab
      (cl-letf* ((new-current-tab current-tab)
                 (new-current-session current-session))
        (oset new-current-tab :conf (current-window-configuration))
        (oset new-current-session :tabs
              (cl-subst new-current-tab current-tab (mesh:get-tabs current-session)))
        (oset new-current-session :current-tab prev-tab)
        (setq mesh:*session-list*
              (cl-subst new-current-session current-session mesh:*session-list*))
        (set-window-configuration (mesh:get-conf prev-tab))))))

(defmethod mesh:tab--find-prev ((current-tab mesh:tab) tabs)
  (cl-letf* ((current-tab-pos (cl-position
                               current-tab
                               tabs)))
    (cond ((eq (length tabs) 1)
           nil)
          ((eq 0 current-tab-pos)
           (car (last tabs)))
          (t
           (cl-nth-value (- current-tab-pos 1) tabs)))))


(provide 'mesh-tab)

;;; mesh-tab.el ends here
