;;; mesh-pane -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'mesh-class "lib/mesh-class")
(require 'mesh-mode-line "lib/mesh-mode-line")

(defmethod mesh:pane--new (session-name tab-name tab-index)
  (cl-letf* ((buffer (mesh:pane--get-buffer-create
                      session-name tab-name tab-index 0)))
    (mesh:pane--make-buffer-eshell-mode buffer)
    (mesh:pane--set-mode-line buffer)
    (mesh:pane--make-pane
     session-name tab-name 0 buffer)))

(defmethod mesh:pane--create ((tab mesh:tab) session-name)
  (cl-letf* ((pane-index 0)
             (tab-name (mesh:get-name tab))
             (tab-index (mesh:get-index tab))
             (buffer
              (mesh:pane--get-buffer-create
               session-name tab-name tab-index pane-index)))
    (mesh:pane--make-buffer-eshell-mode buffer)
    (mesh:pane--set-mode-line buffer)
    (mesh:pane--make-pane
     session-name tab-name pane-index buffer)))

(defmethod mesh:pane--create ((tab mesh:tab) session-name pane-index)
  (cl-letf* ((tab-name (mesh:get-name tab))
             (tab-index (mesh:get-index tab))
             (buffer (mesh:pane--get-buffer-create
                      session-name tab-name tab-index pane-index)))
    (mesh:pane--make-buffer-eshell-mode buffer)
    (mesh:pane--set-mode-line buffer)
    (mesh:pane--make-pane
     session-name tab-name pane-index buffer)))

(cl-defun mesh:pane--make-pane
    (session-name tab-name pane-index buffer)
  (make-instance 'mesh:pane
                 :session session-name
                 :tab tab-name
                 :index pane-index
                 :buffer buffer))

(cl-defun mesh:pane--command-next ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (mesh:get-current-tab current-session))
             (current-pane (mesh:get-current-pane current-tab))
             (next-pane
              (mesh:pane--find-next-pane current-pane
                                         (mesh:get-panes current-tab))))
    (when next-pane
      (cl-letf ((next-pane-buffer (mesh:get-buffer next-pane)))
        (switch-to-buffer-other-window next-pane-buffer)
        (cl-letf* ((new-session current-session)
                   (new-tab current-tab))
          (mesh:set-slots new-tab
            :current-pane next-pane
            :conf (current-window-configuration))
          (mesh:set-slots new-session
            :tabs (cl-subst new-tab current-tab
                            (mesh:get-tabs current-session)))
          (mesh:tab--subst-session-list
           new-session current-session))))))

(defmethod mesh:pane--find-next-pane ((pane mesh:pane) panes)
  (cl-letf* ((current-position
              (cl-position pane panes)))
    (cond ((eq (length panes) 1)
           nil)
          ((eq (- (length panes) 1) current-position)
           (car panes))
          ((< current-position (- (length panes) 1))
           (cl-nth-value (+ current-position 1) panes))
          (t nil))))

(cl-defun mesh:pane--command-kill ()
  (cl-letf* ((old-session (mesh:current-session))
             (old-tab (mesh:get-current-tab old-session))
             (old-pane (mesh:get-current-pane old-tab))
             (next-pane
              (mesh:pane--find-next-pane old-pane
                                         (mesh:get-panes old-tab))))
    (cond
      (next-pane
       (mesh:pane--command-next)
       (with-current-buffer (mesh:get-buffer old-pane)
         (cl-letf ((window-to-kill (get-buffer-window (mesh:get-buffer old-pane))))
           (kill-buffer (mesh:get-buffer old-pane))
           (delete-window window-to-kill)))
       (cl-letf* ((new-session old-session)
                  (new-tab old-tab))
         (mesh:set-slots new-tab
           :current-pane next-pane
           :conf (current-window-configuration)
           :panes (cl-remove old-pane (mesh:get-panes old-tab)))
         (mesh:set-slots new-session
           :tabs (cl-subst new-tab old-tab
                           (mesh:get-tabs old-session)))
         (mesh:tab--subst-session-list
          new-session old-session)))
      ;; TODO kill a last pane with a session that belongs to
      (t
       ))))

(cl-defun mesh:pane--make-buffer-eshell-mode (buffer)
  (with-current-buffer buffer
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))))

(cl-defun mesh:pane--set-mode-line (buffer)
  (with-current-buffer buffer
    (setq mode-line-format
          mesh:mode-line-format)
    (force-mode-line-update)))

(cl-defun mesh:pane--get-buffer-create
    (session-name tab-name tab-index pane-index)
  (get-buffer-create
   (format"*%s:%s.%s:%s*"
          session-name
          tab-name
          tab-index
          pane-index)))


(provide 'mesh-pane)

;;; mesh-pane.el ends here
