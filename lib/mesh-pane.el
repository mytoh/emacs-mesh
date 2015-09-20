;;; mesh-pane -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'glof)
(require 'mesh-class "lib/mesh-class")
(require 'mesh-core "lib/mesh-core")
(require 'mesh-mode-line "lib/mesh-mode-line")
(require 'mesh-header-line "lib/mesh-header-line")

(cl-defmethod mesh:pane--new (session-name tab-name tab-index)
  (cl-letf* ((buffer (mesh:pane--get-buffer-create
                      session-name tab-name tab-index 0)))
    (mesh:pane--make-buffer-eshell-mode buffer)
    (mesh:pane--set-mode-line buffer)
    (mesh:pane--set-header-line buffer)
    (mesh:pane--make-pane
     session-name tab-name 0 buffer)))

(cl-defmethod mesh:pane--create (tab session-name &optional (pane-index 0))
  (cl-letf* ((tab-name (glof:get tab :name))
             (tab-index (glof:get tab :index))
             (buffer (mesh:pane--get-buffer-create
                      session-name tab-name tab-index pane-index)))
    (mesh:pane--make-buffer-eshell-mode buffer)
    (mesh:pane--set-mode-line buffer)
    (mesh:pane--set-header-line buffer)
    (mesh:pane--make-pane
     session-name tab-name pane-index buffer)))

(cl-defun mesh:pane--make-pane
    (session-name tab-name pane-index buffer)
  (glof:assoc mesh:<pane>-template
              :session-name session-name
              :tab-name tab-name
              :index pane-index
              :buffer buffer))

(cl-defun mesh:pane--command-next ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (glof:get current-session :current-tab))
             (current-pane (glof:get current-tab :current-pane))
             (next-pane
              (mesh:find-next `[:pane ,current-pane]
                              (glof:get current-tab :panes))))
    (when next-pane
      (cl-letf ((next-pane-buffer (glof:get next-pane :buffer)))
        (switch-to-buffer-other-window next-pane-buffer)
        (cl-letf* ((new-session current-session))
          (cl-letf* ((new-tab (thread-first current-tab
                                (glof:assoc :current-pane next-pane)
                                (glof:assoc :conf (current-window-configuration))))
                     (new-tabs (cl-substitute-if new-tab
                                                 (lambda (tab)
                                                   (eq (glof:get tab :index)
                                                       (glof:get current-tab :index)))
                                                 (glof:get current-session :tabs))))
            (cl-letf ((new-session (thread-first new-session
                                     (glof:assoc :tabs new-tabs
                                                 :current-tab new-tab))))
              (mesh:set-current-session new-session)
              (mesh:tab--subst-session
               new-session current-session))))))))

(cl-defun mesh:pane--command-kill ()
  (cl-letf* ((old-session (mesh:current-session))
             (old-tab (glof:get old-session :current-tab))
             (old-tabs (glof:get old-session :tabs))
             (old-pane (glof:get old-tab :current-pane))
             (old-panes (glof:get old-tab :panes))
             (next-pane (mesh:find-next `[:pane ,old-pane]
                                        old-panes))
             (next-tab (mesh:find-next `[:tab ,old-tab]
                                       (glof:get old-session :tabs))))
    (pcase (seq-length old-panes)
      (1
       (cl-letf* ((new-session old-session))
         (cl-letf* ((new-tabs (seq-remove (lambda (tab)
                                            (eq (glof:get tab :index)
                                                (glof:get old-tab :index)))
                                          old-tabs))
                    (new-session (thread-first new-session
                                   (glof:assoc :tabs new-tabs
                                               :current-tab next-tab))))

           (mesh:set-current-session new-session)
           (mesh:tab--subst-session new-session old-session)
           (mesh:tab--kill-panes old-tab)
           (set-window-configuration (glof:get next-tab :conf)))))
      (_
       (mesh:pane--command-next)
       (mesh:pane--kill old-session old-tab old-pane next-pane))
      
      ;; (t
      ;;  ;; TODO kill session
      ;;  )
      )))

(cl-defun mesh:pane--kill (session tab pane next-pane)
  (cl-letf* ((old-session session)
             (old-tab tab)
             (old-pane pane))
    (with-current-buffer (glof:get old-pane :buffer)
      (cl-letf ((window-to-kill (get-buffer-window (glof:get old-pane :buffer))))
        (kill-buffer (glof:get old-pane :buffer))
        (delete-window window-to-kill)))
    (cl-letf* ((new-session old-session))
      (cl-letf* ((new-tab (thread-first old-tab
                            (glof:assoc :conf (current-window-configuration))
                            (glof:assoc :current-pane next-pane)
                            (glof:assoc :panes
                                        (seq-remove
                                         (lambda (pane)
                                           (eq (glof:get pane :index)
                                               (glof:get old-pane :index)))
                                         (glof:get old-tab :panes)))))
                 (new-tabs (cl-substitute-if new-tab
                                             (lambda (tab)
                                               (eq (glof:get tab :index)
                                                   (glof:get old-tab :index)))
                                             (glof:get old-session :tabs))))
        (cl-letf ((new-session (thread-first new-session
                                 (glof:assoc :tabs new-tabs
                                             :current-tab new-tab))))
          (mesh:set-current-session new-session)
          (mesh:tab--subst-session
           new-session old-session))))))

(cl-defun mesh:pane--make-buffer-eshell-mode (buffer)
  (with-current-buffer buffer
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))))

(cl-defun mesh:pane--set-mode-line (buffer)
  (with-current-buffer buffer
    (setq mode-line-format
          mesh:mode-line-format)
    (force-mode-line-update)))

(cl-defun mesh:pane--set-header-line (buffer)
  (with-current-buffer buffer
    (setq header-line-format
          mesh:header-line-format)))

(cl-defun mesh:pane--get-buffer-create
    (session-name tab-name tab-index pane-index)
  (get-buffer-create
   (format "*%s:%s.%s:%s*"
           session-name
           tab-name
           tab-index
           pane-index)))


(provide 'mesh-pane)

;;; mesh-pane.el ends here
