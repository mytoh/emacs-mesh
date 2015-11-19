;;; mesh-pane -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'glof)
(require 'mesh-class "lib/mesh-class")
(require 'mesh-core "lib/mesh-core")
(require 'mesh-mode-line "lib/mesh-mode-line")
(require 'mesh-header-line "lib/mesh-header-line")

(cl-defun mesh:pane--new (session-name tab-name tab-index)
  (cl-letf* ((buffer (mesh:pane--get-buffer-create
                      session-name tab-name tab-index 0)))
    (mesh:pane--make-buffer-eshell-mode buffer)
    (mesh:pane--set-mode-line buffer)
    (mesh:pane--set-header-line buffer)
    (mesh:pane--make-pane
     session-name tab-name 0 buffer)))

(cl-defun mesh:pane--create (state &optional (pane-index 0))
  (cl-letf* ((current-session (glof:get state :current-session))
             (session-name (glof:get current-session :name))
             (current-tab (glof:get current-session :current-tab))
             (tab-name (glof:get current-tab :name))
             (tab-index (glof:get current-tab :index))
             (buffer (mesh:pane--get-buffer-create
                      session-name tab-name tab-index pane-index)))
    (mesh:pane--make-buffer-eshell-mode buffer)
    (mesh:pane--set-mode-line buffer)
    (mesh:pane--set-header-line buffer)
    (mesh:pane--make-pane
     session-name tab-name pane-index buffer)))

(cl-defun mesh:pane--make-pane
    (session-name tab-name pane-index buffer)
  (glof:assoc mesh:<pane>
              :session-name session-name
              :tab-name tab-name
              :index pane-index
              :buffer buffer))

(cl-defun mesh:pane--command-next (state)
  (cl-letf* ((current-session (glof:get state :current-session))
             (current-tab (glof:get current-session :current-tab))
             (current-pane (glof:get current-tab :current-pane))
             (next-pane
              (mesh:find-next `[:pane ,current-pane]
                           (glof:get current-tab :panes))))
    (when (not (seq-empty-p next-pane))
      (cl-letf ((next-pane-buffer (glof:get next-pane :buffer)))
        (switch-to-buffer-other-window next-pane-buffer)
        (cl-letf* ((new-session current-session))
          (cl-letf* ((new-tab (thread-first current-tab
                                (glof:assoc :current-pane next-pane)
                                (glof:assoc :conf (current-window-configuration))))
                     (new-tabs (mesh:substitute-if-v new-tab
                                                  (lambda (tab)
                                                    (eq (glof:get tab :index)
                                                        (glof:get current-tab :index)))
                                                  (glof:get current-session :tabs))))
            (cl-letf ((new-session (thread-first current-session
                                     (glof:assoc :tabs new-tabs
                                                 :current-tab new-tab))))
              (glof:assoc state
                          :current-session new-session
                          :sessions
                          (mesh:tab--subst-session state
                                                new-session current-session)))))))))

(cl-defun mesh:pane--command-kill (state)
  (cl-letf* ((old-session (glof:get state :current-session))
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
       (cl-letf* ((new-tabs (mesh:removev (lambda (tab)
                                         (eq (glof:get tab :index)
                                             (glof:get old-tab :index)))
                                       old-tabs))
                  (new-session (thread-first old-session
                                 (glof:assoc :tabs new-tabs
                                             :current-tab next-tab))))

         
         (mesh:tab--kill-panes old-tab)
         (set-window-configuration (glof:get next-tab :conf))
         (glof:assoc state
                     :current-session new-session
                     :sessions
                     (mesh:tab--subst-session state
                                           new-session old-session))))
      (_
       (setq mesh:*state* (mesh:pane--command-next mesh:*state*))
       (setq mesh:*state* (mesh:pane--kill mesh:*state*
                                     old-session old-tab old-pane next-pane)))
      
      ;; (t
      ;;  ;; TODO kill session
      ;;  )
      )))

(cl-defun mesh:pane--kill (state session tab pane next-pane)
  (cl-letf* ((old-session session)
             (old-tab tab)
             (old-pane pane))
    (with-current-buffer (glof:get old-pane :buffer)
      (cl-letf ((window-to-kill (get-buffer-window (glof:get old-pane :buffer))))
        (kill-buffer (glof:get old-pane :buffer))
        (delete-window window-to-kill)))
    (cl-letf* ((new-tab (thread-first old-tab
                          (glof:assoc :conf (current-window-configuration))
                          (glof:assoc :current-pane next-pane)
                          (glof:assoc :panes
                                      (mesh:removev
                                       (lambda (pane)
                                         (eq (glof:get pane :index)
                                             (glof:get old-pane :index)))
                                       (glof:get old-tab :panes)))))
               (new-tabs (mesh:substitute-if-v new-tab
                                            (lambda (tab)
                                              (eq (glof:get tab :index)
                                                  (glof:get old-tab :index)))
                                            (glof:get old-session :tabs))))
      (cl-letf ((new-session (thread-first old-session
                               (glof:assoc :tabs new-tabs
                                           :current-tab new-tab))))
        (glof:assoc state
                    :current-session new-session
                    :sessions
                    (mesh:tab--subst-session state
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
