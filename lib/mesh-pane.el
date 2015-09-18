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
              :session session-name
              :tab tab-name
              :index pane-index
              :buffer buffer)
  ;; (make-instance 'mesh:<pane>
  ;;                :session session-name
  ;;                :tab tab-name
  ;;                :index pane-index
  ;;                :buffer buffer)
  )

(cl-defun mesh:pane--command-next ()
  (cl-letf* ((current-session (mesh:current-session))
             (current-tab (mesh:get-current-tab current-session))
             (current-pane (glof:get current-tab :current-pane))
             (next-pane
              (mesh:find-next `[:pane ,current-pane]
                              (glof:get current-tab :panes))))
    (when next-pane
      (cl-letf ((next-pane-buffer (glof:get next-pane :buffer)))
        (switch-to-buffer-other-window next-pane-buffer)
        (cl-letf* ((new-session current-session)
                   )
          ;; (mesh:set-slots new-tab
          ;;   'current-pane next-pane
          ;;   'conf (current-window-configuration))
          ;; (mesh:set-slots new-session
          ;;             'tabs (cl-subst new-tab current-tab
          ;;                             (mesh:get-tabs current-session)))
          (cl-letf* ((new-tab (thread-first current-tab
                                (glof:assoc :current-pane next-pane)
                                (glof:assoc :conf (current-window-configuration))))
                     (new-tabs (cl-substitute-if
                                new-tab
                                (lambda (tab)
                                  (eq (glof:get tab :index)
                                      (glof:get current-tab :index)))
                                (mesh:get-tabs current-session))))
            (setf (mesh:get-tabs new-session) new-tabs)
            (setf (mesh:get-current-tab new-session) new-tab)
            (mesh:tab--subst-session-list
             new-session current-session)))))))

(cl-defun mesh:pane--command-kill ()
  (cl-letf* ((old-session (mesh:current-session))
             (old-tab (mesh:get-current-tab old-session))
             (old-pane (glof:get old-tab :current-pane))
             (next-pane (mesh:find-next `[:pane ,old-pane]
                                        (glof:get old-tab :panes)))
             (next-tab (mesh:find-next `[:tab ,old-tab]
                                       (mesh:get-tabs old-session))))
    (cond
      (next-pane
       (mesh:pane--command-next)
       (mesh:pane--kill old-session old-tab old-pane next-pane))
      (next-tab
       (cl-letf ((current-session old-session))
         (with-slots ((current-tab current-tab)
                      (current-tabs tabs))
             current-session
           (cl-letf* ((next-tab (mesh:find-next `[:tab ,current-tab] current-tabs))
                      (new-session current-session))
             (mesh:tab--kill-panes current-tab)
             ;; (mesh:set-slots new-session
             ;;   'tabs (cl-remove current-tab current-tabs)
             ;;   'current-tab next-tab)
             (setf (mesh:get-tabs new-session)
                   (seq-remove (lambda (tab)
                                 (eq (glof:get tab :index)
                                     (glof:get current-tab :index)))
                               current-tabs))
             (setf (mesh:get-current-tab new-session)
                   next-tab)
             (mesh:set-current-session new-session)
             (mesh:tab--subst-session-list new-session current-session)
             (set-window-configuration (glof:get next-tab :conf))))))
      (t
       ;; TODO kill session
       ))))

(cl-defun mesh:pane--kill (session tab pane next-pane)
  (cl-letf* ((old-session session)
             (old-tab tab)
             (old-pane pane))
    (with-current-buffer (glof:get old-pane :buffer)
      (cl-letf ((window-to-kill (get-buffer-window (glof:get old-pane :buffer))))
        (kill-buffer (glof:get old-pane :buffer))
        (delete-window window-to-kill)))
    (cl-letf* ((new-session old-session))
      ;; (mesh:set-slots new-tab
      ;;   'conf (current-window-configuration)
      ;;   'panes (cl-remove old-pane (glof:get old-tab :panes)))
      ;; (mesh:set-slots new-session
      ;;         'tabs (cl-subst new-tab old-tab
      ;;                         (mesh:get-tabs old-session)))
      (cl-letf* ((new-tab (thread-first old-tab
                            (glof:assoc :conf (current-window-configuration))
                            (glof:assoc :current-pane next-pane)
                            (glof:assoc :panes
                                        (seq-remove
                                         (lambda (pane)
                                           (eq (glof:get pane :index)
                                               (glof:get old-pane :index)))
                                         (glof:get old-tab :panes)))))
                 (new-tabs (cl-substitute-if
                            new-tab
                            (lambda (tab)
                              (eq (glof:get tab :index)
                                  (glof:get old-tab :index)))
                            (mesh:get-tabs old-session))))
        (setf (mesh:get-tabs new-session) new-tabs)
        (setf (mesh:get-current-tab new-session) new-tab)
        (mesh:tab--subst-session-list
         new-session old-session)))))

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
   (format"*%s:%s.%s:%s*"
          session-name
          tab-name
          tab-index
          pane-index)))


(provide 'mesh-pane)

;;; mesh-pane.el ends here
