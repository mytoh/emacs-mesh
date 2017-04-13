;;; mesh-pane -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'glof)
(require 'mesh-class "lib/mesh-class")
(require 'mesh-core "lib/mesh-core")
(require 'mesh-mode-line "lib/mesh-mode-line")
(require 'mesh-header-line "lib/mesh-header-line")

(cl-defun mesh:pane--new (sname tname tindex)
  (cl-letf* ((buffer (mesh:pane--get-buffer-create
                      sname tname tindex 0)))
    (mesh:pane--make-buffer-eshell-mode buffer)
    (mesh:pane--set-mode-line buffer)
    (mesh:pane--set-header-line buffer)
    (mesh:pane--make-pane
     sname tname 0 buffer)))

(cl-defun mesh:pane--create (state &optional (pindex 0))
  (cl-letf* ((cursession (glof:get state :current-session))
             (sname (glof:get cursession :name))
             (curtab (glof:get cursession :current-tab))
             (tname (glof:get curtab :name))
             (tindex (glof:get curtab :index))
             (buffer (mesh:pane--get-buffer-create
                      sname tname tindex pindex)))
    (mesh:pane--make-buffer-eshell-mode buffer)
    (mesh:pane--set-mode-line buffer)
    (mesh:pane--set-header-line buffer)
    (mesh:pane--make-pane
     sname tname pindex buffer)))

(cl-defun mesh:pane--make-pane
    (sname tname pindex buffer)
  (glof:assoc mesh:<pane>
              :sname sname
              :tname tname
              :index pindex
              :buffer buffer))

(cl-defun mesh:pane--command-next (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (curtab (glof:get cursession :current-tab))
             (curpane (glof:get curtab :current-pane))
             (nextpane
              (mesh:find-next `[:pane ,curpane]
                            (glof:get curtab :panes))))
    (when (not (colle:empty-p nextpane))
      (cl-letf ((nextpanebuf (glof:get nextpane :buffer)))
        (switch-to-buffer-other-window nextpanebuf)
        (cl-letf* ((newsession cursession))
          (cl-letf* ((newtab (thread-first curtab 
                               (glof:assoc :current-pane nextpane
                                           :conf (current-window-configuration))))
                     (newtabs (mesh:substitute-if-v newtab
                                                  (lambda (tab)
                                                    (eq (glof:get tab :index)
                                                        (glof:get curtab :index)))
                                                  (glof:get cursession :tabs))))
            (cl-letf ((newsession (thread-first cursession
                                    (glof:assoc :tabs newtabs
                                                :current-tab newtab))))
              (glof:assoc state
                          :current-session newsession
                          :sessions
                (mesh:tab--subst-session state
                                       newsession cursession)))))))))

(cl-defun mesh:pane--command-kill (state)
  (cl-letf* ((oldsession (glof:get state :current-session))
             (oldtab (glof:get oldsession :current-tab))
             (oldtabs (glof:get oldsession :tabs))
             (oldpane (glof:get oldtab :current-pane))
             (oldpanes (glof:get oldtab :panes))
             (nextpane (mesh:find-next `[:pane ,oldpane]
                                     oldpanes))
             (nexttab (mesh:find-next `[:tab ,oldtab]
                                    (glof:get oldsession :tabs))))
    (pcase (seq-length oldpanes)
      (1
       (cl-letf* ((newtabs (mesh:removev (lambda (tb)
                                         (eq (glof:get tb :index)
                                             (glof:get oldtab :index)))
                                       oldtabs))
                  (newsession (thread-first oldsession
                                (glof:assoc :tabs newtabs
                                            :current-tab nexttab))))
         (mesh:tab--kill-panes oldtab)
         (set-window-configuration (glof:get nexttab :conf))
         (glof:assoc state
                     :current-session newsession
                     :sessions
           (mesh:tab--subst-session state
                                  newsession oldsession))))
      (_
       (setq mesh:*state* (mesh:pane--command-next mesh:*state*))
       (setq mesh:*state* (mesh:pane--kill mesh:*state*
                                       oldsession oldtab oldpane nextpane)))
      
      ;; (t
      ;;  ;; TODO kill session
      ;;  )
      )))

(cl-defun mesh:pane--kill-buffer (pane)
  (cl-letf* ((buffer (glof:get pane :buffer))
             (windowtokill (get-buffer-window buffer)))
    (with-current-buffer buffer
      (kill-buffer buffer)
      (delete-window windowtokill))))

(cl-defun mesh:pane--kill (state session tab pane nextpane)
  (cl-letf* ((oldsession session)
             (oldtab tab)
             (oldpane pane))
    (mesh:pane--kill-buffer oldpane)
    (cl-letf* ((newtab (thread-first oldtab
                         (glof:assoc
                          :conf (current-window-configuration)
                          :current-pane nextpane
                          :panes
                           (mesh:removev
                            (lambda (pane)
                              (eq (glof:get pane :index)
                                  (glof:get oldpane :index)))
                            (glof:get oldtab :panes)))))
               (newtabs (mesh:substitute-if-v newtab
                                            (lambda (tab)
                                              (eq (glof:get tab :index)
                                                  (glof:get oldtab :index)))
                                            (glof:get oldsession :tabs))))
      (cl-letf ((newsession (thread-first oldsession
                              (glof:assoc :tabs newtabs
                                          :current-tab newtab))))
        (glof:assoc state
                    :current-session newsession
                    :sessions
          (mesh:tab--subst-session state
                                 newsession oldsession))))))

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
    (sname tname tindex pindex)
  (get-buffer-create
   (format "*%s:%s.%s:%s*"
           sname
           tname
           tindex
           pindex)))


(provide 'mesh-pane)

;;; mesh-pane.el ends here
