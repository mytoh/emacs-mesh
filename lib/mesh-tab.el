;;; mesh-tab -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'glof)

(require 'mesh-class "lib/mesh-class")
(require 'mesh-core "lib/mesh-core")
(require 'mesh-pane "lib/mesh-pane")

(cl-defun mesh:tab--new (tname sname &optional (index 0))
  (cl-letf* ((newpane (mesh:pane--new sname
                                    tname
                                    index)))
    (glof:assoc mesh:<tab>
                :name tname
                :conf nil
                :index index
                :current-pane newpane
                :session-name sname
                :panes `[,newpane])))

(cl-defun mesh:tab--command-new (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (cursessionname (glof:get cursession :name)))
    (cl-letf* ((newtabname mesh:default-tab-name)
               (lasttabindex
                (mesh:find-last (glof:get cursession :tabs)))
               (missingtabincides
                (mesh:find-missing-index (lambda (tab) (glof:get tab :index))
                                       (glof:get cursession :tabs)))
               (newtabindex
                (if missingtabincides
                    (mesh:first missingtabincides)
                  (1+  lasttabindex))))
      (cl-letf* ((newtab (mesh:tab--new newtabname cursessionname
                                      newtabindex))
                 (curtab (glof:get cursession :current-tab))
                 (curtabs (glof:get cursession :tabs)))
        (cl-letf* ((newcurtab (glof:assoc  curtab
                                           :conf (current-window-configuration)))
                   (newcurtabs (mesh:substitute-if-v newcurtab
                                                   (lambda (tab)
                                                     (eq (glof:get tab :index)
                                                         (glof:get curtab :index)))
                                                   curtabs))
                   (newcursession (thread-first cursession
                                    (glof:assoc :current-tab newcurtab
                                                :tabs newcurtabs))))
          (delete-other-windows)
          (switch-to-buffer
           (glof:-> newtab
             :panes
             mesh:first
             :buffer))
          (cl-letf* ((newtab (thread-first newtab
                               (glof:assoc :conf
                                           (current-window-configuration))))
                     (newsession
                      (thread-first newcursession
                        (glof:assoc :tabs (mesh:conj newtab
                                                   (glof:get newcursession :tabs))
                                    :current-tab newtab))))
            (glof:assoc state
                        :current-session newsession
                        :sessions
                        (mesh:tab--subst-session state
                                               newsession cursession))))))))

(cl-defun mesh:tab--command-split (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (curtab (glof:get cursession
                               :current-tab))
             (pane (glof:get curtab :current-pane))
             (panelastindex
              (seq-max (seq-map
                        (lambda (p) (glof:get p :index))
                        (glof:get curtab :panes))))
             (panemissingindices
              (mesh:find-missing-index (lambda (p) (glof:get p :index))
                                     (glof:get curtab :panes))))
    (cl-letf* ((newpane (mesh:pane--create
                         state
                         (if panemissingindices
                             (mesh:first panemissingindices)
                           (1+  panelastindex)))))
      (cl-letf ((newwindow (split-window nil nil 'below)))
        (set-window-buffer newwindow (glof:get newpane :buffer))
        (select-window newwindow))
      (cl-letf* ((newtab (thread-first curtab
                           (glof:assoc
                            :current-pane newpane
                            :panes (mesh:conj
                                    newpane
                                    (glof:get curtab :panes))
                            :conf (current-window-configuration))))
                 (newtabs (mesh:substitute-if-v
                           newtab
                           (lambda (tab)
                             (eq (glof:get tab :index)
                                 (glof:get curtab :index)))
                           (glof:get cursession :tabs))))
        (cl-letf ((newsession
                   (thread-first cursession
                     (glof:assoc :tabs newtabs
                                 :current-tab newtab))))
          (glof:assoc state
                      :current-session newsession
                      :sessions
                      (mesh:tab--subst-session state
                                             newsession cursession)))))))

(cl-defun mesh:tab--command-vsplit (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (curtab (glof:get cursession :current-tab))
             (pane (glof:get curtab :current-pane))
             (panelastindex
              (seq-max (seq-map
                        (lambda (p) (glof:get p :index))
                        (glof:get curtab :panes))))
             (panemissingindices
              (mesh:find-missing-index (lambda (p) (glof:get p :index))
                                     (glof:get curtab :panes))))
    (cl-letf* ((newpane (mesh:pane--create
                         state
                         (if panemissingindices
                             (mesh:first panemissingindices)
                           (1+  panelastindex)))))
      (cl-letf ((newwindow (split-window nil nil 'right)))
        (set-window-buffer newwindow (glof:get newpane :buffer))
        (select-window newwindow))
      (cl-letf* ((newtab (thread-first curtab
                           (glof:assoc :current-pane newpane
                                       :panes
                                       (mesh:conj newpane
                                                (glof:get curtab :panes))
                                       :conf
                                       (current-window-configuration))))
                 (newtabs (mesh:substitute-if-v newtab
                                              (lambda (tab)
                                                (eq (glof:get tab :index)
                                                    (glof:get curtab :index)))
                                              (glof:get cursession :tabs)))
                 (newsession (thread-first cursession
                               (glof:assoc :tabs newtabs
                                           :current-tab newtab))))
        (glof:assoc state
                    :current-session newsession
                    :sessions
                    (mesh:tab--subst-session state
                                           newsession cursession))))))

(cl-defun mesh:tab--subst-session (state newsession oldsession)
  (mesh:substitute-if-v
   newsession
   (lambda (session)
     (cl-equalp (glof:get oldsession :name)
                (glof:get session :name)))
   (glof:get state :sessions)))

(cl-defun mesh:tab--command-next (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (curtab (glof:get cursession :current-tab))
             (curtabs (glof:get cursession :tabs))
             (nexttab (mesh:find-next `[:tab ,curtab] curtabs)))
    (when nexttab
      (cl-letf* ((newcursession
                  (thread-first cursession
                    mesh:tab--update-tabs
                    (mesh:tab--update-current-tab nexttab))))
        (set-window-configuration (glof:get nexttab :conf))
        (glof:assoc state
                    :current-session newcursession
                    :sessions
                    (mesh:tab--subst-session state
                                           newcursession cursession))))))

(cl-defun mesh:tab--command-prev (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (curtab (glof:get cursession :current-tab))
             (curtabs (glof:get cursession :tabs))
             (prevtab (mesh:find-prev `[:tab ,curtab] curtabs)))
    (when prevtab
      (cl-letf* ((newcursession
                  (thread-first cursession
                    mesh:tab--update-tabs
                    (mesh:tab--update-current-tab prevtab))))
        (set-window-configuration (glof:get prevtab :conf))
        (glof:assoc state
                    :current-session new-cursession
                    :sessions
                    (mesh:tab--subst-session state
                                           newcursession cursession))))))

(cl-defun mesh:tab--update-current-tab (cursession newtab)
  (glof:assoc cursession :current-tab newtab))

(cl-defun mesh:tab--update-tabs (session)
  (cl-letf ((curtab (glof:get session :current-tab)))
    (glof:update session
                 :tabs
                 (lambda (tabs)
                   (thread-first curtab
                     (glof:assoc :conf (current-window-configuration))
                     (mesh:substitute-if-v (lambda (tab)
                                           (eq (glof:get tab :index)
                                               (glof:get curtab :index)))
                                         tabs))))))

(cl-defun mesh:tab--command-kill (state)
  (cl-letf ((cursession (glof:get state :current-session)))
    (cl-letf ((curtab (glof:get cursession :current-tab))
              (curtabs (glof:get cursession :tabs)))
      (pcase (seq-length curtabs)
        (1 (mesh:session--command-kill cursession))
        (_ (cl-letf* ((nexttab (mesh:find-next `[:tab ,curtab] curtabs))
                      (newcursession cursession))
             (mesh:tab--kill-panes curtab)
             (cl-letf* ((newcurtabs (mesh:removev
                                     (lambda (tab)
                                       (eq (glof:get tab :index)
                                           (glof:get curtab :index)))
                                     curtabs))
                        (newcursession (thread-first cursession
                                         (glof:assoc :tabs newcurtabs
                                                     :current-tab nexttab))))
               (set-window-configuration (glof:get nexttab :conf))
               (glof:assoc state
                           :current-session newcursession
                           :sessions (mesh:tab--subst-session state
                                                            newcursession cursession)))))))))

(cl-defun mesh:tab--kill-panes (tb)
  (seq-each
   #'kill-buffer
   (seq-map
    (lambda (p) (glof:get p :buffer))
    (glof:get tb :panes))))

(provide 'mesh-tab)

;;; mesh-tab.el ends here
