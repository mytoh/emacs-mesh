;;; mesh-session -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'glof)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-class "lib/mesh-class")
(require 'mesh-tab "lib/mesh-tab")


(cl-defun mesh:session--new (sname sessions)
  (let ((found (seq-find (lambda (s) (cl-equalp (glof:get s :name)
                                                sname))
                         sessions)))
    (if (not (seq-empty-p found))
        found
      (cl-letf ((newtab (mesh:tab--new
                         mesh:default-tab-name
                         sname)))
        (glof:plist
         :name sname
         :current-tab newtab
         :tabs (vector newtab))))))

(cl-defun mesh:session--command-create (state newsessionname)
  (cl-letf ((newsessionname
             (if (not (seq-empty-p
                       (seq-find
                        (lambda (session) (cl-equalp newsessionname
                                                     (glof:get session :name)))
                        (glof:get state :sessions))))
                 (seq-concatenate 'string newsessionname "*")
               newsessionname)))
    (cl-letf* ((cursession (glof:get state :current-session))
               (cursessiontabs (glof:get cursession :tabs))
               (curtab (glof:get cursession :current-tab)))
      (cl-letf* ((newtab (thread-first curtab
                           (glof:assoc :conf (current-window-configuration))))
                 (newtabs (mesh:substitute-if-v newtab
                                              (lambda (tab)
                                                (eq (glof:get curtab :index)
                                                    (glof:get tab :index)))
                                              cursessiontabs)))
        (cl-letf* ((newstate (glof:assoc state
                                         :sessions
                                         (mesh:tab--subst-session state
                                                                (thread-first cursession
                                                                  (glof:assoc :current-tab newtab
                                                                              :tabs newtabs))
                                                                cursession)))
                   (newsession (mesh:session--new
                                newsessionname
                                (glof:get newstate :sessions))))
          (switch-to-buffer
           (glof:-> newsession
             :tabs mesh:first :panes mesh:first :buffer))
          (delete-other-windows)
          (glof:assoc newstate
                      :current-session newsession
                      :sessions (mesh:conj newsession
                                         (glof:get newstate :sessions))))))))

(cl-defun mesh:session--command-next (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (nextsession
              (mesh:find-next `[:session ,cursession ]
                            (glof:get state :sessions))))
    (pcase nextsession
      (`nil state)
      (_
       (cl-letf* ((cursessiontabs (glof:get cursession :tabs))
                  (cursessiontab (glof:get cursession :current-tab)))
         (cl-letf ((new-session current-session))
           (cl-letf* ((newtab (thread-first cursessiontab
                                (glof:assoc :conf (current-window-configuration))))
                      (newtabs (mesh:substitute-if-v newtab
                                                   (lambda (tab)
                                                     (eq (glof:get tab :index)
                                                         (glof:get cursessiontab
                                                                   :index)))
                                                   cursessiontabs)))
             (cl-letf* ((newstate (glof:assoc state
                                              :sessions
                                              (mesh:tab--subst-session
                                               state
                                               (thread-first cursession
                                                 (glof:assoc :current-tab newtab
                                                             :tabs newtabs))
                                               cursession)))
                        (nextsessionconf
                         (glof:get-in nextsession [:current-tab :conf])))
               (set-window-configuration nextsessionconf)
               (glof:assoc newstate
                           :current-session nextsession)))))))))

(cl-defun mesh:session--command-prev (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (prevsession
              (mesh:find-prev `[:session ,cursession]
                            (glof:get state :sessions))))
    (pcase prevsession
      (`nil state)
      (_
       (cl-letf* ((cursessiontabs (glof:get cursession :tabs))
                  (cursessiontab (glof:get cursession :current-tab)))
         (cl-letf* ((newtab (thread-first cursessiontab
                              (glof:assoc :conf (current-window-configuration))))
                    (newtabs (mesh:substitute-if-v newtab
                                                 (lambda (tab)
                                                   (eq (glof:get tab :index)
                                                       (glof:get cursessiontab :index)))
                                                 cursessiontabs)))
           (cl-letf* ((newstate (glof:assoc state
                                            :sessions
                                            (mesh:tab--subst-session state
                                                                   (thread-first cursession
                                                                     (glof:assoc :current-tab newtab
                                                                                 :tabs newtabs))
                                                                   cursession)))
                      (prevsessionconf
                       (glof:get-in prevsession [:current-tab :conf])))
             (set-window-configuration prevsessionconf)
             (glof:assoc newstate
                         :current-session prevsession))))))))

(cl-defun mesh:session--command-kill (state)
  (cl-letf* ((cursession (glof:get state :current-session))
             (cursessions (glof:get state :sessions)))
    (pcase (seq-length cursessions)
      (1
       (cl-letf ((curtab (glof:get cursession :current-tab))
                 (curtabs (glof:get cursession :tabs)))
         (jump-to-register mesh:*window-configuration-name*)
         (seq-each
          #'mesh:tab--kill-panes
          curtabs)
         (glof:assoc state
                     :session []
                     :inside-session-p nil
                     :current-session nil)))
      (_
       (cl-letf ((curtab (glof:get cursession :current-tab))
                 (curtabs (glof:get cursession :tabs))
                 (nextsession (mesh:find-next `[:session ,cursession] cursessions)))
         (set-window-configuration
          (glof:get-in nextsession [:current-tab :conf]))
         (seq-each
          #'mesh:tab--kill-panes
          curtabs)
         (glof:assoc state
                     :sessions (mesh:removev cursession cursessions)
                     :current-session nextsession))))))


(provide 'mesh-session)

;;; mesh-session.el ends here
