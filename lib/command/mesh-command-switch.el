;;; mesh-command-switch -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'glof)
(require 'glof-thread)

(require 'mesh-core "lib/mesh-core")
(require 'mesh-session "lib/mesh-session")
(require 'mesh-tab "lib/mesh-tab")
(require 'mesh-pane "lib/mesh-pane")

;;;###autoload
(cl-defun mesh:switch ()
  (interactive)
  (cl-letf ((state (if mesh:*state* mesh:*state*
                     (mesh:initial-state))))
    (if (glof:get state :inside-session-p)
        (setq mesh:*state* (mesh:command--switch-inside state))
      (setq mesh:*state* (mesh:command--switch-outside state)))))

(cl-defun mesh:command--switch-inside (state)
  (cl-letf* ((oldsession (glof:get state :current-session))
             (tabs (glof:get oldsession :tabs))
             (oldtab (glof:get oldsession :current-tab)))
    (cl-letf* ((newsession oldsession))
      (cl-letf* ((newtab (thread-first oldtab
                           (glof:assoc :conf (current-window-configuration))))
                 (newtabs (mesh:substitute-if-v newtab
                                              (lambda (tab) (eq (glof:get tab :index)
                                                           (glof:get oldtab :index)))
                                              tabs)))
        (jump-to-register mesh:*window-configuration-name*)
        (glof:assoc state
                    :sessions
                    (mesh:tab--subst-session
                     state
                     (thread-first newsession
                       (glof:assoc :current-tab newtab
                                   :tabs newtabs))
                     oldsession)
                    :inside-session-p nil)))))

(cl-defun mesh:command--switch-outside (state)
  (window-configuration-to-register mesh:*window-configuration-name*)
  (if (not (seq-empty-p (glof:get state :sessions)))
      (cl-letf* ((session (glof:get state :current-session))
                 (conf (glof:get-in session
                                    [:current-tab :conf])))
        (set-window-configuration conf)
        (glof:assoc state
                    :inside-session-p t))
    (cl-letf* ((newsession
                (glof:->> state
                  :sessions
                  (mesh:session--new mesh:default-session-name)))
               (newtab (mesh:first (glof:get newsession :tabs)))
               (conf (glof:get newtab :conf)))
      (cond
        (conf
         (set-window-configuration conf)
         (glof:assoc state
                     :current-session newsession
                     :sessions (mesh:conj newsession
                                        (glof:get state :sessions))
                     :inside-session-p t))
        (t
         (switch-to-buffer
          (glof:-> newtab 
            :panes
            mesh:first
            :buffer))
         (delete-other-windows)
         (cl-letf* ((newtab (thread-first newtab 
                              (glof:assoc :conf (current-window-configuration))))
                    (newtabs (mesh:substitute-if-v newtab
                                                 (lambda (tb) (eq (glof:get tb :index)
                                                             (glof:get newtab :index)))
                                                 (glof:get newsession :tabs)))
                    (newsession (thread-first newsession
                                  (glof:assoc
                                   :current-tab newtab
                                   :tabs newtabs))))
           (glof:assoc state
                       :current-session newsession
                       :sessions (mesh:conj newsession
                                          (glof:get state :sessions))
                       :inside-session-p t)))))))

(provide 'mesh-command-switch)

;;; mesh-command-switch.el ends here
