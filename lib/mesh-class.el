;;; mesh-class -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(defvar mesh:<session>-template
  (glof:plist
   :name ""
   :current-tab nil
   :tabs nil))

(defvar mesh:<pane>-template
  (glof:plist
   :session-name ""
   :tab-name ""
   :index 0
   :buffer nil))

(defvar mesh:<tab>-template
  (glof:plist
   :name ""
   :session-name ""
   :index 0
   :conf nil
   :current-pane nil
   :panes nil))


(provide 'mesh-class)

;;; mesh-class.el ends here
