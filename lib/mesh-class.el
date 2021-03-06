;;; mesh-class -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'glof)

(defconst mesh:<session>
  (glof:plist
   :name ""
   :current-tab nil
   :tabs []))

(defconst mesh:<pane>
  (glof:plist
   :session-name ""
   :tab-name ""
   :index 0
   :buffer nil))

(defconst mesh:<tab>
  (glof:plist
   :name ""
   :session-name ""
   :index 0
   :conf nil
   :current-pane nil
   :panes []))


(provide 'mesh-class)

;;; mesh-class.el ends here
