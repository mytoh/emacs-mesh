;;; mesh-class -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'cl-lib)
(require 'eieio)

(defclass mesh:<session> ()
  ((name :initarg :name
         :type string
         :initform ""
         :accessor mesh:get-name)
   (current-tab :initarg :current-tab
                :type mesh:tab
                :accessor mesh:get-current-tab)
   (tabs :initarg :tabs
         :type list
         :initform nil
         :accessor mesh:get-tabs))
  "session class")


(defclass mesh:<pane> ()
  ((session :initarg :session
            :type string
            :initform ""
            :accessor mesh:get-session)
   (tab :initarg :tab
        :type string
        :initform ""
        :accessor mesh:get-tab)
   (index :initarg :index
          :type number
          :initform 0
          :accessor mesh:get-index)
   (buffer :initarg :buffer
           :type buffer
           :accessor mesh:get-buffer))
  "pane class")


(defclass mesh:<tab> ()
  ((name :initarg :name
         :type string
         :initform ""
         :accessor mesh:get-name)
   (session :initarg :session
            :type string
            :initform ""
            :accessor mesh:get-session)
   (index :initarg :index
          :type number
          :initform 0
          :accessor mesh:get-index)
   (conf :initarg :conf
         :accessor mesh:get-conf)
   (current-pane :initarg :current-pane
                 :accessor mesh:get-current-pane)
   (panes :initarg :panes
          :type list
          :initform nil
          :accessor mesh:get-panes))
  "tab class")


(provide 'mesh-class)

;;; mesh-class.el ends here
