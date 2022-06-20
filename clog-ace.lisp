(defpackage #:clog-ace
  (:use #:cl #:clog)
  (:export clog-ace-element
	   create-clog-ace-element
	   create-clog-ace-design
	   mode theme tab-size
	   resize
           init-clog-ace
	   attach-clog-ace
           start-test))

(in-package :clog-ace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-ace-element (clog-element)()
  (:documentation " clog-ace Element Object."))

(defgeneric create-clog-ace-element (clog-obj &key hidden class html-id auto-place)
  (:documentation "Create a new clog-ace-element as child of CLOG-OBJ."))

(defmethod create-clog-ace-element ((obj clog:clog-obj)
					 &key
					   (hidden nil)
					   (class nil)
					   (html-id nil)
					       (auto-place t))
  "Not used by builder, but used to create in non-builder code"
  (let ((new-obj (create-div obj
			     :class class
			     :hidden hidden
			     :html-id html-id
			     :auto-place auto-place)))
    (set-geometry new-obj :width 400 :height 200)
    (set-border new-obj :thin :solid :black)
    (attach-clog-ace new-obj)
    (change-class new-obj 'clog-ace-element)))

(defgeneric create-clog-ace-design (clog-obj &key hidden class html-id auto-place)
  (:documentation "Create a new clog-ace-element as child of CLOG-OBJ to display
in builder representing clog-ace at design time."))

(defmethod create-clog-ace-design ((obj clog:clog-obj)
					&key
					  (hidden nil)
					  (class nil)
					  (html-id nil)
					  (auto-place t))
  (let ((new-obj (create-div obj
			     :class class
			     :hidden hidden
			     :html-id html-id
			     :auto-place auto-place)))
    (set-geometry new-obj :width 400 :height 200)
    (set-border new-obj :thin :solid :black)
    new-obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; mode ;;
;;;;;;;;;;

(defgeneric mode (clog-ace-element mode)
  (:documentation "The ace edit mode. eg. ace/mode/lisp (write-only)"))


(defmethod mode ((obj clog-ace-element) mode)
  (js-execute obj (format nil "~A.session.setMode('~A')" (js-ace obj) mode)))

;;;;;;;;;;;;;;
;; tab-size ;;
;;;;;;;;;;;;;;

(defgeneric tab-size (clog-ace-element tab-size)
  (:documentation "Tab size (write-only)"))


(defmethod tab-size ((obj clog-ace-element) tab-size)
  (js-execute obj (format nil "~A.session.setTabSize(~A)" (js-ace obj) tab-size)))

;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;

(defgeneric theme (clog-ace-element theme)
  (:documentation "The ace color theme. eg. ace/theme/xcode (write-only)"))


(defmethod theme ((obj clog-ace-element) theme)
  (js-execute obj (format nil "~A.setTheme('~A')" (js-ace obj) theme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; resize ;;
;;;;;;;;;;;;

(defgeneric resize (clog-ace-element)
  (:documentation "Trigger a resize of the underlying editor in contained in
the CLOG-ACE-ELEMENT"))

(defmethod resize ((obj clog-ace-element))
  (js-execute obj (format nil "~A.resize()" (js-ace obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - js binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-clog-ace (obj)
  (check-type obj clog:clog-obj)
  (load-script (html-document (connection-data-item obj "clog-body"))
	       "https://cdnjs.cloudflare.com/ajax/libs/ace/1.6.0/ace.js"))

(defun js-ace (obj)
  "Javascript editor variable"
  (check-type obj clog:clog-obj)  
  (format nil "clog['editor_~A']" (html-id obj)))
	  
(defun attach-clog-ace (obj)
  "Initialize plugin"
  (init-clog-ace obj)
  (js-execute obj (format nil
			  "~A = ace.edit('~A')"
			  (js-ace obj) (html-id obj))))

(defun on-test-clog-ace (body)
  (clog:debug-mode body)
  ;; Use the panel-box-layout to center horizontally
  ;; and vertically our div on the screen.
  (let* ((layout (create-panel-box-layout body))
	 (test   (create-clog-ace-element (center-panel layout)))
	 (button (create-button (top-panel layout) :content "Resize")))
    (center-children (center-panel layout))
    (set-on-click button (lambda (obj)
			   (set-geometry test :height 300)
			   (clog-ace:resize test)))))
  
(defun start-test ()
  (initialize 'on-test-clog-ace
   :static-root (merge-pathnames "./www/"
		  (asdf:system-source-directory :clog-ace)))
  (open-browser))

