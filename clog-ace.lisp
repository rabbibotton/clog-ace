(defpackage #:clog-ace
  (:use #:cl #:clog)
  (:export clog-ace-element
	   create-clog-ace-element
	   mode text-value theme tab-size
	   clipboard-copy clipboard-paste
	   excute-command focus move-cursor resize selected-text
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-on-change ((obj clog-ace-element) handler)
  (set-on-event obj "clog-ace-change" (lambda (obj)
				       (funcall handler obj)))
  (js-execute obj (format nil "~A.on('change', function()~
                                  {~A.trigger('clog-ace-change')})"
			  (js-ace obj)
			  (jquery obj))))

(defmethod set-on-blur ((obj clog-ace-element) handler)
  (set-on-event obj "clog-ace-blur" (lambda (obj)
				      (funcall handler obj)))
  (js-execute obj (format nil "~A.on('blur', function()~
                                  {~A.trigger('clog-ace-blur')})"
			  (js-ace obj)
			  (jquery obj))))

(defmethod set-on-focus ((obj clog-ace-element) handler)
  (set-on-event obj "clog-ace-focus" (lambda (obj)
				       (funcall handler obj)))
  (js-execute obj (format nil "~A.on('focus', function()~
                                  {~A.trigger('clog-ace-focus')})"
			   (js-ace obj)
			   (jquery obj))))

(defmethod set-on-cut ((obj clog-ace-element) handler)
  (set-on-event obj "clog-ace-cut" (lambda (obj)
				       (funcall handler obj)))
  (js-execute obj (format nil "~A.on('cut', function()~
                                  {~A.trigger('clog-ace-cut')})"
			  (js-ace obj)
			  (jquery obj))))

(defmethod set-on-copy ((obj clog-ace-element) handler)
  (set-on-event obj "clog-ace-copy" (lambda (obj)
					(funcall handler obj)))
  (js-execute obj (format nil "~A.on('copy', function()~
                                  {~A.trigger('clog-ace-copy')})"
			  (js-ace obj)
			  (jquery obj))))

(defmethod set-on-paste ((obj clog-ace-element) handler)
  (set-on-event obj "clog-ace-paste" (lambda (obj)
					(funcall handler obj)))
  (js-execute obj (format nil "~A.on('paste', function()~
                                  {~A.trigger('clog-ace-paste')})"
			  (js-ace obj)
			  (jquery obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; mode ;;
;;;;;;;;;;

(defgeneric mode (clog-ace-element)
  (:documentation "The ace edit mode. eg. ace/mode/lisp"))


(defmethod mode ((obj clog-ace-element))
  (js-query obj (format nil "~A.session.getMode()" (js-ace obj))))

(defgeneric set-mode (clog-ace-element mode)
  (:documentation "The ace edit mode. eg. ace/mode/lisp"))


(defmethod set-mode ((obj clog-ace-element) mode)
  (js-execute obj (format nil "~A.session.setMode('~A')" (js-ace obj) mode)))

(defsetf mode set-mode)

;;;;;;;;;;;;;;
;; tab-size ;;
;;;;;;;;;;;;;;

(defgeneric tab-size (clog-ace-element)
  (:documentation "Tab size"))


(defmethod tab-size ((obj clog-ace-element))
  (js-query obj (format nil "~A.session.getTabSize()" (js-ace obj))))

(defgeneric set-tab-size (clog-ace-element tab-size)
  (:documentation "Tab size"))


(defmethod set-tab-size ((obj clog-ace-element) tab-size)
  (js-execute obj (format nil "~A.session.setTabSize(~A)" (js-ace obj) tab-size)))

(defsetf tab-size set-tab-size)

;;;;;;;;;;;;;;;;
;; text-value ;;
;;;;;;;;;;;;;;;;

(defmethod text-value ((obj clog-ace-element))
  (js-query obj (format nil "~A.getValue()" (js-ace obj))))

(defmethod set-text-value ((obj clog-ace-element) value)
  (js-execute obj (format nil "~A.setValue('~A')" (js-ace obj) theme)))

;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;

(defgeneric theme (clog-ace-element)
  (:documentation "The ace color theme. eg. ace/theme/xcode"))


(defmethod theme ((obj clog-ace-element) )
  (js-query obj (format nil "~A.getTheme()" (js-ace obj))))

(defgeneric set-theme (clog-ace-element theme)
  (:documentation "The ace color theme. eg. ace/theme/xcode"))


(defmethod set-theme ((obj clog-ace-element) theme)
  (js-execute obj (format nil "~A.setTheme('~A')" (js-ace obj) theme)))

(defsetf theme set-theme)

;;;;;;;;;;;;;;;;;;;
;; selected-text ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric selected-text (clog-ace-element)
  (:documentation "Get currently selected (read only)"))

(defmethod selected-text ((obj clog-ace-element))
  (js-query obj (format nil "~A.getCopyText()" (js-ace obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; clipboard-copy ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric clipboard-copy (clog-ace-element)
  (:documentation "Copy selected text to global clipboard and return text."))

(defmethod clipboard-copy ((obj clog-ace-element))
  (js-query obj (format nil "~A.execCommand('copy');~
                             navigator.clipboard.writeText(~A.getCopyText());~
                             ~A.getCopyText();"
			(js-ace obj)
			(js-ace obj)
			(js-ace obj))))

;;;;;;;;;;;;;;;;;;;;;
;; clipboard-paste ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric clipboard-paste (clog-ace-element)
  (:documentation "Paste selected text to global clipboard and return text."))

(defmethod clipboard-paste ((obj clog-ace-element))
  (js-execute obj (format nil "navigator.clipboard.readText().then(function(text) {~
                                        editor_~A.execCommand('paste', text)~
                                     }"
			  (js-ace obj))))

;;;;;;;;;;;;;;;;;;;;;
;; execute-command ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric execute-command (clog-ace-element command)
  (:documentation "execute-command COMMAND"))

(defmethod execute-command ((obj clog-ace-element) command)
  (js-execute obj (format nil "~A.execCommand('~A')" (js-ace obj) command)))

;;;;;;;;;;;
;; focus ;;
;;;;;;;;;;;

(defmethod focus ((obj clog-ace-element))
  (js-execute obj (format nil "~A.focus()" (js-ace obj))))

;;;;;;;;;;;;;;;;;
;; move-cursor ;;
;;;;;;;;;;;;;;;;;

(defgeneric move-cursor (clog-ace-element x y)
  (:documentation "move-cursor to x y"))

(defmethod move-cursor ((obj clog-ace-element) x y)
  (js-execute obj (format nil "~A.moveCursorTo(~A,~A)" (js-ace obj) x y)))

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
  "Javascript editor variable (private)"
  (check-type obj clog:clog-obj)  
  (format nil "clog['editor_~A']" (html-id obj)))
	  
(defun attach-clog-ace (obj)
  "Initialize plugin"
  (init-clog-ace obj)
  (js-execute obj (format nil "~A = ace.edit('~A')"
			  (js-ace obj) (html-id obj))))

(defun on-test-clog-ace (body)
  (clog:debug-mode body)
  ;; Use the panel-box-layout to center horizontally
  ;; and vertically our div on the screen.
  (let* ((layout (create-panel-box-layout body))
	 (test   (create-clog-ace-element (center-panel layout)))
	 (button (create-button (top-panel layout) :content "Resize"))
	 (sel    (create-button (top-panel layout) :content "Selection"))
	 (text   (create-button (top-panel layout) :content "Text")))
    (center-children (center-panel layout))
    (set-on-cut test (lambda (obj)
		       (declare (ignore obj))
		       (print "cut")))
    (set-on-copy test (lambda (obj)
			(declare (ignore obj))
		       (print "copy")))
    (set-on-paste test (lambda (obj)
			 (declare (ignore obj))
		       (print "paste")))
    (set-on-focus test (lambda (obj)
			 (declare (ignore obj))
			 (print "focus")))
    (set-on-blur test (lambda (obj)
			(declare (ignore obj))
			(print "blur")))
    (set-on-change test (lambda (obj)
			  (declare (ignore obj))
			  (print "change")))
    (set-on-click text (lambda (obj)
			 (declare (ignore obj))
			 (print (text-value test))))
    (set-on-click sel (lambda (obj)
			 (declare (ignore obj))
			 (print (selected-text test))))
    (set-on-click button (lambda (obj)
			   (declare (ignore obj))
			   (set-geometry test :height 300)
			   (clog-ace:resize test)))))
  
(defun start-test ()
  (initialize 'on-test-clog-ace
   :static-root (merge-pathnames "./www/"
		  (asdf:system-source-directory :clog-ace)))
  (open-browser))

