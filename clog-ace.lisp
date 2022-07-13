(defpackage #:clog-ace
  (:use #:cl #:clog)
  (:export clog-ace-element
	   create-clog-ace-element
	   mode text-value theme tab-size
	   clipboard-copy clipboard-paste set-auto-completion
	   execute-command focus move-cursor resize selected-text
           init-clog-ace set-on-auto-complete
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
;; Properties - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; mode ;;
;;;;;;;;;;

(defgeneric mode (clog-ace-element)
  (:documentation "The ace edit mode. eg. ace/mode/lisp"))

(defmethod mode ((obj clog-ace-element))
  (js-query obj (format nil "~A.session.getMode()" (js-ace obj))))

(defgeneric (setf mode) (mode clog-ace-element)
  (:documentation "The ace edit mode. eg. ace/mode/lisp"))

(defmethod (setf mode) (mode (obj clog-ace-element))
  (js-execute obj (format nil "~A.session.setMode('~A')" (js-ace obj) mode)))

;;;;;;;;;;;;;;
;; tab-size ;;
;;;;;;;;;;;;;;

(defgeneric tab-size (clog-ace-element)
  (:documentation "Tab size"))

(defmethod tab-size ((obj clog-ace-element))
  (js-query obj (format nil "~A.session.getTabSize()" (js-ace obj))))

(defgeneric (setf tab-size) (tab-size clog-ace-element)
  (:documentation "Tab size"))

(defmethod (setf tab-size) (tab-size (obj clog-ace-element))
  (js-execute obj (format nil "~A.session.setTabSize(~A)" (js-ace obj) tab-size)))

;;;;;;;;;;;;;;;;
;; text-value ;;
;;;;;;;;;;;;;;;;

(defmethod text-value ((obj clog-ace-element))
  (js-query obj (format nil "~A.getValue()" (js-ace obj))))

(defmethod (setf text-value) (value (obj clog-ace-element))
  (js-execute obj (format nil "~A.setValue('~A')" (js-ace obj) (escape-string value))))

;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;

(defgeneric theme (clog-ace-element)
  (:documentation "The ace color theme. eg. ace/theme/xcode"))

(defmethod theme ((obj clog-ace-element) )
  (js-query obj (format nil "~A.getTheme()" (js-ace obj))))

(defgeneric (setf theme) (theme clog-ace-element)
  (:documentation "The ace color theme. eg. ace/theme/xcode"))

(defmethod (setf theme) (theme (obj clog-ace-element))
  (js-execute obj (format nil "~A.setTheme('~A')" (js-ace obj) theme)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-auto-completion ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-auto-completion (clog-ace-element auto-completion)
  (:documentation "Turn auto complete on or off"))

(defmethod set-auto-completion ((obj clog-ace-element) auto-completion)
  (js-execute obj (format nil "~A.setOption('enableBasicAutocompletion', ~A)"
			  (js-ace obj)
			  (if auto-completion
			      "true"
			      "false")))
  (js-execute obj (format nil "~A.setOption('enableLiveAutocompletion', ~A)"
			  (js-ace obj)
			  (if auto-completion
			      "true"
			      "false"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-on-auto-complete ((obj clog-ace-element) handler &key (meta "clog"))
  (set-on-event-with-data obj "clog-ace-auto-complete"
			  (lambda (obj data)
			    (js-execute obj (format nil "clog['~A-ace'](null,[~{~A~}])"
						    (html-id obj)
						    (mapcar (lambda (s)
							      (format nil "{'caption':'~A','value':'~A','score':1,'meta':'~A'},"
								      s s meta))
							    (funcall handler obj data))))))
  (when handler
    (js-execute obj
		(format nil "var comps={getCompletions: function(editor, session, pos, prefix, callback) {~
                               clog['~A-ace']=callback;
                               ~A.trigger('clog-ace-auto-complete', prefix);}};~
                               var lt=ace.require('ace/ext/language_tools');lt.addCompleter(comps);"
			(html-id obj)
			(jquery obj)))))
      

(defun set-ace-event (obj event handler)
  (set-on-event obj (format nil "clog-ace-~A" event) handler)
  (if handler
      (js-execute obj (format nil "~A.on('~A', function()~
                                  {~A.trigger('clog-ace-~A')})"
                              (js-ace obj) event
                              (jquery obj) event))
      (js-execute obj (format nil "~A.off('~A')" (js-ace obj) event))))

(defmethod set-on-change ((obj clog-ace-element) handler)
  (set-ace-event obj "change" handler))

(defmethod set-on-blur ((obj clog-ace-element) handler)
  (set-ace-event obj "blur" handler))

(defmethod set-on-focus ((obj clog-ace-element) handler)
  (set-ace-event obj "focus" handler))

(defmethod set-on-cut ((obj clog-ace-element) handler)
  (set-ace-event obj "cut" handler))

(defmethod set-on-copy ((obj clog-ace-element) handler)
  (set-ace-event obj "copy" handler))

(defmethod set-on-paste ((obj clog-ace-element) handler)
  (set-ace-event obj "paste" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - js binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-clog-ace (obj)
  (check-type obj clog:clog-obj)
  (load-script (html-document (connection-data-item obj "clog-body"))
               "https://cdnjs.cloudflare.com/ajax/libs/ace/1.6.0/ace.js")
  (load-script (html-document (connection-data-item obj "clog-body"))
               "https://cdnjs.cloudflare.com/ajax/libs/ace/1.6.0/ext-language_tools.js"))

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
         (stext  (create-button (top-panel layout) :content "Set Text"))
         (text   (create-button (top-panel layout) :content "Text")))
    (center-children (center-panel layout))
    (set-auto-completion test t)
    (set-on-auto-complete test (lambda (obj data)
				 (list "test" "wall" "super")))
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
    (set-on-click stext (lambda (obj)
			 (declare (ignore obj))
			 (setf (text-value test) "test text")))
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
