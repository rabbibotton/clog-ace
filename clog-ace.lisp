(defpackage #:clog-ace
  (:use #:cl #:clog)
  (:export clog-ace-element
           create-clog-ace-element
           mode text-value theme tab-size read-only-p
           clipboard-copy clipboard-cut clipboard-paste
           set-option set-auto-completion get-mode-from-extension
           execute-command focus get-cursor move-cursor resize
           font-size selected-text
           init-clog-ace set-on-auto-complete
           attach-clog-ace
           *completer-installed*
           start-test))

(in-package :clog-ace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *completer-installed* nil)

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
  "Create ace editor"
  (let ((new-obj (create-div obj
                             :class class
                             :hidden hidden
                             :html-id html-id
                             :auto-place auto-place)))
    (set-geometry new-obj :width 400 :height 200)
    (set-border new-obj :thin :solid :black)
    (change-class new-obj 'clog-ace-element)
    (attach-clog-ace new-obj)))

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

;;;;;;;;;;;;;;;;;
;; read-only-p ;;
;;;;;;;;;;;;;;;;;

(defgeneric read-only-p (clog-ace-element)
  (:documentation "Get/Setf ace edit read-only status"))

(defmethod read-only-p ((obj clog-ace-element))
  (when (equal "true" (js-query obj (format nil "~A.getReadOnly()" (js-ace obj))))
    t))

(defgeneric (setf read-only-p) (read-only clog-ace-element)
  (:documentation "Set the ace edit read-only status"))

(defmethod (setf read-only-p) (read-only (obj clog-ace-element))
  (js-execute obj (format nil "~A.setReadOnly(~A)" (js-ace obj) (if read-only
                                                                    "true"
                                                                    "false"))))

;;;;;;;;;;;;;;;;
;; text-value ;;
;;;;;;;;;;;;;;;;

(defmethod text-value ((obj clog-ace-element))
  (js-query obj (format nil "~A.getValue()" (js-ace obj))))

(defmethod (setf text-value) (value (obj clog-ace-element))
  (js-execute obj (format nil "~A.setValue('~A', -1)" (js-ace obj) (escape-string value))))

;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;

(defgeneric theme (clog-ace-element)
  (:documentation "The ace color theme. eg. ace/theme/xcode"))

(defmethod theme ((obj clog-ace-element))
  (js-query obj (format nil "~A.getTheme()" (js-ace obj))))

(defgeneric (setf theme) (theme clog-ace-element)
  (:documentation "The ace color theme. eg. ace/theme/xcode"))

(defmethod (setf theme) (theme (obj clog-ace-element))
  (js-execute obj (format nil "~A.setTheme('~A')" (js-ace obj) theme)))

;;;;;;;;;;;;;;;
;; font-size ;;
;;;;;;;;;;;;;;;

(defgeneric font-size (clog-ace-element)
  (:documentation "Font size"))

(defmethod font-size ((obj clog-ace-element))
  (js-to-integer (js-query obj (format nil "~A.getFontSize()" (js-ace obj)))))

(defgeneric (setf font-size) (font-size clog-ace-element)
  (:documentation "Font size"))

(defmethod (setf font-size) (font-size (obj clog-ace-element))
  (js-execute obj (format nil "~A.setFontSize(~A)" (js-ace obj) font-size)))

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

;;;;;;;;;;;;;;;;;;;
;; clipboard-cut ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric clipboard-cut (clog-ace-element)
  (:documentation "Copy selected text to global clipboard and return text."))

(defmethod clipboard-cut ((obj clog-ace-element))
    (js-execute obj (format nil "navigator.clipboard.writeText(~A.getCopyText());~
                                  ~A.execCommand('cut')"
                            (js-ace obj)
                            (js-ace obj))))

;;;;;;;;;;;;;;;;;;;;;
;; clipboard-paste ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric clipboard-paste (clog-ace-element)
  (:documentation "Paste selected text to global clipboard and return text."))

(defmethod clipboard-paste ((obj clog-ace-element))
  (js-execute obj (format nil "navigator.clipboard.readText().then(function(text) {~
                                        ~A.execCommand('paste', text)~
                                     })"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-mode-from-extension ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-mode-from-extension (clog-ace-element file-name)
  (:documentation "Returns the mode to use based on the FILE-NAME"))

(defmethod get-mode-from-extension ((obj clog-ace-element) file-name)
  (js-query obj (format nil "var modelist = ace.require('ace/ext/modelist'); ~
                             modelist.getModeForPath('~A').mode;" file-name)))

;;;;;;;;;;;;;;;;;
;; get-cursor ;;
;;;;;;;;;;;;;;;;;

(defgeneric get-cursor (clog-ace-element)
  (:documentation "get cursor x y position"))

(defmethod get-cursor ((obj clog-ace-element))
  (let ((row (js-query obj (format nil "~A.selection.getCursor().row" (js-ace obj))))
        (column (js-query obj (format nil "~A.selection.getCursor().column" (js-ace obj)))))
    (values (js-to-integer row) (js-to-integer column))))

;;;;;;;;;;;;;;;;;
;; move-cursor ;;
;;;;;;;;;;;;;;;;;

(defgeneric move-cursor (clog-ace-element row column)
  (:documentation "move-cursor to row column"))

(defmethod move-cursor ((obj clog-ace-element) row column)
  (js-execute obj (format nil "~A.selection.clearSelection()" (js-ace obj)))
  (js-execute obj (format nil "~A.selection.moveCursorTo(~A,~A)" (js-ace obj) row column)))

;;;;;;;;;;;;
;; resize ;;
;;;;;;;;;;;;

(defgeneric resize (clog-ace-element)
  (:documentation "Trigger a resize of the underlying editor in contained in
the CLOG-ACE-ELEMENT"))

(defmethod resize ((obj clog-ace-element))
  (js-execute obj (format nil "~A.resize()" (js-ace obj))))

;;;;;;;;;;;;;;;;
;; set-option ;;
;;;;;;;;;;;;;;;;

(defgeneric set-option (clog-ace-element name value)
  (:documentation "Set option NAME to VALUE"))

(defmethod set-option ((obj clog-ace-element) name value)
  (js-execute obj (format nil "~A.setOption('~A', ~A)"
                          (js-ace obj) name value)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-auto-completion ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-auto-completion (clog-ace-element auto-completion)
  (:documentation "Turn auto complete on or off"))

(defmethod set-auto-completion ((obj clog-ace-element) auto-completion)
  (let ((js-boolean (if auto-completion
                        "true"
                        "false")))
    (set-option obj "enableBasicAutocompletion" js-boolean)
    (set-option obj "enableLiveAutocompletion" js-boolean)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events - clog-ace-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-on-auto-complete (obj handler
                             &key (default-score 100) (meta "clog"))
  "There can only be one auto complete handler per application currently."
  (set-on-event-with-data
   obj "clog-ace-auto-complete"
   (lambda (obj data)
     (js-execute obj (format nil "clog['clog-ace-callback'](null,[~{~A~}])"
                             (mapcar (lambda (s)
                                       (if (typep s 'string)
                                           (format nil "{'caption':'~A','value':'~A','score':~A,'meta':'~A'},"
                                                   s s default-score meta)
                                           (format nil "{'caption':'~A','value':'~A','score':~A,'meta':'~A'},"
                                                   (or (getf s :caption) (getf s :value))
                                                   (or (getf s :value) (getf s :caption))
                                                   (or (getf s :score) default-score)
                                                   (or (getf s :meta) meta))))
                                     (funcall handler obj data))))))
  (js-execute obj
              (format nil "var comps={getCompletions: function(editor, session, pos, prefix, callback) {~
                               clog['clog-ace-callback']=callback;
                               ~A.trigger('clog-ace-auto-complete', prefix);}};~
                               var lt=ace.require('ace/ext/language_tools');lt.addCompleter(comps);"
                      (jquery obj)))
  (setf *completer-installed* t))

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

(defgeneric init-clog-ace (clog-ace-element)
  (:documentation "Initialize ace library"))

(defmethod init-clog-ace ((obj clog-ace-element))
  (let* ((root  (merge-pathnames "./www/"
                                 (asdf:system-source-directory :clog-ace)))
         (plug (format nil "~A/clog-ace" root))
         (local (format nil "~A/clog-ace" clog:*static-root*)))
    (cond ((or (uiop:directory-exists-p local)
               (uiop:directory-exists-p plug))
           (unless (uiop:directory-exists-p local)
             (clog-connection:add-plugin-path "^/clog-ace/" plug))
           (load-script (html-document (connection-data-item obj "clog-body"))
                        "/clog-ace/src/ace.js")
           (load-script (html-document (connection-data-item obj "clog-body"))
                        "/clog-ace/src/ext-language_tools.js")
           (load-script (html-document (connection-data-item obj "clog-body"))
                        "/clog-ace/src/ext-modelist.js")
           (js-execute obj "ace.config.set('basePath', '/clog-ace/src/')"))
          (t
           (load-script (html-document (connection-data-item obj "clog-body"))
                        "https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.5/ace.js")
           (load-script (html-document (connection-data-item obj "clog-body"))
                        "https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.5/ext-language_tools.js")
           (load-script (html-document (connection-data-item obj "clog-body"))
                        "https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.5/ext-modelist.js")))))

(defgeneric js-ace (clog-ace-element)
  (:documentation "Access to ace javascript object (private)"))

(defmethod js-ace ((obj clog-ace-element))
  (format nil "clog['editor_~A']" (html-id obj)))

(defgeneric attach-clog-ace (clog-ace-element)
  (:documentation "Initialize plugin"))

(defmethod attach-clog-ace ((obj clog-ace-element))
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
         (text   (create-button (top-panel layout) :content "Text"))
         (move   (create-button (top-panel layout) :content "Move cursor"))
         (cursor   (create-button (top-panel layout) :content "Get cursor")))
    (center-children (center-panel layout))
    (set-auto-completion test t)
    (set-option test "minLines" "1")
    (set-option test "maxLines" "Infinity")
    (print (read-only-p test))
    (setf (read-only-p test) t)
    (print (read-only-p test))
    (setf (read-only-p test) nil)
    (print (read-only-p test))
    (set-on-auto-complete test (lambda (obj data)
                                 (declare (ignore obj data))
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
                         (setf (text-value test) "test text
second line")))
    (set-on-click text (lambda (obj)
                         (declare (ignore obj))
                         (print (text-value test))))
    (set-on-click sel (lambda (obj)
                         (declare (ignore obj))
                         (print (selected-text test))))
    (set-on-click button (lambda (obj)
                           (declare (ignore obj))
                           (set-geometry test :height 300)
                           (clog-ace:resize test)))
    (set-on-click move (lambda (obj)
                         (declare (ignore obj))
                         (move-cursor test 1 7)))
    (set-on-click cursor (lambda (obj)
                           (declare (ignore obj))
                           (multiple-value-bind (row column)
                               (get-cursor test)
                             (format t "~%Row: ~a, column: ~a~%" row column))))
))

(defun start-test ()
  (initialize 'on-test-clog-ace
   :static-root (merge-pathnames "./www/"
                  (asdf:system-source-directory :clog-ace)))
  (open-browser))
