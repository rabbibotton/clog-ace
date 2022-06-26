(in-package :clog-ace)

(progn
  (clog-tools:add-supported-controls
   (list `(;; unique name to control used to identify it the .clog xml
           :name           "clog-ace"
	   ;; how control appears in builder control list 
	   :description    "clog-ace"
	   ;; the common lisp type of the control
	   :clog-type      clog-ace:clog-ace-element
	   ;; the create-function used to create the function
	   ;; at _design time_ at run time only clog:attach-as-child is used
	   ;; any initialization at _run time_ is done with :on-setup below.
	   :create         clog-ace:create-clog-ace-element
	   ;; clog has the following create-types
	   ;;   :base         - create
	   ;;   :element      - create create-content
	   ;;   :form         - create create-param create-value
	   ;;   :text-area    - create create-value
	   ;;   :custom-query - create (ask user for string)
	   ;;   :custom       - create create-content
	   :create-type    :base
	   ;; setup the control at _design time_ and custom attributes
	   :setup          ,(lambda (control content control-record)
                              (declare (ignore content) (ignore control-record))
			      ;; tell the builder this is a composite control
			      (setf (attribute control "data-clog-composite-control") "t")			      
			      ;; default custom attribute values and events at design time
                              (setf (attribute control "data-clog-ace-theme") "ace/theme/xcode")
                              (setf (attribute control "data-clog-ace-mode") "ace/mode/lisp")
                              (setf (attribute control "data-clog-ace-tab-size") "2"))
	   ;; code to run at _run time_ after all controls attached to panel
	   :on-setup       ,(lambda (control control-record)
                              (declare (ignore control control-record))
			      ;; initialization at run time and apply custom attributes
			      (format nil "(clog-ace:attach-clog-ace target)
(clog-ace:theme target \"~A\")
(clog-ace:mode target \"~A\")
(clog-ace:tab-size target ~A)"
				      (attribute control "data-clog-ace-theme")
				      (attribute control "data-clog-ace-mode")
				      (attribute control "data-clog-ace-tab-size")))
	   ;; events handled
           :events         (,@clog-tools::*events-element*)
	   ;; properties handled
	   :properties     ((:name "ace theme"
			     :attr "data-clog-ace-theme")
			    (:name "ace mode"
			     :attr "data-clog-ace-mode")
			    (:name "ace tab size"
			     :attr "data-clog-ace-tab-size")
			    ,@clog-tools::*props-element*))))
  (format t "~%clog-ace installed in CLOG Builder"))
