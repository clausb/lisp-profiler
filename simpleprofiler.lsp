;;-*-Lisp-*-

;; Extremely trivial profiler for Lisp code

(in-package :clausbrod.de)
(export '(profile-function unprofile-function list-profiling-results with-profiler))

(let ((profile-hashtable (make-hash-table)))
  (defun profile-function(func)
    "Instrument function for profiling"
    
    ;; check if symbol-plist already contains profiler flag
    (unless (get func :profile-original-symbol-function)
      (let ((original-symbol-function (symbol-function func)))
	(when original-symbol-function
	  (setf (get func :profile-original-symbol-function) original-symbol-function) ;; mark as profiled
	  
	  ;; install profiler code
	  (setf (symbol-function func)
		(lambda(&rest r)
		  (let ((start-time (f2::seconds-since-1970)))
		    (unwind-protect
			(if r
			    (apply original-symbol-function r)
			  (funcall original-symbol-function))
		      (let ((execution-time (- (f2::seconds-since-1970) start-time))
			    (accum (gethash func profile-hashtable 0.0)))
			(setf (gethash func profile-hashtable) (+ accum execution-time))
			;;(format *standard-output* "~%Execution time for ~S: ~,10F~%" func execution-time)
			)))))
	  ))))

  (defun profile-package(pkg)
    (do-external-symbols (s pkg)
			 (when (functionp s)
			   (profile-function s))))

  (defun unprofile-package(pkg)
    (do-external-symbols (s pkg)
			 (when (functionp s)
			   (unprofile-function s))))
  
  (defun profile-functions(&rest function-specifiers)
    (dolist (fspec function-specifiers)
      (if (packagep fspec)
	  (profile-package fspec)
	(profile-function fspec))))

  (defun unprofile-functions(&rest function-specifiers)
    (dolist (fspec function-specifiers)
      (if (packagep fspec)
	  (unprofile-package fspec)
	(unprofile-function fspec))))
    
  (defun unprofile-function(func)
    "Remove profiling instrumentation for function"
    (let ((original-symbol-function (get func :profile-original-symbol-function)))
      (when (remprop func :profile-original-symbol-function)
	(setf (symbol-function func) original-symbol-function))))

  (defun list-profiling-results()
    "List profiling results in order of decreasing accumulated execution times"
    (format *standard-output* "~%Accumulated execution times:~%")
    (let (table-as-list)
      (maphash (lambda(k v) (push (cons k v) table-as-list)) profile-hashtable)
      (dolist (pair (sort table-as-list #'> :key #'cdr))
	(format *standard-output* "~,10F  ~S~%" (cdr pair) (car pair)))))

  (defmacro with-profiler(functions &body b)
    `(progn
       (profile-functions ,@functions)
       (progn ,@b)
       (unprofile-functions ,@functions)
       (list-profiling-results)))
   
  )

;; TBD: Try using (setf (fdefinition func) (lambda...))
;;      (with-profiling (f1 f2...) (run-tests))

(f2::win-open-console-window)
(setf si::*enter-break-handler* t)
(use-fast-links nil)

;;(trace profile-function)
;;(trace unprofile-function)
;;(trace profile-package)

(defun test-func(cnt)
 (dotimes (i cnt)
  (format t "~%~a" i)))

(frame2-ui::display (macroexpand '(with-profiler ('test-func (find-package "ELAN")) (test-func 42))))

(with-profiler ('test-func 'format (find-package "ELAN") (find-package "OLI")) (test-func 42))

