;;-*-Lisp-*-

;; Extremely trivial profiler for Lisp code

(in-package :profiler.clausbrod.de)
(export '(profile-function unprofile-function list-profiling-results with-profiler))

(defun get-current-time-in-microseconds()
  #+hcl (f2::seconds-since-1970)
  #+clisp (get-internal-real-time)
  #-hcl #-clisp (* (/ 1000000 internal-time-units-per-second) (get-internal-real-time))
  )

(let ((profile-hashtable (make-hash-table)))
  (defun execute-with-profiling(func original-symbol-function args)
    (let ((start-time (get-current-time-in-microseconds)))
      (unwind-protect
	  (if args
	      (apply original-symbol-function args)
	    (funcall original-symbol-function))
	(let ((execution-time (- (get-current-time-in-microseconds) start-time))
	      (accum (gethash func profile-hashtable 0.0)))
	  (setf (gethash func profile-hashtable) (+ accum execution-time)))))) 
  
  (defun profile-function(func)
    "Instrument function for profiling"

    (when (or (not (fboundp func))
	      (get func :profile-original-symbol-function))
      (return-from profile-function))
    
    (let ((original-symbol-function (symbol-function func)))
      (setf (get func :profile-original-symbol-function) original-symbol-function) ;; mark as profiled
	
      ;; install profiler code
      (setf (symbol-function func)
	    (lambda(&rest r) (execute-with-profiling func original-symbol-function r)))))

  (defun profile-package(pkg)
    "Profile all external functions in a package"
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

  (defun reset-profiling-results()
    (clrhash profile-hashtable))
    
  (defun list-profiling-results()
    "List profiling results in order of decreasing accumulated execution times"
    (format *standard-output* "~%Accumulated execution times:~%")
    (let (table-as-list)
      (maphash (lambda(k v) (push (cons k v) table-as-list)) profile-hashtable)
      (dolist (pair (sort table-as-list #'> :key #'cdr))
	(format *standard-output* "~,10F  ~S~%" (cdr pair) (car pair)))))

  (defmacro with-profiler(function-specifiers &body b)
    `(progn
       (reset-profiling-results)
       (profile-functions ,@function-specifiers)
       (progn ,@b)
       (unprofile-functions ,@function-specifiers)
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
