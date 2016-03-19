;;-*-Lisp-*-

;; Ridiculously trivial profiler for Lisp code
;; See http://www.clausbrod.de/Blog/DefinePrivatePublic20160308LispProfiler

(in-package :profiler.clausbrod.de)
(export '(profile-function
	  profile-package
	  profile-functions
	  unprofile-function
	  unprofile-all
	  list-profiling-results
	  with-profiler
	  *profiler-stream*))

(provide "lisp-profiler")

;; To redirect profiling output, create a dynamic binding for this stream before profiling.
(defvar *profiler-stream* *standard-output*)

(let ((profile-hashtable (make-hash-table)))
  (defun reset-profiling-results()
    (clrhash profile-hashtable))
  
  (defun update-hashtable(func execution-time)
    (let ((accum (gethash func profile-hashtable 0.0)))
      (setf (gethash func profile-hashtable) (+ accum execution-time))))

  (defun list-profiling-results()
    "List profiling results in order of decreasing accumulated execution times"
    (format *profiler-stream* "~%Accumulated execution times:~%")
    
    (let (table-as-list)
      (maphash (lambda(k v) (push (cons k v) table-as-list)) profile-hashtable)
      
      (dolist (pair (sort table-as-list #'> :key #'cdr))
	(let* ((sym (symbol-name (car pair)))
	       (sympkg (symbol-package sym)))
	  (multiple-value-bind (s st) (find-symbol sym sympkg)
	    (format *profiler-stream* "~,10F  ~A~A~A~%"
		    (cdr pair)
		    (package-name sympkg)
		    (if (eq :internal st) "::" ":") sym))))))
  )


(defun get-current-time-in-microseconds()
  #+hcl (f2::seconds-since-1970)
  #-hcl (* (/ 1000000 internal-time-units-per-second) (get-internal-real-time))
  )

(defun execute-with-profiling(func original-symbol-function args)
  (let ((start-time (get-current-time-in-microseconds)))
    (unwind-protect
	(if args
	    (apply original-symbol-function args)
	  (funcall original-symbol-function))
      (update-hashtable func (- (get-current-time-in-microseconds) start-time)))))

(let ((unprofilable-functions (make-hash-table :test 'equal)))
  (setf (gethash "FRAME2" unprofilable-functions) '("SECONDS-SINCE-1970"))
    
  (defun profilable-p(func)
    (let ((p (gethash (package-name (symbol-package func)) unprofilable-functions)))
      (not (member (symbol-name func) p :test 'equal))))

  (defun profile-function(func)
    "Instrument function for profiling"

    (when (stringp func) (setf func (find-symbol func)))
    
    (when (or (not (fboundp func))
	      (get func :profile-original-symbol-function))
      (return-from profile-function))

    (unless (profilable-p func)
      (return-from profile-function))
    
    (let ((original-symbol-function (symbol-function func)))
      (setf (get func :profile-original-symbol-function) original-symbol-function) ;; mark as profiled
      
      ;; install profiler code
      (setf (symbol-function func)
	    (lambda(&rest r) (execute-with-profiling func original-symbol-function r))))))
  

(defun profile-package(pkg)
  "Profile all external functions in a package"
  (do-external-symbols (s pkg)
		       (when (and (functionp s) (fboundp s))
			 (profile-function s))))

(defun unprofile-package(pkg)
  (do-external-symbols (s pkg)
		       (when (and (functionp s) (fboundp s))
			 (unprofile-function s))))

(defun unprofile-function(func)
  "Remove profiling instrumentation for function"
  (let ((original-symbol-function (get func :profile-original-symbol-function)))
    (when (remprop func :profile-original-symbol-function)
      (setf (symbol-function func) original-symbol-function))))

(defun to-package(fspec)
  (cond ((packagep fspec) fspec)
	((stringp fspec) (find-package fspec))
	((keywordp fspec) (find-package fspec))
	((symbolp fspec) (find-package fspec))))

(defun profile-functions(&rest function-specifiers)
  (dolist (fspec function-specifiers)
    (if (to-package fspec)
	(profile-package (to-package fspec))
      (profile-function fspec))))

(defun unprofile-functions(&rest function-specifiers)
  (dolist (fspec function-specifiers)
    (if (packagep fspec)
	(unprofile-package fspec)
      (unprofile-function fspec))))

(defun unprofile-all()
  (apply 'unprofile-functions (list-all-packages)))

(defmacro with-profiler(function-specifiers &body b)
  `(progn
     (reset-profiling-results)
     (profile-functions ,@function-specifiers)
     (progn ,@b)
     (unprofile-functions ,@function-specifiers)
     (list-profiling-results)))

