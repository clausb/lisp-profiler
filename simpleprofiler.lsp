;;-*-Lisp-*-

;; Extremely trivial profiler for Lisp code

(in-package :clausbrod.de)
(export '(profile-function unprofile-function list-profiling-results))

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
         (format *standard-output* "~%Execution time for ~S: ~,10F~%" func execution-time))))))
     ))))

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
   (format *standard-output* "~S:  ~,10F~%" (car pair) (cdr pair)))))
  )

(f2::win-open-console-window)
(setf si::*enter-break-handler* t)
(use-fast-links nil)

