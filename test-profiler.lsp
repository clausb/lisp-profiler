;;-*-Lisp-*-

;; Test code for ridiculously trivial profiler for Lisp code
;; See http://www.clausbrod.de/Blog/DefinePrivatePublic20160308LispProfiler

(in-package :clausbrod.de)

(require "lisp-profiler" #P"lisp-profiler")

(use-package :profiler.clausbrod.de)

#+hcl (f2::win-open-console-window)
#+hcl (setf si::*enter-break-handler* t)
#+hcl (use-fast-links nil)

(defun test-func(cnt)
 (dotimes (i cnt)
  (format t "~%~a" i)))

;;(frame2-ui::display (macroexpand '(with-profiler ('test-func (find-package "ELAN")) (test-func 42))))

#+hcl (with-profiler ('test-func 'format (find-package "ELAN") (find-package "OLI")) (test-func 42))
