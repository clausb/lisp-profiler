;;-*-Lisp-*-

;; UI dialog for ridiculously trivial Lisp profiler
;; See http://www.clausbrod.de/Blog/DefinePrivatePublic20160308LispProfiler

;; Original author: Claus Brod
;;                  http://www.clausbrod.de
;;
;; For licensing details, see the LICENSE file.
;; For documentation, see README.md.

(in-package :profiler-ui.clausbrod.de)

(use-package :oli)

;;(require "lisp-profiler" #P"lisp-profiler")
(require "lisp-profiler")

(use-package :profiler.clausbrod.de)


(sd-defdialog
 'profiler
 :dialog-title "Ridiculously trivial profiler"
 :toolbox-button t
 
 :variables
 '((package-or-function
    :value-type :string
    :title "Pkg/function"
    :prompt-text "Specify package or function to be profiled"
    :after-input (profile-another-package-or-function package-or-function))
   (code-to-profile
    :title "Code to profile"
    :prompt-text "Specify function name or arbitrary Lisp form"
    :value-type :string
    :after-input (profile-code code-to-profile))
   (start-profiling
    :title "Start profiling"
    :toggle-type :grouped-toggle
    :push-action (profile-start))
   (stop-profiling
    :title "Stop profiling"
    :toggle-type :grouped-toggle
    :push-action (profile-stop))
   (unprofile-all
    :title "Unprofile all"
    :toggle-type :wide-toggle
    :push-action (profiler.clausbrod.de:unprofile-all))
   )
 
 :local-functions
 '((profile-another-package-or-function(p-or-f)
				       (dolist (segment (sd-string-split p-or-f " "))
					 (profiler.clausbrod.de:profile-functions segment)))
   
   (profile-code(code)
		(with-output-to-string (profiler.clausbrod.de:*profiler-stream*)
				       (with-profiler () (eval (read-from-string code)))
				       (frame2-ui::display (get-output-stream-string profiler.clausbrod.de:*profiler-stream*))))

   (profile-start()
		 (profiler.clausbrod.de:reset-profiling-results))
   
   (profile-stop()
		(with-output-to-string (profiler.clausbrod.de:*profiler-stream*)
				       (profiler.clausbrod.de:list-profiling-results)
				       (frame2-ui::display (get-output-stream-string profiler.clausbrod.de:*profiler-stream*))))
   
   )
 
 :ok-action
 '(profiler.clausbrod.de:unprofile-all)
 
 )
