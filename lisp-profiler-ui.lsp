;; UI dialog for profiling in CoCreate Modeling (aka PTC Creo Elements/Direct Modeling)
(in-package :profiler-ui.clausbrod.de)

(use-package :oli)

(require "lisp-profiler" #P"lisp-profiler")

(use-package :profiler.clausbrod.de)


;; Variables in dialog:
;; - Text field for entering package or function name
;; - Text field for entering code to be profiled (will display profiling results after running test code)
;; - Button to unprofile everything (will also reset hashtable)
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
   (unprofile-all
    :title "Unprofile all"
    :toggle-type :visible
    :push-action (profiler.clausbrod.de:unprofile-all))
   )
 
 :local-functions
 '((profile-another-package-or-function(p-or-f)
				       (dolist (segment (sd-string-split p-or-f " "))
					 (profiler.clausbrod.de:profile-functions segment)))
   
   (profile-code(code)
		(with-output-to-string (profiler.clausbrod.de:*profiler-stream*)
				       (with-profiler () (eval (read-from-string code)))
				       (frame2-ui::display (get-output-stream-string profiler.clausbrod.de:*profiler-stream*)))))
 
 :ok-action
 '(profiler.clausbrod.de:unprofile-all)
 
 )
