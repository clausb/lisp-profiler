;; UI dialog for profiling in CoCreate Modeling (aka PTC Creo Elements/Direct Modeling)
(in-package :profiler-ui.clausbrod.de)

(use-package :oli)

(require "lisp-profiler" #P"lisp-profiler.lsp")

(use-package :profiler.clausbrod.de)


;; Variables in dialog:
;; - Text field for entering package or function name
;; - Text field for entering code to be profiled (will display profiling results after running test code)
;; - Button to unprofile everything (will also reset hashtable)
(sd-defdialog
 'profiler
 :dialog-title "Profiler"
 :toolbox-button t
 :variables
 '((package-or-function
    :value-type :string
    :title "Pkg/function"
    :prompt-text "Specify package or function to be profiled"
    :after-input (profile-package-or-function package-or-function))
   (code-to-profile
    :title "Code to profile"
    :value-type :string
    :after-input (profile-code code-to-profile))
   (unprofile-all
    :toggle-type :visible
    :push-action (profiler.clausbrod.de:unprofile-all))
   )
 :local-functions
 '((profile-package-or-function(p-or-f)
			       (cond ((find-package p-or-f) (profiler.clausbrod.de:profile-package p-or-f))
				     ((find-symbol p-or-f) (when (fboundp (find-symbol p-or-f)) (profiler.clausbrod.de:profile-function p-or-f)))))
   
   (profile-code(code)
		(with-profiler () (eval (read-from-string code))))
   )
 :ok-action
 '(progn
    (profiler.clausbrod.de:unprofile-all))
 )
