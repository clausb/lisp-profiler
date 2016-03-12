# lisp-profiler

Trivial profiling tool written for Common Lisp, particularly for the Lisp dialect used in [CoCreate Modeling](http://www.clausbrod.de/cgi-bin/view.pl/CoCreateModeling/)
(also known as SolidDesigner, or more recently 
[PTC Creo Elements/Direct Modeling](https://de.wikipedia.org/wiki/Creo_Elements/Direct_Modeling)).

## Introduction

See <http://www.clausbrod.de/Blog/DefinePrivatePublic20160308LispProfiler>. Thanks a lot to AlexG for the original inspiration.

## Status

At this point, consider this code experimental. Contributions are of course welcome, but be aware that the code is still in a state of rapid flux. And probably buggy as hell anyway.

So far, this code was tested in CoCreate Modeling only. It is likely, but not guaranteed to work in other Common Lisp implementations as well.

## Usage

To load the profiler into CoCreate Modeling, load `lisp-profiler.lsp` via File/Load, or enter the following in the user input line:

	(load "lisp-profiler")
	
To enable profiling for an individual function:

	(profiler.clausbrod.de:profile-function 'my-function)
	
Now run your test (which, of course, at some point should call `my-function`), and then review the profiling results as follows:

	(profiler.clausbrod.de:list-profiling-results)
	
To stop profiling `my-function`:

	(profiler.clausbrod.de:unprofile-function 'my-function)
	
The `with-profiler` macro conveniently wraps the above steps into a single call:

	(profiler.clausbrod.de:with-profiler ('my-function)
		(run-some-test-code))
		
To profile the function under test as well as all externally visible functions in a package:

	(profiler.clausbrod.de:with-profiler 
	  ('my-function (find-package "FOO-PACKAGE"))
		(run-some-test-code))


## Limitations

CoCreate Modeling implements a subset of CLtL2 only, which means that some of the other available profiling tools for Common Lisp cannot be used, or at least not directly. Therefore, providing a profiler for CoCreate Modeling is the top priority, even if this means that lisp-profiler may become incompatible with other Lisp implementations.

I will try to provide at least a minimum level of compatibility with other Lisp dialects, though. Every now and then, I may even verify that claim by running tests :-D


