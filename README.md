# lisp-profiler

Trivial profiling tool written for Common Lisp, particularly for the Lisp dialect used in [CoCreate Modeling](http://www.clausbrod.de/cgi-bin/view.pl/CoCreateModeling/)
(also known as SolidDesigner, or more recently 
[PTC Creo Elements/Direct Modeling](https://de.wikipedia.org/wiki/Creo_Elements/Direct_Modeling)).

## Introduction

See <http://www.clausbrod.de/Blog/DefinePrivatePublic20160308LispProfiler>

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

	(profiler.clausbrod.de:with-profiler 'my-function
		(run-some-test-code))
		
## Limitations


This code is geared towards use in CoCreate Modeling. I will try to provide some level of compatibility with other Lisp dialects, but this is not a top priority (yet).


