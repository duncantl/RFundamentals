My background
Why "mastering" a language saves time and is worth the investment.


+ REPL
+ Parsing 
   + incomplete expressions
     + if  and trailing else.
   + syntax errors
   + language objects and exploring
      + e = parse(text = "x <- pi + 1")
     
+ Basic rules for evaluation
  + eval() to see what happens
+ literal  (TRUE, 1, 1L, "string")
  + not T/F
+ symbol - evaluate x (after assigning x <- 1)
  + find along search path
  + find(), get()
  + search path
     + library()
	 + Don't use attach()
+ calling functions  
  + (almost) everything in R is a function call.
     + including =/<-, if, for, +, <, :
  + finding the function - same as symbol, but has to be a function. Keep looking.
     + including x$foo(1, 2)
  + pass by value - copy
     + on write
	 + some exceptions - connections, environments, external pointer, external resources
	 + functional programming
+ passing & evaluating arguments
  + call frames
  + matching arguments to parameters
     + exact name
	 + partial name
	 + position
	 + ...
	 + error
  + match.call
  + lazy evaluation
     + may never be evaluated
	 + evaluated in caller's frame
     + substitute()
  + default values evaluated in 	 
  
+ try(), tryCatch()
    + lapply(x, function(v) try(foo(x)))

+ How functions work
    + evaluating the calls within
	+ return() - single object
	+ not needed - result is the value of the last expression.
	+ if() ... else ...
	+ Scope		 
  	  + how values are found

+ OOP
   + S3 methods
     + generic function
	 + finding method

+ perils of attach()

+ garbage collection
   x = 1:10
   x = 2
   previous value gets garbage collected.

Data structures
 + vectors and vectors of length 1. No scalars
 + logical, integer, numeric, [complex], character
 + hierarchy of coercion. Why?
 + typeof(), class(), 
   + inherits versus class(x) == "foo" 
 + What class is NA
 + operations involving NA. is.na()
    + x == NA
 + factor - categorical variables
    + used in modeling
    + efficient representation
	+ subset
	+ ordered factors
 + Date, POSIXct	
 + list
 + data.frame - list with vectors all having same length with relationship between i-th element
   across vectors within data.frame.
 + matrix
   + versus data.frame
 + attributes
   + "hang" additional information on an object.
     + difference between  obj with attributes and list(obj, moreInfo)
   + names, class, dim, dimnames
   + attr(), attr<-, attributes
   + structure()
 + names
 
+ Subsetting
   + position/index
   + negative index
   + logical
   + name
   + two-dimensional subsetting
   + matrix subsetting
      + x[i, j]
	  + x[ cbind(i, j) ]
   + [ and [[ for lists.
   + $ and partial name matching.
   
   + hierarchical indexing.
      + erroneously

+ Vectorized operations
  + recycling rule
  + mapping between vectors
     + match()



+ Writing Functions 
  + default values
  + on.exit
     + close connections
	 + reset options(), par(), environment variables
	 + delete temporary files (or use tempfile())
	 + remove temporary files
  + specifying functions by package prefix.
  + checking for non-local variables
     + codetools::findGlobals, CodeAnalysis::getGlobals  
	 
  +  x = if() ... else	... rather than
         if() x = .. else x = ...

+ Debugging Functions
  + browser(), recover(), debug, trace, 
  + traceback
  + options(error = recover)
  + graphical interface.
  + debugging a function defined inside a function call.
    ¿example?

+ raising errors, warnings
   + conditions
   + stop(), warning()

+ formulae and scope

+ NSE generally

+ Loops
  + know when to use them.
  + preallocate
  + apply functions

+ Writing Packages
  + functions in packages
  + exporting variables
  + non-exported internal helper functions
  + S3 methods
  + S4 classes and methods.
