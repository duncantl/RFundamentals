#  Questions at
https://docs.google.com/document/d/1hRG5HpKUTL6tLLlBf_RlWWu74_QxduA6p9xRXWvQrBg/edit#

+ [Supporting Code/files](Day5)
+ [Session 5](Day5/Session5)



## Debugging

RStudio will provide a different interface, but to essentially the same debugging functionality.

  
```r
source("read.dcf.R"); source("readEML.R")  

m = readEmailMsg("sample.eml")
```
```
Error in read.dcf(lines = lines[2:(br - 1)], all = all) : 
  Invalid DCF format.
Regular lines must have a tag.
Offending lines start with:
  --sAKGr6Tu023551.1416502513/imapssl.cs.ucdavis.ed
```

I always have 
```
options(error = recover)
```
to examine the call stack when there is an error.
I can easily exit this with 0 and return, but I have the choice to look.
Alternatively, we can call traceback(), bu that just shows the calls, but
doesn't let us examine the call frames and move between them to investigate

So now we run the command again with recover as the error handler.
```r
m = readEmailMsg("sample.eml")
```

```r
Error in read.dcf(lines = lines[2:(br - 1)], all = all) : 
  Invalid DCF format.
Regular lines must have a tag.
Offending lines start with:
  --sAKGr6Tu023551.1416502513/imapssl.cs.ucdavis.ed

Enter a frame number, or 0 to exit   

1: readEmailMsg("sample.eml")
2: readEML.R#28: mkAttachments(m, all = all)
3: readEML.R#77: lapply(att[w], make, bndry)
4: FUN(X[[i]], ...)
5: readEML.R#105: read.dcf(lines = lines[2:(br - 1)], all = all)

Selection: 
```



Looking at the entire stack, we see mkAttachments().
So this past reading the header which is where read.dcf is used first.
But it is also used in processing each attachment as an attachment has a header,
typically much smaller.

We'll look at the 5th call frame as that is where the error happens.
This may not be where the real problem is but we'll get a sense.
So we type 5 and enter and we get a prompt and we are evaluating commands in that call frame,
the call frame of the call to read.dcf()

We can find the variables in the call frame with `ls()`
```
ls()
[1] "all"        "ctype"      "fields"     "file"       "ind" 
[6] "keep.white" "lines"
```
This is my own read.dcf() rather than the one in the `base` package.

We can look at the body by either looking in the read.dcf.R file, or directly in R,
```r
body()

{
    if (missing(lines)) {
        if (is.character(file)) {
            file <- gzfile(file)
            on.exit(close(file))
        }
        if (!inherits(file, "connection")) 
            stop("'file' must be a character string or connection")
    }
    else file = textConnection(lines, local = TRUE, name = "bob")
    if (!all) 
        return(.Internal(readDCF(file, fields, keep.white)))
    .assemble_things_into_a_data_frame <- function(tags, vals, 
        nums) {
        tf <- factor(tags, levels = unique(tags))
        cnts <- table(nums, tf)
        out <- array(NA_character_, dim = dim(cnts), dimnames = list(NULL, 
            levels(tf)))
        if (all(cnts <= 1L)) {
            out[cbind(nums, tf)] <- vals
            out <- as.data.frame(out, optional = TRUE, stringsAsFactors = FALSE)
        }
        else {
            levs <- colSums(cnts > 1L) == 0L
            if (any(levs)) {
                inds <- tf %in% levels(tf)[levs]
                out[cbind(nums[inds], tf[inds])] <- vals[inds]
            }
            out <- as.data.frame(out, optional = TRUE, stringsAsFactors = FALSE)
            for (l in levels(tf)[!levs]) {
                out[[l]] <- rep.int(list(NA_character_), nrow(cnts))
                i <- tf == l
                out[[l]][unique(nums[i])] <- split(vals[i], nums[i])
            }
        }
        out
    }
    ctype <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", ctype), add = TRUE)
    Sys.setlocale("LC_CTYPE", "C")
    ind <- grep("^[^[:blank:]][^:]*$", lines)
    if (length(ind)) {
        lines <- substr(lines[ind], 1L, 0.7 * getOption("width"))
        stop(gettextf("Invalid DCF format.\nRegular lines must have a tag.\nOffending lines start with:\n%s", 
            paste0("  ", lines, collapse = "\n")), domain = NA)
    }
    line_is_not_empty <- !grepl("^[[:space:]]*$", lines)
    nums <- cumsum(diff(c(FALSE, line_is_not_empty) > 0L) > 0L)
    nums <- nums[line_is_not_empty]
    lines <- lines[line_is_not_empty]
    line_is_escaped_blank <- grepl("^[[:space:]]+\\.[[:space:]]*$", 
        lines)
    if (any(line_is_escaped_blank)) 
        lines[line_is_escaped_blank] <- ""
    line_has_tag <- grepl("^[^[:blank:]][^:]*:", lines)
    pos <- c(1L, which(diff(nums) > 0L) + 1L)
    ind <- !line_has_tag[pos]
    if (any(ind)) {
        lines <- substr(lines[pos[ind]], 1L, 0.7 * getOption("width"))
        stop(gettextf("Invalid DCF format.\nContinuation lines must not start a record.\nOffending lines start with:\n%s", 
            paste0("  ", lines, collapse = "\n")), domain = NA)
    }
    lengths <- rle(cumsum(line_has_tag))$lengths
    pos <- cumsum(lengths)
    tags <- sub(":.*", "", lines[line_has_tag])
    lines[line_has_tag] <- sub("[^:]*:[[:space:]]*", "", lines[line_has_tag])
    fold <- is.na(match(tags, keep.white))
    foldable <- rep.int(fold, lengths)
    lines[foldable] <- sub("^[[:space:]]*", "", lines[foldable])
    lines[foldable] <- sub("[[:space:]]*$", "", lines[foldable])
    vals <- mapply(function(from, to) paste(lines[from:to], collapse = "\n"), 
        c(1L, pos[-length(pos)] + 1L), pos)
    vals[fold] <- trimws(vals[fold])
    out <- .assemble_things_into_a_data_frame(tags, vals, nums[pos])
    if (!is.null(fields)) 
        out <- out[fields]
    out
}
```
RStudio's GUI may point to which "line"/expression is currently being evaluated and so where we are
    in the function.

We may want to proatively stop at the start of read.dcf with `debug(read.dcf)`.
However, that will stop when working on the email header, before we get to the attachment.

We check whether all is TRUE or FALSE. It is TRUE, so that means
we didn't evaluate `return(.Internal(readDCF()))`

We could have inferred this from the existence of the variables ctype, ind, etc. which are defined
after this.


The error message we got was 'Invalid DCF format   Regular lines must have a tag'.
Let's look for this in the code as that is where the error is happening.
And it is 
```r
    if (length(ind)) {
        lines <- substr(lines[ind], 1L, 0.7 * getOption("width"))
        stop(gettextf("Invalid DCF format.\nRegular lines must have a tag.\nOffending lines start with:\n%s", 
            paste0("  ", lines, collapse = "\n")), domain = NA)
    }
```

So what is the value of ind
```
ind
[1] 2
```
So let's look at lines,
specifically the second element.
```
lines[2]
[1] NA
```
That's odd.
What's the length of lines?  
```
length(lines)
[1] 1
```
So there is no second element and that is why we got `NA`.

So why did we get only one line?

We look at the calls
```
sys.calls()
```
We see 
```r
[[1]]
readEmailMsg("sample.eml")

[[2]]
if(attachments)
        mkAttachments(m, all = all)
    else
        m

[[3]]
attachments = lapply(att[w], make, bndry)

[[4]]
FUN(X[[i]], ...)

[[5]]
h = read.dcf(lines = lines[2:(br-1)], all = all)

[[6]]
stop(gettextf("Invalid DCF format.\nRegular lines must have a tag.\nOffending lines start with:\n%s", 
            paste0("  ", lines, collapse = "\n")), domain = NA)
```

So we see lines being explicitly passed as a named argument with the value
`lines[2:(br-1)]`.
So let's jump to call frame 4 and examine its lines variable and the value of br.



The message is supposed to look like

```
From: Some One <someone@example.com>
MIME-Version: 1.0
Content-Type: multipart/mixed;
        boundary="XXXXboundary text"

This is a multipart message in MIME format.

--XXXXboundary text
Content-Type: text/plain

this is the body text

--XXXXboundary text
Content-Type: text/plain;
Content-Disposition: attachment;
        filename="test.txt"

this is the attachment text

--XXXXboundary text--
```
https://docs.microsoft.com/en-us/previous-versions/office/developer/exchange-server-2010/aa563375(v=exchg.140)






##  Troubleshooting code with match.call(), new.env(), parent.env()

Generally, you don't need these explicitly. Instead, we used them to emulate/mimic/construct
R's evaluation model and show it in action. and what is going on **implicitly**.

The debug and recover functions (above) allows us to 
+ jump between call frames,
+ use ls() to list the variables, 
+ get their values,  
+ do computations on them, and 
+ find functions and other variables  that this function can see along its environment chain.

We can do the last of these explicitly with `parent.env()` and the function
I wrote `findVarInFrame()` in [showEnv.R](showEnv.R).

new.env() was how we showed R creating new call frames, or environments
in which we explicitly evaluated an expression when it would have been evaluated
in a different one by default.


### match.call()
You don't need match.call() very often when you 
+ provide the parameter name for all arguments in a call, or
+ have internalized how R matches arguments in a call to parameters in the function being called

When debugging, we can just step through a function and examine 
the value of parameter and see its value. So we just know what value(s) matched it.
However, match.call() can be useful 
+ when writing code and you are not sure which arguments will be matched to which parameter,
  especially ..., before you run it and want to verify, or
+ you are looking at the value of a parameter in a function call, and looking at the call
  and can't see why the parameter has this value based on the call and 
  want to examine that call to the function and see what it really matched.
  
  
We can use all of these functions to do non-standard evaluation, but avoid that
when you can.



# Interpreting Errors
Such as coercion and others.
+ Examples

```
as.numeric(letters[1:3])
[1] NA NA NA
Warning message:
NAs introduced by coercion 
```
Not an error, but outcome may not be what you wanted.

Sometimes useful to convert warnings to errors
```
opions(warn = 2)  # negative, 0, 1 or 2
```


What about errors when coercing
```
as.numeric(list(1:3))
```

```
as.numeric(list(1:3, 10:4))
```


Implicit coercion with c()
```r
c(1:3, letters[1:5])
```
but
```r
c(1:3, letters[1:5], list(a = 1))
```
If we want a list with 3 elements - 1:3,  letters[1:5], list(a = 1) then use list()
```r
list(1:3, letters[1:5], list(a = 1))
```




# Profiling

+ Debugging/rw1.R
+ Debugging/Prof.md



```r
e = new.env()
source("rw1.R", e)
tm = lapply(as.list(e), function(f) system.time(f(N)))
o = order(as.numeric(gsub("rw2d", "", names(tm))))
tm = tm[o]
e = sapply(tm, `[`, 3)
plot(e)
mapply(`/`, e[1:(length(e)-1)], e[2:length(e)])
```
---

+ Using [MJ & Serena's example](https://github.com:duncantl/MJSRandPerm.git).


+ Profile the original script
```
rpIter = 4
Rprof("prof")
source("origScript.R")
Rprof(NULL)
p = summaryRprof("prof")
head(p$by.self, 20)
```
Looking at by.self to see which functions take a lot of time
doing their own work, not calling other functions.
```
                               self.time self.pct total.time total.pct
"lazyLoadDBfetch"                   0.80     7.74       0.80      7.74
"<Anonymous>"                       0.52     5.03       2.38     23.02
"is"                                0.40     3.87       0.64      6.19
"map"                               0.38     3.68       0.44      4.26
"withCallingHandlers"               0.36     3.48       2.56     24.76
"chop_rectangular_df"               0.34     3.29       0.36      3.48
"stopifnot"                         0.30     2.90       0.30      2.90
"data.frame"                        0.28     2.71       0.40      3.87
"as.list"                           0.28     2.71       0.34      3.29
".External2"                        0.26     2.51       4.66     45.07
"findCenvVar"                       0.22     2.13       0.24      2.32
"ls"                                0.20     1.93       0.20      1.93
"is_negated_colon"                  0.18     1.74       0.28      2.71
"file.exists"                       0.18     1.74       0.18      1.74
"%in%"                              0.12     1.16       0.38      3.68
"length"                            0.12     1.16       0.12      1.16
"NROW"                              0.12     1.16       0.12      1.16
"rlang_as_list_from_list_impl"      0.12     1.16       0.12      1.16
"FUN"                               0.10     0.97       0.82      7.93
"getClassDef"                       0.10     0.97       0.28      2.71
```

Probably want to run the script several times to avoid the 
first-time loading/fetching of variables,
byte-code compilation.
Unfortunately, the script contains the two functions
so each time we source() the script, these are redefined
and not necessarily byte-compiled.

+ So split the functions out and source them once and then
run the script several times, or explicitly byte-compile them manually.

From now on, use 
```
source("origScript_noFuns.R")
```
Run this 3 or 4 times and then profile again

```
Rprof("prof")
source("origScript_noFuns.R")
Rprof(NULL)
head(summaryRprof("prof")$by.self, 20)
```

```
                                self.time self.pct total.time total.pct
"map"                               0.56     9.96       0.62     11.03
"<Anonymous>"                       0.54     9.61       1.62     28.83
"data.frame"                        0.40     7.12       0.52      9.25
"withCallingHandlers"               0.22     3.91       2.62     46.62
"chop_rectangular_df"               0.18     3.20       0.18      3.20
"ls"                                0.18     3.20       0.18      3.20
".External2"                        0.16     2.85       4.10     72.95
"stopifnot"                         0.16     2.85       0.16      2.85
"is_negated_colon"                  0.14     2.49       0.22      3.91
"is_quosure"                        0.14     2.49       0.14      2.49
"order"                             0.10     1.78       0.20      3.56
"$"                                 0.10     1.78       0.14      2.49
"FUN"                               0.08     1.42       0.28      4.98
"utils::getAnywhere"                0.08     1.42       0.26      4.63
"vec_match"                         0.06     1.07       0.14      2.49
"[.data.frame"                      0.06     1.07       0.08      1.42
"NROW"                              0.06     1.07       0.06      1.07
"rlang_as_list_from_list_impl"      0.06     1.07       0.06      1.07
"eval"                              0.04     0.71       5.62    100.00
"pivot_wider_spec"                  0.04     0.71       2.06     36.65
```


After reading through the code
+ we can move some code out of the last loop and do it in a vectorized way for all the results.
+ some commands are redundant
```r
list[dfMissing_pairedWide_RP, fit.LMEMis_RP] <- pairTrials_RandomPerm(dfMissing)
```
   + Uses non-standard call to list[] to do multiple assignments
   + never uses the first value. So don't assign it.

+ Let's drill-down/focus on pairTrials_RandomPerm
```r
Rprof("prof")
invisible(replicate(20, pairTrials_RandomPerm(dfMissing)))
Rprof(NULL)
head(summaryRprof("prof")$by.self, 20)
```
```
                                self.time self.pct total.time total.pct
"&lt;Anonymous&gt;"                       3.46    13.57       5.36     21.02
"map"                               2.16     8.47       2.44      9.57
"data.frame"                        1.60     6.27       2.00      7.84
"withCallingHandlers"               1.36     5.33      13.74     53.88
"chop_rectangular_df"               1.36     5.33       1.38      5.41
"stopifnot"                         0.96     3.76       0.98      3.84
"is_quosure"                        0.82     3.22       0.86      3.37
".External2"                        0.58     2.27      22.10     86.67
"NROW"                              0.50     1.96       0.50      1.96
"is_negated_colon"                  0.42     1.65       1.18      4.63
"[.data.frame"                      0.42     1.65       0.58      2.27
"$"                                 0.36     1.41       0.64      2.51
"rlang_as_list_from_list_impl"      0.36     1.41       0.36      1.41
"order"                             0.32     1.25       0.48      1.88
"propagate_names"                   0.30     1.18       1.88      7.37
"parent.frame"                      0.26     1.02       0.26      1.02
"anyDuplicated"                     0.24     0.94       0.28      1.10
".row_names_info"                   0.20     0.78       0.20      0.78
"pivot_wider_spec"                  0.18     0.71      10.14     39.76
"$<-.data.frame"                    0.18     0.71       0.80      3.14
```
+ Anonymous takes the longes, but we don't easily know what that is (working on making this easier)  
  and it could be many different functions.
+ We could trace() the calls to data.frame to see how many there are, where they are coming from
+ withCallingHandlers() is related to tryCatch() and seems to be expensive here.
+ is_quosure) is tidyversy
+ .External2 calls compiled code.  What are these routines? how many are there? and how many calls to each?

None of these look very familiar or obvious as to where they are being called from
So let's look at by.total data.frame from summaryRprof().  
This is how much time is spent in a function and the functions it calls.
So not clear how to assign the time to. But can be useful since 
the by.self didn't give many clues.
  
```
                          total.time total.pct self.time self.pct
"lapply"                       25.50    100.00      0.12     0.47
"FUN"                          25.50    100.00      0.08     0.31
"pairTrials_RandomPerm"        25.50    100.00      0.02     0.08
"replicate"                    25.50    100.00      0.00     0.00
"sapply"                       25.50    100.00      0.00     0.00
".External2"                   22.10     86.67      0.58     2.27
"pivot_wider"                  21.78     85.41      0.02     0.08
"%>%"                          21.78     85.41      0.00     0.00
"pivot_wider.data.frame"       21.72     85.18      0.02     0.08
"withCallingHandlers"          13.74     53.88      1.36     5.33
"tidyselect::eval_select"      11.88     46.59      0.02     0.08
"eval_select_impl"             11.82     46.35      0.06     0.24
"tryCatch"                     11.58     45.41      0.00     0.00
"doTryCatch"                   11.54     45.25      0.10     0.39
"tryCatchList"                 11.54     45.25      0.00     0.00
"tryCatchOne"                  11.54     45.25      0.00     0.00
"with_subscript_errors"        11.50     45.10      0.02     0.08
"instrument_base_errors"       11.38     44.63      0.00     0.00
"vars_select_eval"             11.32     44.39      0.08     0.31
"pivot_wider_spec"             10.14     39.76      0.18     0.71
```
What does this tell me?  our top-level replicate()/lapply() and implicit FUN, and pairTrials_RandomPerm are the top-level function calls
so are highest on this list. 
But, .External2 and pivot_wider and the pipe %>% account for a lot of time, probably calling other functions.



I decided to get rid of pivot_wider.
I debugged the function pairTrials_RandomPerm to see the
inputs and outputs for pivot_wider and inferred what I think is a) what it does,
and b) the specifics of this use of pivot_wider()

So I implemented the specifics directly. (We made this faster later.)
```
source("funs_no_pivot_wider.R")
z = pairTrials_RandomPerm(dfMissing)

Rprof("prof")
z = replicate(20, pairTrials_RandomPerm(dfMissing))
Rprof(NULL)
summaryRprof(
```

BTW, `profile = function(expr){ Rprof(tf <- tempfile()) ; expr; Rprof(NULL); summaryRprof(tf)}`)




## Profiling Example 2 

+ Example from Ross Ihaka, one of the 2 creators of R.


profile = function(expr){ Rprof(tf <- tempfile()) ; expr; Rprof(NULL); summaryRprof(tf)}
source("rw1.R")

N = 1e5
system.time(rw2d1(N))
   user  system elapsed 
  2.610   0.030   2.663
  
system.time(p<-profile(rw2d1(N)))
head(p$by.self, 20)
             self.time self.pct total.time total.pct
"sample.int"      1.54    64.17       1.70     70.83
"sample"          0.46    19.17       2.18     90.83
"rw2d1"           0.22     9.17       2.40    100.00
"/"               0.04     1.67       0.04      1.67
">"               0.04     1.67       0.04      1.67
"is.null"         0.04     1.67       0.04      1.67
"!"               0.02     0.83       0.02      0.83
"<="              0.02     0.83       0.02      0.83
"length"          0.02     0.83       0.02      0.83


+ Why switch sample.int ? Tossing coins < or >= .5. Maybe faster to use runif() than sample from set of values?

system.time(p<-profile(rw2d2(N)))
   user  system elapsed 
  1.274   0.047   1.328

+ 2 times faster

        self.time self.pct total.time total.pct
"runif"      0.86    72.88       0.86     72.88
"rw2d2"      0.32    27.12       1.18    100.00

+ All the time in runif()
+ How many calls to runif()?

# Next step

+ Vectorize the runif() to generate all n in one go.
+ 2 times for horiz or vertical and then the amount +1 or -1

```
system.time(p<-profile(rw2d2.5(N)))
   user  system elapsed 
  0.086   0.001   0.088
```  


  
  
# quo, enquo, !!, !!!

Frankly, this isn't the R evaluation model, but a very different evaluation model  implemented
in R, that mimics many concepts and functionality that can already be done in R.

Just like this series of workshops, if you are going to use this computational model a lot,
it is worthwhile learning it at a fundamental  level so that you don't have to 
"Dr. Google it" or "experiment by changing something to see if it works".
You want to focus on expressing yourself, not guessing what will happen next.
This proficiency where you are saying what you mean, rather than hoping,  makes you so much more
productive on the task at hand. You will still have bugs, but you can reason about them.

Let's try to understand this together.

+ What does the {{}} do?
+ What do the different functions expect as arguments? - typically a data.frame() as that is an
  enormous  assumption/restriction in the tidyverse.


```r
separate_rows("a;b;c", sep = ";")
```
This gives an error. Instead, I have to be very indirect and put the character
vector in a data frame and then specify the column.
This is very odd.  " I want to give you something. So I will put it in a bag, then hand the bag to
you, and then you take it out of the bag" versus  "hand you the thing"

```
separate_rows(data.frame(x = "a;b;c"), x, sep = ";")
```

We can use strsplit and unlist for this.
What about multiple columns
```
separate_rows(data.frame(x = "a;b;c", y = "x;y"), c(x, y), sep = ";")
```
```
Error: In row 1, can't recycle input of size 3 to size 2.
Run `rlang::last_error()` to see where the error occurred.
```

But it can deal with  equal number of terms in each element
```
separate_rows(data.frame(x = "a;b;c", y = "x;y;z"), c(x, y), sep = ";")
```
What about more than one element per vector
```
separate_rows(data.frame(x = c("a;b;c", "1;2"), y = c("x;y;z", "101;109")), c(x, y), sep = ";")
```
So just all pairs of elements have to have the same number of elements.


