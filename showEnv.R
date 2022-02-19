showEnv =
function(e = globalenv(), print = FALSE)
{
    ans = list(e)
    ctr = 2L
    while(!is.null(e) && !identical(e, emptyenv())) {
        if(print)
            print(e)
        e = parent.env(e)
        ans[[ctr]] = e
        ctr = ctr + 1L
        
    }
    invisible(ans[-length(ans)])
}



findVarFromFun =
    #
    # analogous to find() but works on call frames, not just the search path.
    # Given a call frame, walk its 
    #
function(var, frame)
{
    echain = showEnv(frame)
    w = sapply(echain, function(e) exists(var, e, inherits = FALSE))
    echain [[ which(w)[1] ]]
}
