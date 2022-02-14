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
