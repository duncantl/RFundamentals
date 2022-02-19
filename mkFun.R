f = function(dir , files = list.files(dir, full.names = TRUE, ...), numLines = -1, ...) {
    util = function(x) {
               browser()
            length(readLines(x, numLines))
    }
    browser()
    sapply(files,  util  )
}


x = if(cond) {
    tmp = 2
    tmp2 = 3
    tmp + tmp2
    } else if(cond2) {
     ...
    }

x=  if(cond) {
    tmp = 2
    tmp2 = 3
    tmp + tmp2
    } else if(cond2) {
    ...
    }
    else
      val 
