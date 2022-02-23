getUTC = 
function(url = "https://worldtimeapi.org/api/timezone/etc/UTC")
{
    foo <- suppressWarnings(readLines(url, warn = FALSE))
          ## gives time in sub-secs
    as.POSIXct(gsub(".*\"datetime\":\"([^Z]*).*", "\\1", foo),
                     "UTC", "%Y-%m-%dT%H:%M:%S")
}
