# !Message Threads.

readEmailMsg =
function(f, x = readLines(f, warn = FALSE), attachments = TRUE, all = TRUE)
{
    br = which(x == "")[1]
    if(is.na(br)) return(x)
    hdr = read.dcf(f, all = all, lines = x[1:br])
    m = list(header = hdr, body = x[-(1:br)])
    if(attachments)
        mkAttachments(m, all = all)
    else
        m
}

mkAttachments  =
function(m, make = mkAttachment, all = TRUE)
{
    if(!"Content-Type" %in% names(m$header))
        return(m)

    ty = strsplit(m$header$"Content-Type", ";")[[1]]
    i = grep("boundary", ty)
    if(length(i) == 0)
        return(m)

#XX Handle boundary that has a sequence 0 1 2 3 4 at the end. 
    bndry = trimws(gsub("boundary=", "", ty[i]))
    bndry = gsub('^"|"$', '', bndry)
    bndry = paste0("--", bndry)
    g = grepl(bndry, m$body)

    att = split(m$body, cumsum(g))

    # Make more precise. Has to start with the boundary.    
#    w = grepl(bndry, sapply(att, `[`, 1), fixed = TRUE)
     w = substring(sapply(att, `[`, 1), 1, nchar(bndry)) == bndry
    
    m$body = unlist(att[!w])

    attachments = lapply(att[w], make, bndry)
    attachments = attachments[ ! sapply(attachments, is.null) ]
    
    m$att = attachments
    m
}

mkAttachment =
function(lines, boundary, all = TRUE)
{
    if(lines[1] == paste0(boundary, "--") & length(lines[lines != ""]) == 1 )
        return(NULL)

    br = which(lines == "")[1]
#if(br < 10) recover() # browser()
#    h = list()
    if(br > 2)
        h = read.dcf(lines = lines[2:(br-1)], all = all)
    
    list(header = h, body = lines[ -(1:br) ])
}



mkInfoDF =
function(msgs, files = names(msgs))
{
    from = sapply(msgs, function(x) { m = match("from", tolower(names(x$header))); x$header[[m]]})

    efrom = gsub(".* <(.*)>", "\\1", from)

    subject = sapply(msgs, function(x) if("Subject" %in% names(x$header))  x$header$"Subject" else NA)
    to = sapply(msgs, function(x) if("To" %in% names(x$header)) x$header$"To" else NA)

    isR = sapply(msgs, function(x) if("Received" %in% names(x$header)) any(grepl("hypatia.math.ethz.ch", x$header[["Received"]])) else FALSE)

    when = mkDateTime(sapply(msgs, function(x) if("Date" %in% names(x$header)) x$header$"Date" else NA))

    cc = sapply(msgs, function(x) { i = tolower(names(x$header)) == "cc";  if(any(i)) paste(unlist(x$header[i]), collapse = ";") else NA})

    if(length(files) == 0)
        files = rep(as.character(NA), length(from))
    
    mm = data.frame(from = from, to = to, cc = cc, subject = subject, isR = isR, emailFrom = efrom, file = files, numAttachments = sapply(msgs, function(x) length(x$att)), stringsAsFactors = FALSE)
}

mkDateTime =
function(x)
{
    ans = strptime(x, "%a, %d %b %Y %H:%M:%S %z")
    w = is.na(ans)
    ans[w] = strptime(x[w], "%d %b %Y %H:%M:%S %z")
    ans
}

getContentType =
function(x)
{
    tmp = strsplit(x$header[["Content-Type"]], ";")[[1]]
    tmp = trimws(unique(unlist(tmp)))
    tmp = tmp[ !grepl("boundary|multipart", tmp) ]
   # grep("[^/]pdf", tmp, value = TRUE)
}


saveAttachment =
function(att, to = file.path(dir, getAttachmentName(att)), dir = ".")
{
    val = base64enc::base64decode(att$body)
     # Gradhub is my own package, not on CRAN.
    Gradhub::savePDF(val, to)
}

getAttachmentName =
function(att, header = att$header)
{
    type = strsplit(header[["Content-Type"]], ";")[[1]]
    m = gregexpr('name="[^"]+"', type)
    ans = regmatches(type, m)
    ans = ans[sapply(ans, length) > 0 ]
    gsub("\n", " ", gsub('name="([^"]+)"', "\\1", ans))
}

readMessages =
function(dir, files = list.files(recursive = TRUE,  full.names  = TRUE, ...), ...)
{
    msgs = lapply(files, readEmailMsg)
}



#############################





if(FALSE) {
    # shell:
    #   ag -rl 'S/U|grading option|petition|P/NP' 2020 > SU.2020
    
    source("~/GMail/readEML.R")
    eml = readLines("SU.2020")
    msgs = lapply(eml, readEmailMsg)

    mm = mkInfoDF(msgs)
    w = !mm$isR & grepl("ucdavis.edu", mm$emailFrom, ignore.case = TRUE)
    mm2 = mm[w, ]
    msgs2 = msgs[w]

    tmp = lapply(msgs2, function(x) sapply(x$att, function(x) strsplit(x$header[,"Content-Type"], ";")))
    tmp = trimws((unique(unlist(tmp))))
    tmp = tmp[ !grepl("boundary|multipart", tmp) ]
    grep("[^/]pdf", tmp, value = TRUE)
}

