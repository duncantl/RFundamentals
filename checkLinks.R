library(XML)
h = list.files(pattern = "html$")
h = h[ - match("Outline.html", h) ]
ll = sapply(h, getHTMLLinks)
doc = grep("^http", unique(unlist(ll)), invert = TRUE, value = TRUE)
anchors = grep("#", doc, value = TRUE)
doc = grep("#", doc, invert = TRUE, value = TRUE)
print(doc[ !file.exists(doc) ])
