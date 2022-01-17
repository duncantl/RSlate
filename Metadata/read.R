library(XML)
doc = htmlParse("gradPrograms.html")
opts = getNodeSet(doc, "//option[@value]")
# gsub("\\|.*", "", sapply(opts, xmlGetAttr, "value"))
programIds = structure(sapply(opts, xmlGetAttr, "value"),  names = sapply(opts, xmlValue))


doc = htmlParse("degreeObjective.html")
opts = getNodeSet(doc, "//option[@value]")
# gsub("\\|.*", "", sapply(opts, xmlGetAttr, "value"))
degreeObjectives = structure(sapply(opts, xmlGetAttr, "value"),  names = sapply(opts, xmlValue))

kon = file("../R/metadata.R", "w")
cat("programIds = \n", file = kon)
dput(programIds, kon)
cat("\n\n\ndegreeObjectives = \n", file = kon)
dput(degreeObjectives, kon)
close(kon)

