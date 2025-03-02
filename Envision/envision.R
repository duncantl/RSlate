d = readxl(mostRecent("2024 Envision_FINAL_Attendees.xlsx"))
d$Name = paste(d$Last, d$First, sep = ", ")

ids = lapply(d$Name, lookup, con = con)
table(ni <- sapply(ids, function(x) length(x)))

# where ni is 0, use just the last name and , and see what we get.

ids[ni == 0] = lapply(paste0(d$Last[ni == 0], ","), lookup, con = con)    

table(ni <- sapply(ids, function(x) length(x)))

w = ni > 1
ids[w] = mapply(matchName, d$First[w], ids[w], SIMPLIFY = FALSE)

table(ni <- sapply(ids, function(x) length(x)))

w = ni > 1
ids[w] = lapply(ids[w], pickByEnvision, con)

table(sapply(ids, length))

stopifnot(all(sapply(ids, length) == 1))

# Some people applied to 2 or more programs.

d$slateId = sapply(ids, getField, "id")

tabs = lapply(d$slateId, getTabs, con = con)

env24 = sapply(tabs, function(x) "Envision 2024" %in% x$name)

d$numApps = sapply(tabs, function(x) nrow(submittedApplications(x)))

w = d$numApps > 0

des = lapply(tabs[w], getDecisions, con = con)

progInfo = lapply(tabs[w], function(x) lapply(submittedApplications(x)$title, programApplications))

v = paste0(c("program", "degree", "decision"), rep(1:2, each = 3))
d[v] = NA

d[w, v[c(1, 4)]] = lapply(1:2, \(i) sapply(progInfo, function(x) if(length(x) >= i) x[[i]]["prog"] else NA))
d[w, v[c(1, 4) + 1]] = lapply(1:2, \(i) sapply(progInfo, function(x) if(length(x) >= i) x[[i]]["deg"] else NA))        

d[w, v[c(1, 4) + 2]] = lapply(1:2, \(i) sapply(des, function(x) if(length(x) >= i) x[[i]][1] else NA))            

d[w, v]

d[, c("Name", "numApps", "program1", "degree1", "program2", "degree2")]
