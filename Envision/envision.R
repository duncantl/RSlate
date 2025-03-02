library(RSlate)

con = getConnection()

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

# So now we have matched the names in the spreadsheet to the envision attendees.
# Next time, let's ask for their Ids directly.

# Note that some people applied to 2 or more programs.

d$slateId = sapply(ids, getField, "id")

# Landing page tab information which includes the standard Dashboard, Timeline, ...
# but importantly all their applications - whether submitted or not.
tabs = lapply(d$slateId, getTabs, con = con)

# Verify that Envision 2024 is in all of the people's tabs. If not, we have got the wrong people.
env24 = sapply(tabs, function(x) "Envision 2024" %in% x$name)
stopifnot(all(env24))

# now determine the number of applications each had - ranging from 0 to ...  2 in this case.
d$numApps = sapply(tabs, function(x) nrow(submittedApplications(x)))

w = d$numApps > 0

# Get the decisions for those who submitted 1 or more applications
des = lapply(tabs[w], getDecisions, con = con)

# Get information about each program.
progInfo = lapply(tabs[w], function(x) lapply(submittedApplications(x)$title, programApplications))

# Now put this information for up to 2 applications into the data.frame with NAs for everyone
# unless they had an application.
v = paste0(c("program", "degree", "decision"), rep(1:2, each = 3))
d[v] = NA

# program
d[w, v[c(1, 4)]] = lapply(1:2, \(i) sapply(progInfo, function(x) if(length(x) >= i) x[[i]]["prog"] else NA))
# degree type
d[w, v[c(1, 4) + 1]] = lapply(1:2, \(i) sapply(progInfo, function(x) if(length(x) >= i) x[[i]]["deg"] else NA))
# decision
d[w, v[c(1, 4) + 2]] = lapply(1:2, \(i) sapply(des, function(x) if(length(x) >= i) x[[i]][1] else NA))            

d$decision1[w] = sapply(des, function(x) x[[1]][1])
d$decision2[w] = sapply(des, function(x) if(length(x) > 1) x[[2]][1] else NA)


d$priorInstitutions = sapply(d$slateId, function(x) paste(getPriorEd(x, con = con, universityOnly = TRUE), collapse = "; "))

d[, c("Name", "numApps", v)]
