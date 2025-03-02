submittedApplications =
function(x)
{
    w = !is.na(x$"datatab") & x$"datatab" ==  "Application" &
          grepl("2025-26", x$title) & !grepl("Awaiting Submission", x$name)
    x[w, ]
}


programApplications =
function(x)    
{
    x = gsub("^[0-9]{4}-[0-9]{2} ", "", x)

    c(prog = gsub("(.*),.*", "\\1", x), deg = gsub(".*, ", "", x))
}


getDecisions =
function(tab, apps = submittedApplications(tab), con = getConnection())
{
    lapply(apps$dataid, getDecision, con = con)
}


getDecision =
function(id, app, con = getConnection(), v2 = FALSE)    
{
    u = paste0(SlateURL, "/manage/lookup/application")
    doc = htmlParse(getForm(u, id = id, curl = con))

#  if(v2) {
#      ans = xpathSApply(doc, "//div[@class = 'dashboard']/div/div/text()", xmlValue)[[1]]
#      if(length(ans) == 0 || ans == "")
#          browser()
#      return(ans)
#  }
    

    tbl = getNodeSet(doc, sprintf("//div[@id = 'part_%s_decision']//table", id))

    if(length(tbl) == 0) {
        browser()
        return(NA)
    }

    ans = unique(xpathSApply(tbl[[1]], ".//tr/td[position() = 2]/div/text()", xmlValue))
    if(length(ans) == 0)
        # "Awaiting program decision"
        xpathSApply(doc, "//div[@class = 'dashboard']/div/div/text()", xmlValue)[[1]]
    else
        ans
}

