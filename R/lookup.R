library(RCurl)
library(RJSONIO)
library(RSlate)

# https://apply.grad.ucdavis.edu/manage/service/lookup?type=&context=search&q=Hussin, Sara
lookup =
function(name, url = "https://apply.grad.ucdavis.edu/manage/service/lookup",
             con = getConnection(), type = "ap")
{
    tt = getForm(url, .params = list(type = type, context = "search", q = name), curl = con)

    fromJSON(tt)$item
}

if(FALSE) {
    con = getConnection(verbose = FALSE, useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:135.0) Gecko/20100101 Firefox/135.0")

    o = lookup("Cambridge, Anaya", con = con)

    o[[1]]$id
}


submittedApplications =
function(x)
{
    w = !is.na(x$"datatab") & x$"datatab" ==  "Application" &
          grepl("2025-26", x$title) & !grepl("Awaiting Submission", x$name)
    x[w, ]
}

matchName =
function(who, items)
{
    snames = getField(items)
    init = initial(snames)

    s = substring(who, 1, 1)

    w = s == init

    if(sum(w) == 1)
        return(items[which(w)])

    items
}

pickByEnvision =
function(item, con = getConnection())
{
    w = sapply(getField(item, "id"), isEnvision, con)
    if(any(w))
        return(item[w])

    item
}

programApplications =
function(x)    
{
    x = gsub("^[0-9]{4}-[0-9]{2} ", "", x)
    c(prog = gsub(", [^,]+", "", x), deg = gsub(".*, ", "", x))
}

isEnvision =
function(id, con = getConnection())    
{
    doc = getPageById(id, con)
    length(getNodeSet(doc, "//text()[contains(., 'Envision 2024')]")) > 0
}


getTabs =
function(id, doc = getPageById(id, con), con = getConnection())    
{
    li = getNodeSet(doc, "//ul[@class = 'tabs']//li/a")
    ats = c("data-tab", "title", "id", "data-url", "data-href", "data-id", "data-div")
    df = as.data.frame(lapply(ats, function(at) sapply(li, xmlGetAttr, at, NA)))
    names(df) = gsub("-", "", ats)
    df$name = sapply(li, xmlValue)
    df[, c(ncol(df), 1:(ncol(df) - 1))]
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

SlateURL = "https://apply.grad.ucdavis.edu"
RecordURL = "https://apply.grad.ucdavis.edu/manage/lookup/record"

slate =
function(id)    
{
    browseURL(paste0(RecordURL, "?id=", id))
}

getPageById =
function(id, con = getConnection())
{
    url = RecordURL
     # 9ee3e0ca-0f48-444a-9b87-3a006592b411
    htmlParse(getForm(url, id = id, curl = con))
}

pickByData =
function(x)
{
    d = getField(x, "data")

    if(any(w <- grepl("UCD ID:", d)))
        return(x[ w ])

    x
}


getField =
    # from lookup, we get a list of results.
    # Each of this is a character vector with fields id, data, name, col
    # So we loop over the results (x) and retreive field from each.
function(x, field = "name")
{
    ans = sapply( x, function(x) x[field])
    switch(field,
           name =  gsub("</?b>", "", ans),
           data = gsub("<br />", " ", ans),
           ans)
}


initial =
    # Get a person's first name initial. e.g., Duncan to D
function(n)
{
   substring(trimws(sapply(strsplit(n, ", ?"), `[`, 2)), 1, 1)
}
