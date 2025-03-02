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

if(FALSE){
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
    ats = c("data-tab", "title", "id", "data-url", "data-href", "data-id")
    df = as.data.frame(lapply(ats, function(at) sapply(li, xmlGetAttr, at, NA)))
    names(df) = ats
    df$name = sapply(li, xmlValue)
    df[, c(ncol(df), 1:(ncol(df) - 1))]
}

getPageById =
function(id, con = getConnection())
{
    url = "https://apply.grad.ucdavis.edu/manage/lookup/record"
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
function(x, field = "name")
{
    ans = sapply( x, function(x) x[field])
    switch(field,
           name =  gsub("</?b>", "", ans),
           data = gsub("<br />", " ", ans),
           ans)
    

}


initial =
function(n)
{
   substring(trimws(sapply(strsplit(n, ", ?"), `[`, 2)), 1, 1)
}
