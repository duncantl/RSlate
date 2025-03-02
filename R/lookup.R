#library(RCurl)
#library(RJSONIO)
#library(RSlate)

# https://apply.grad.ucdavis.edu/manage/service/lookup?type=&context=search&q=Hussin, Sara

lookup =
    #
function(name, url = "https://apply.grad.ucdavis.edu/manage/service/lookup",
             con = getConnection(), type = "ap")
{
    tt = getForm(url, .params = list(type = type, context = "search", q = name), curl = con)

    fromJSON(tt)$item
}

if(FALSE) {
    con = getConnection()
    o = lookup("Cambridge,", con = con)
    o[[1]]$id
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
