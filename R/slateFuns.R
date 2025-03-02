getByName =
    # how does his relate to lookup()?
function(name, con, id = character(), process = length(id) > 0,
          params = getFormParams("base=94c75ffa-e602-4656-855e-0ece8e605f7f&filter_search_type=filter&filter_search_id=fd4bdac3-2711-4cce-a4de-203c90513425&filter_search_operator=CONTAINS&filter_search_operand=Joshi+Ansh&filter_bin_type=filter&filter_bin_id=1ba9c718-0c8e-4544-b0ba-b00e4c7bdc2b&filter_bin_operator=IN&filter_bin_operand=&preset=8ac1301f-6eda-45c1-a265-260de118ad70"),
         url = "https://apply.grad.ucdavis.edu/manage/reader/search?mode=apps&offset=0&limit=250")
{
    p = params
    p["filter_search_operand"] = name
    body = paste(names(p), p, sep = "=", collapse = "&")
    ans = httpPOST(url, postfields = body, httpheader = c('content-type' = "application/x-www-form-urlencoded"), curl = con)

    if(process)
        getInfo(ans, id)
    else
        ans
}

#
getByID =
function(id, con,  url = "https://apply.grad.ucdavis.edu/manage/reader/search?mode=apps&offset=0&limit=250")
{
   body = sprintf("base=94c75ffa-e602-4656-855e-0ece8e605f7f&filter_search_type=filter&filter_search_id=fd4bdac3-2711-4cce-a4de-203c90513425&filter_search_operator=CONTAINS&filter_search_operand=%s&filter_bin_type=filter&filter_bin_id=1ba9c718-0c8e-4544-b0ba-b00e4c7bdc2b&filter_bin_operator=IN&filter_bin_operand=&preset=8ac1301f-6eda-45c1-a265-260de118ad70", id)
    ans = httpPOST(url, postfields = body, httpheader = c('content-type' = "application/x-www-form-urlencoded"), curl = con)
    getInfo(ans, id)
}




getInfo =
function(tt, id = character(), name = character())
{
    ans = readSlateTable(tt)
    if(length(id)) {
        ans = ans[ ans$'UCD ID#' == id,]
    }
    ans
}

readSlateTable =
function(tt)    
{
    doc = htmlParse(tt)
    ans = readHTMLTable(doc, which = 1)
    ans$studentName = xpathSApply(doc, "//table//tr/td[1]/div[@class = 'name']", xmlValue)
    ans$data.id = xpathSApply(doc, "//tbody/tr", xmlGetAttr, "data-id", NA)
    ans
}


