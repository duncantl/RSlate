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

