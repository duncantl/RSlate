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

