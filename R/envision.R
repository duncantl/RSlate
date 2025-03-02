pickByEnvision =
function(item, con = getConnection(), year = "2024")
{
    w = sapply(getField(item, "id"), isEnvision, con)
    if(any(w))
        return(item[w])

    item
}


isEnvision =
function(id, con = getConnection(), year = "2024")    
{
    doc = getPageById(id, con)
    length(getNodeSet(doc, sprintf("//text()[contains(., 'Envision %s')]", year)) > 0)
}

