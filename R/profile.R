getPriorEd =
function(id, con = getConnection(), doc = getProfile(id, con), universityOnly = TRUE)    
{
    p = getNodeSet(doc, "//h2[. = 'Academic History']/following-sibling::div[position() = 1]/p")
    if(length(p) == 0)
        return(NULL)

    # If we just want the insitution, do this.
    if(universityOnly)
        return(sapply(p, function(x) xmlValue(x[[1]])))

    # Problems here if no major included for some. So need to make this of length 8 and fill in the 2.
    # Do them individually and then rbind.
    
    ans = t(sapply(p, function(x) xmlSApply(x, xmlValue)))
    
    # drop is very important here when we have only 1 row/1 p.
    # Otherwise, the as.data.frame will collapse to a single column
    # Put this in the book <<<<<<<<<<<<<<<<
    ans = ans[,colnames(ans) != "br", drop = FALSE]
    # Do this before converting to a data.frame.
    ans = gsub("^, *", "", ans)
    ans = as.data.frame(ans)
    names(ans) = c("Institution", "Degree", "Major", "GPA", "Location")
    ans
}

getProfile =
function(id, con = getConnection())    
{
    u = paste0(SlateURL, "/manage/lookup/record")
    doc = htmlParse(getForm(u, id = id, cmd = "profile", curl = con))
}

