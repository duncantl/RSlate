slate =
function(id)    
{
    browseURL(paste0(RecordURL, "?id=", id))
}


getPageById =
    # Get the landing page from an applicant's id.
function(id, con = getConnection())
{
    url = RecordURL
    htmlParse(getForm(url, id = id, curl = con))
}
