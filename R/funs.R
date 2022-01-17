nonce =
function()
    paste0(as.integer(Sys.time()), "000")


getApplicantInfo =
    #
    # given an applicant's id, get the link to the PDF
    #
function(id, con = getConnection(), base = getSlateBase(), baseURL = "https://apply.grad.ucdavis.edu/manage/reader/")
{
    rw = getForm(baseURL, cmd = "lookup", id = id, base = base, "_" = nonce(), binary = TRUE, curl = con)
    doc = htmlParse(rawToChar(rw))
    links = getHTMLLinks(doc, baseURL = baseURL, relative = TRUE)
    links["Download PDF"]
}

getPDF =
function(url, con = getConnection(), out = sprintf("%s.pdf", id), id = getFormParams(url)["id"])
{
    if(!grepl("^http", url)) {
        if(!grepl("^[-0-9-f]+$", url))
            stop("Neither a URL or an applicant data.id")

        url = getApplicantInfo(url, con)
    }
    
    content = getURLContent(url, curl = con, binary = TRUE)
    if(length(out) && length(content)) {
        Gradhub::savePDF(content, out)
        out
    } else
        content
}

getConnection =
function(cooky = cookie("slate.cookie"), ...)
   getCurlHandle(cookie = cooky, followlocation = TRUE, cookiejar = '', ...)



# These param values are almost definitely specific to UC Davis.

allApplicants =
    #
    # get all of the applicants that have submitted their application (not just started, etc.)
    #
function(limit = 3000L, offset = 0L,
         con = getConnection(...),
         url = sprintf("https://apply.grad.ucdavis.edu/manage/reader/search?mode=apps&offset=%d&limit=%d", offset, limit),
         base = getSlateBase(), preset = getSlatePreset(),
         params = c(filter_search_type = "filter",
                    filter_search_id = "fd4bdac3-2711-4cce-a4de-203c90513425", 
                    filter_search_operator = "CONTAINS",
                    filter_search_operand = "", 
                    filter_bin_type = "filter",
                    filter_bin_id = "1ba9c718-0c8e-4544-b0ba-b00e4c7bdc2b", 
                    filter_bin_operator = "IN",
                    filter_bin_operand = "33bf2810-6890-425c-8030-035e491f6eff|Evaluation+-+Faculty+Members"),
         ...)
{
    params["base"] = base
    params["preset"] = getSlatePreset()
    paramStr = mkParamString(params)
    tt2 = getURLContent(url, customrequest = "POST", postfields = paramStr, curl = con,
                        referer = sprintf("https://apply.grad.ucdavis.edu/manage/reader/?r=/manage/reader&b=%s&tab=browse", base))

    doc = htmlParse(tt2)
    tbl = getNodeSet(doc, "//table")[[1]]
    ans = readHTMLTable(tbl)
    ans$data.id = xpathSApply(doc, "//tbody/tr", xmlGetAttr, "data-id", NA)

    ans$gpa = as.numeric(ans$"Verified UG GPA")
    ans$grad.gpa = as.numeric(ans$"Verified Grad GPA")
    
    ans
}

mkParamString =
function(params)
    paste( names(params), as.character(params), sep = "=", collapse = "&")



statMSApps =
function(...)    
{
   params = c(filter_search_type = "filter", 
           filter_search_id = "fd4bdac3-2711-4cce-a4de-203c90513425", filter_search_operator = "CONTAINS", 
           filter_search_operand = "", filter_bin_type = "filter", filter_bin_id = "1ba9c718-0c8e-4544-b0ba-b00e4c7bdc2b", 
           filter_bin_operator = "IN", filter_bin_operand = "", 
           `filter_46da58c3-021b-4aa6-9505-e6bba938fb17_active` = "1", `filter_46da58c3-021b-4aa6-9505-e6bba938fb17_type` = "filter", 
           `filter_46da58c3-021b-4aa6-9505-e6bba938fb17_name` = "Graduate+Program", 
           `filter_46da58c3-021b-4aa6-9505-e6bba938fb17_id` = "e0960eab-3137-4533-a8a7-066497dbb470", 
           `filter_46da58c3-021b-4aa6-9505-e6bba938fb17_source_type` = "local", 
           `filter_46da58c3-021b-4aa6-9505-e6bba938fb17_pinned` = "0", `filter_46da58c3-021b-4aa6-9505-e6bba938fb17_operandName` = "", 
           `filter_46da58c3-021b-4aa6-9505-e6bba938fb17_operator` = "IN", 
           `filter_46da58c3-021b-4aa6-9505-e6bba938fb17_operand` = "ae0a5c9a-b272-4c55-989a-bae55a435926%7CStatistics", 
           `filter_e882181c-a259-4154-bab0-60a33fbe435e_active` = "1", `filter_e882181c-a259-4154-bab0-60a33fbe435e_type` = "filter", 
           `filter_e882181c-a259-4154-bab0-60a33fbe435e_name` = "Degree+Objective", 
           `filter_e882181c-a259-4154-bab0-60a33fbe435e_id` = "4bf6957a-a18a-48c2-81b1-1a10c62ad677", 
           `filter_e882181c-a259-4154-bab0-60a33fbe435e_source_type` = "local", 
           `filter_e882181c-a259-4154-bab0-60a33fbe435e_pinned` = "0", `filter_e882181c-a259-4154-bab0-60a33fbe435e_operandName` = "", 
           `filter_e882181c-a259-4154-bab0-60a33fbe435e_operator` = "IN", 
           `filter_e882181c-a259-4154-bab0-60a33fbe435e_operand` = "1955aef8-b272-4a87-bb6b-cb41a89c1768%7CM.S.")

   allApplicants(params  = params, ...)
}


# We put the value of the base and preset form parameters in R options and not in the code
# as a) they probably change per person/Slate installation, and b) they may provide "secret" information.
# So just to be safe.
#
#  Set them as
#     options(Slate.base = "...", Slate.preset = "...")
#

getSlateBase =
function()
    getSlateOpt("base")

getSlatePreset =
function()
    getSlateOpt("preset")

getSlateOpt =
function(var)    
{
    opt = paste0("Slate.", var)
    getOption(opt, stop("No option set for ", opt))
}

    
