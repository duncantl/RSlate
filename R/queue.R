getQueue =
function(con, url = "https://apply.grad.ucdavis.edu/manage/reader/search?mode=queue")
{
    params = "base=94c75ffa-e602-4656-855e-0ece8e605f7f&filter_search_type=filter&filter_search_id=fd4bdac3-2711-4cce-a4de-203c90513425&filter_search_operator=CONTAINS&filter_search_operand=&filter_bin_type=filter&filter_bin_id=1ba9c718-0c8e-4544-b0ba-b00e4c7bdc2b&filter_bin_operator=IN&filter_bin_operand=&preset=8ac1301f-6eda-45c1-a265-260de118ad70"

    tt = httpPOST(url, postfields = params,
                  httpheader = c('Content-Type' = "application/x-www-form-urlencoded"),
                  #XXXX absolutely need the referer.
                  referer = "https://apply.grad.ucdavis.edu/manage/reader/?r=%2fmanage%2freader&b=94c75ffa-e602-4656-855e-0ece8e605f7f",
                  curl = con)

    readSlateTable(tt)
}

