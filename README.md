
You need to login into Slate via the browser and get the cookie.
I store it in the file named slate.cookie and the code will look for it there. However, you can
specify the cookie directly. You can also get it with the RBrowserCookies package.


```r
con = getConnection()
z  = statMSApps(con = con)
options(width = 200)

# ignoring "Admit" and Awaiting Materials"
submitted = z[z$"Current Workflow Bin" == "Faculty Members",]
dim(submitted)

submitted$urls = sapply(submitted$data.id, function(id) try(getApplicantInfo(id, con)))

table(grepl("https", submitted$urls))
err = sapply(files, is, 'try-error')

table(err)

if(!file.exists("PDFs"))
    dir.create("PDFs")

# Download the application packets.
files = mapply(function(u, out) try(getPDF(u, con, out)),  submitted$urls, file.path("PDFs", paste0(trimws(submitted$Name), ".pdf")))
```


`allApplicants()` queries all applicants


`byProgramDegree()` allows us to query one or more programs and one or more degree objectives
```r
tmp = byProgramDegree(c("Avian Sciences", "Statistics"), c("M.S.", "Ph.D."))
```

The names of the programs and degree objectives are available via
```r
names(programIds)
names(degreeObjectives)
```
