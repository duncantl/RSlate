
You need to login into Slate via the browser and get the cookie.
I store it in the file named slate.cookie and the code will look for it there. However, you can
specify the cookie directly. You can also get it with the RBrowserCookies package.


```r
con = getConnection()
z  = statMSApps(con = con)
z  = byProgramDegree("Statistics", "M.S.")
```

We'll focus on the fully submitted applications, ignoring "Admit" and Awaiting Materials":
```r
submitted = z[z$"Current Workflow Bin" == "Faculty Members",]
dim(submitted)
```


### Download Student Packets

We'll store them in a directory named PDFs
```r
if(!file.exists("PDFs"))
    dir.create("PDFs")
```

We can do this directly with 
```r
files = mapply(function(u, out) try(getPDF(u, con, out)),  submitted$data.id, file.path("PDFs", paste0(trimws(submitted$Name), ".pdf")), SIMPLIFY = FALSE)
```

<!--
The less direct way  is to get the URL from the id and then download it:
```r
submitted$urls = sapply(submitted$data.id, function(id) try(getApplicantInfo(id, con)))
files = mapply(function(u, out) try(getPDF(u, con, out)),  submitted$urls, file.path("PDFs", paste0(trimws(submitted$Name), ".pdf")))
```
-->

## Get All Applicants

`allApplicants()` queries all applicants



## Applicants by Program and Degree

`byProgramDegree()` allows us to query one or more programs and one or more degree objectives
```r
tmp = byProgramDegree(c("Avian Sciences", "Statistics"), c("M.S.", "Ph.D."))
```

The names of the programs and degree objectives are available via
```r
names(programIds)
names(degreeObjectives)
```
