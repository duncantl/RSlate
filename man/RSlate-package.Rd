\name{RSlate-package}
\alias{RSlate-package}
\alias{RSlate}
\alias{pickByEnvision}
\alias{lookup}
\alias{programIds}
\alias{byProgramDegree}
\alias{allApplicants}
\alias{submittedApplications}
\alias{getProfile}
\alias{matchName}
\alias{statMSApps}
\alias{programApplications}
\alias{getConnection}
\alias{getField}
\alias{getPageById}
\alias{getQueue}
\alias{degreeObjectives}
\alias{getTabs}
\alias{getDecisions}
\alias{getPDF}
\alias{slate}
\alias{getPriorEd}

\docType{package}
\title{
\packageTitle{RSlate}
}
\description{
\packageDescription{RSlate}
}
\details{

  I use this package in 2 ways
  \enumerate{
    \item To get the students applying to a degree for which I am on the admission's committee
    and to download their PDF packet so I can review it locally.

    \item To query applicants as part of my Grad Studies roles, specifically when I need to look
     at more than one student at a time.
  }


  As with most other interfaces to Web systems, I need a cookie to make HTTP requests.
  I manually login into the Slate instance using my Web browser and then use the Developer Tools
  to extract the cookie. I then store this in \code{~/slate.cookie} or in the current directory
  and the functions will read it, as needed.  The cookie, however, will expire and I will repeat this.

  I typically create a connection to the Slate instance using \code{getConnection} and then pass
  this in all calls to functions that will make an HTTP request. This avoids having to reestablish
  the connection each time. This is not a big issue as Slate is quite quick.
  
  
    \code{getConnection} - make a reusable connection with the Slate HTTP server.

    \code{statMSApps} - the applicants to the Stat MS    
    \code{getQueue} - a data.frame of the applications in my queue
    \code{getPDF} - download the application PDF for a given id.
    
    \code{allApplicants} - a data.frame of "all" the applicants, i.e., that I can see.

    \code{degreeObjectives}    
    \code{byProgramDegree} - this looks like it doesn't work !!
    
    \code{lookup} - given a search query, e.g., the name of an applicant, get all the matches.

    \code{matchName} - after we use \code{lookup} to match a name, we
       might get multiple answers. This helps to filter them by matching the
      initial of the first name.
    
    \code{slate} - given an id, show in the user's Web browser


    
  
    \code{getTabs} - get a data.frame describing each tab on the applicant's landing page.
    
    \code{submittedApplications} - filter the tabs to only the submitted applications rows.
    
    \code{getDecisions} - using the result of \code{getTabs} and \code{submittedApplications},
       one can get the decisions on the submitted applications.


    \code{getPriorEd}  - given an id, get the prior institutions or all available elements including
              institution, major, GPA, location   and the year(s)


    \code{programApplications} - for a data.frame of application tasks and specifically its title
	       column, get the program and degree.

    
    \code{pickByEnvision} - very specific to the Envision program, determining whether the
    student's tabs include Envision. This does not check they attended Envision.
    See the timeline for that. (Not implemented yet.)

 Low-level functions for building others

    \code{getPageById} - get the HTML landing page  for a given applicant's id    
    
    \code{getProfile} - given an id, get the profile HTML page, needed for \code{getPriorEd}
    
    \code{getField} - get a specific field from all of the results returned by \code{lookup} for a
      given query.

      \code{programIds} - a character vector of ids for each program. This may be old.
       Each is of the form \code{id|program name}.
}
\author{
\packageAuthor{RSlate}

Maintainer: \packageMaintainer{RSlate}
}
\keyword{package}

%\seealso{}
%\examples{}
