% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ba_connect.R
\name{ba_connect}
\alias{ba_connect}
\title{ba_connect}
\usage{
ba_connect(
  brapiDb = NULL,
  secure = FALSE,
  protocol = "http://",
  db = "127.0.0.1",
  port = 2021,
  apipath = NULL,
  multicrop = FALSE,
  crop = "sweetpotato",
  user = "",
  password = "",
  token = "",
  granttype = "password",
  clientid = "rbrapi",
  bms = FALSE
)
}
\arguments{
\item{brapiDb}{brapiDb R object for brapi databases created by as.ba_db [default: NULL]}

\item{secure}{logical TRUE/FALSE}

\item{protocol}{string; default: http:// (else: https://)}

\item{db}{string; default '127.0.0.1' (localhost)}

\item{port}{integer; default: 80}

\item{apipath}{character; default NULL}

\item{multicrop}{logical; default FALSE}

\item{crop}{string; default: '' (empty)}

\item{user}{string; default: '' (empty)}

\item{password}{string; default: '' (empty)}

\item{token}{string; default: '' (empty)}

\item{granttype}{string}

\item{clientid}{string: default: rbrapi}

\item{bms}{logical; default: FALSE}
}
\value{
connection object
}
\description{
Sets a global list variable 'brapi' that contains all parameters to connect to
a BrAPI compliant online database.
}
\details{
If the brapi_db parameter is used it will override the other parameters; except user, password and session.
}
\examples{
if (interactive()) {
  library(brapi)

  con <- ba_db()$testserver

  con <- ba_connect(con)
}
}
\seealso{
Other brapiutils: 
\code{\link{as.ba_db}()},
\code{\link{ba_can_internet}()},
\code{\link{ba_check}()},
\code{\link{ba_db}()},
\code{\link{ba_login}()},
\code{\link{ba_logout}()},
\code{\link{is.ba_con}()},
\code{\link{print.ba_con}()},
\code{\link{print.ba_db_list}()}
}
\author{
Reinhard Simon, Maikel Verouden
}
\concept{brapiutils}
