% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ba_locations_details.R
\name{ba_locations_details}
\alias{ba_locations_details}
\title{ba_locations_details}
\usage{
ba_locations_details(
  con = NULL,
  locationDbId = "",
  rclass = c("tibble", "data.frame", "list", "json")
)
}
\arguments{
\item{con}{list, brapi connection object}

\item{locationDbId}{character, the internal database identifier for a
location of which the details are to be retrieved;
\strong{REQUIRED ARGUMENT} with default: ""}

\item{rclass}{character, class of the object to be returned;  default: "tibble"
, possible other values: "json"/"list"/"data.frame"}
}
\value{
An object of class as defined by rclass containing the location
        details.
}
\description{
Gets details for a location given by a required database identifier.
}
\details{
All standard attributes are always listed. However, attributes in the additionalInfo
only when at least one record has data.
}
\note{
Tested against: test-server

BrAPI Version: 1.0, 1.1, 1.2

BrAPI Status: active
}
\examples{
if (interactive()) {
  library(brapi)
  # Need to connect to a database with genetic data

  con <- ba_db()$testserver

  loc <- ba_locations_details(con = con, "1")

}
}
\references{
\href{https://github.com/plantbreeding/API/blob/V1.2/Specification/Locations/LocationDetails.md}{github}
}
\author{
Reinhard Simon, Maikel Verouden
}
