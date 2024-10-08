% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ba_studies_observationunits.R
\name{ba_studies_observationunits}
\alias{ba_studies_observationunits}
\title{ba_studies_observationunits}
\usage{
ba_studies_observationunits(
  con = NULL,
  studyDbId = "",
  observationLevel = c("any", "plot", "plant"),
  pageSize = 1000,
  page = 0,
  rclass = c("tibble", "data.frame", "list", "json")
)
}
\arguments{
\item{con}{list, brapi connection object}

\item{studyDbId}{character, the internal database identifier for a study of
which the observation units are to be retrieved e.g. "1001";
\strong{REQUIRED ARGUMENT} with default: ""}

\item{observationLevel}{character, specifying the granularity level of
observation units, where either "plotNumber" or
"plantNumber" fields will be relavant depending on
whether granularity is specified as "plot" or "plant"
respectively; default: "any", other possible value:
"plot"/"plant"}

\item{pageSize}{integer, items per page to be returned; default: 1000}

\item{page}{integer, the requested page to be returned; default: 0 (1st page)}

\item{rclass}{character, class of the object to be returned;  default: "tibble"
, possible other values: "data.frame"/"list"/"json"}
}
\value{
An object of class as defined by rclass containing the observation
        units for a requested study.
}
\description{
Retrieve all the observation units within a specific study.
}
\details{
This call must have set a specific identifier. The default is an empty
         string. If not changed to an identifier present in the database this
         will result in an error.
}
\note{
Tested against: test-server, sweetpotatobase

BrAPI Version: 1.0, 1.1, 1.2

BrAPI Status: active

R Brapi Status: incomplete response parsing
}
\examples{
if (interactive()) {
  library(brapi)

  con <- ba_db()$testserver

  ba_studies_observationunits(con, studyDbId = "1001")
}
}
\references{
\href{https://github.com/plantbreeding/API/blob/V1.2/Specification/Studies/Studies_ObservationUnits_GET.md}{github}
}
\seealso{
Other studies: 
\code{\link{ba_phenotypes_search}()},
\code{\link{ba_studies_details}()},
\code{\link{ba_studies_germplasm_details}()},
\code{\link{ba_studies_layout}()},
\code{\link{ba_studies_observationlevels}()},
\code{\link{ba_studies_observations}()},
\code{\link{ba_studies_observationvariables}()},
\code{\link{ba_studies_search_post}()},
\code{\link{ba_studies_search}()},
\code{\link{ba_studies_seasons}()},
\code{\link{ba_studies_studytypes}()},
\code{\link{ba_studies_table_save}()},
\code{\link{ba_studies_table}()}

Other phenotyping: 
\code{\link{ba_phenotypes_search}()},
\code{\link{ba_samples_save}()},
\code{\link{ba_samples_search_post}()},
\code{\link{ba_samples_search}()},
\code{\link{ba_samples}()},
\code{\link{ba_studies_details}()},
\code{\link{ba_studies_layout}()},
\code{\link{ba_studies_observationlevels}()},
\code{\link{ba_studies_observations}()},
\code{\link{ba_studies_observationunits_save}()},
\code{\link{ba_studies_observationvariables}()},
\code{\link{ba_studies_search_post}()},
\code{\link{ba_studies_search}()},
\code{\link{ba_studies_seasons}()},
\code{\link{ba_studies_studytypes}()},
\code{\link{ba_studies_table_save}()},
\code{\link{ba_studies_table}()},
\code{\link{ba_trials_details}()},
\code{\link{ba_trials}()}
}
\author{
Reinhard Simon
}
\concept{phenotyping}
\concept{studies}
