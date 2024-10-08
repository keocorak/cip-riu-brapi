% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ba_studies_layout.R
\name{ba_studies_layout}
\alias{ba_studies_layout}
\title{ba_studies_layout}
\usage{
ba_studies_layout(
  con = NULL,
  studyDbId = "",
  pageSize = 1000,
  page = 0,
  rclass = c("tibble", "data.frame", "list", "json")
)
}
\arguments{
\item{con}{list, brapi connection object}

\item{studyDbId}{character, the internal database identifier for a study of
which the plot layout is to be retrieved e.g. "1001";
\strong{REQUIRED ARGUMENT} with default: ""}

\item{pageSize}{integer, items per page to be returned; default: 1000}

\item{page}{integer, the requested page to be returned; default: 0 (1st page)}

\item{rclass}{character, class of the object to be returned;  default: "tibble"
, possible other values: "json"/"list"/"data.frame"}
}
\value{
An object of class as defined by rclass containing the plot layout
        for a requested study.
}
\description{
Retrieve the plot layout of a specific study available on a brapi server.
}
\details{
This call must have set a specific identifier (studyDbId). The
         default is an empty string. If not changed to an identifier present
         in the database this will result in an error.
}
\note{
Tested against: sweetpotatobase, test-server

BrAPI Version: 1.0, 1.1, 1.2

BrAPI Status: active
}
\examples{
if (interactive()) {
  library(brapi)

  con <- ba_db()$testserver

  ba_studies_layout(con, studyDbId = "1001")
}
}
\references{
\href{https://github.com/plantbreeding/API/blob/V1.2/Specification/Studies/Studies_Layout_GET.md}{github}
}
\seealso{
Other studies: 
\code{\link{ba_phenotypes_search}()},
\code{\link{ba_studies_details}()},
\code{\link{ba_studies_germplasm_details}()},
\code{\link{ba_studies_observationlevels}()},
\code{\link{ba_studies_observations}()},
\code{\link{ba_studies_observationunits}()},
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
\code{\link{ba_studies_observationlevels}()},
\code{\link{ba_studies_observations}()},
\code{\link{ba_studies_observationunits_save}()},
\code{\link{ba_studies_observationunits}()},
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
Reinhard Simon, Maikel Verouden
}
\concept{phenotyping}
\concept{studies}
