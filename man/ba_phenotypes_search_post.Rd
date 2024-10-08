% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ba_phenotypes_search_post.R
\name{ba_phenotypes_search_post}
\alias{ba_phenotypes_search_post}
\title{ba_phenotypes_search_post}
\usage{
ba_phenotypes_search_post(
  con = NULL,
  germplasmDbIds = "",
  observationVariableDbIds = "",
  studyDbIds = "",
  locationDbIds = "",
  trialDbIds = "",
  programDbIds = "",
  seasonDbIds = "",
  observationLevel = "",
  observationTimeStampRangeStart = "",
  observationTimeStampRangeEnd = "",
  pageSize = 1000,
  page = 0,
  rclass = c("tibble", "data.frame", "list", "json")
)
}
\arguments{
\item{con}{brapi list, connection object}

\item{germplasmDbIds}{vector of character; default: ''}

\item{observationVariableDbIds}{vector of character; default: ''}

\item{studyDbIds}{vector of character; default: ''}

\item{locationDbIds}{vector of character; default: ''}

\item{trialDbIds}{vector of character; default: ''}

\item{programDbIds}{vector of character; default: ''}

\item{seasonDbIds}{vector of character; default: ''}

\item{observationLevel}{character; default: ''}

\item{observationTimeStampRangeStart}{character; default: ''}

\item{observationTimeStampRangeEnd}{character; default: ''}

\item{pageSize}{integer default: 1000}

\item{page}{integer default: 0}

\item{rclass}{character; default: tibble}
}
\value{
rclass
}
\description{
lists the breeding observationvariables
}
\examples{
if (interactive()) {
  library(brapi)

  con <- ba_db()$testserver

  ba_phenotypes_search_post(con)
}
}
\references{
\href{https://github.com/plantbreeding/API/blob/V1.2/Specification/Phenotypes/PhenotypesSearch_GET.md}{github}
}
\seealso{
Other brapicore: 
\code{\link{ba_calls}()},
\code{\link{ba_commoncropnames}()},
\code{\link{ba_crops}()},
\code{\link{ba_crosses_study}()},
\code{\link{ba_germplasm_details_study}()},
\code{\link{ba_germplasm_details}()},
\code{\link{ba_germplasm_pedigree}()},
\code{\link{ba_germplasm_progeny}()},
\code{\link{ba_germplasm_search_post}()},
\code{\link{ba_germplasm_search}()},
\code{\link{ba_observationvariables_datatypes}()},
\code{\link{ba_observationvariables_details}()},
\code{\link{ba_observationvariables_ontologies}()},
\code{\link{ba_observationvariables}()},
\code{\link{ba_programs_search_post}()},
\code{\link{ba_programs}()},
\code{\link{ba_studies_germplasm_details}()},
\code{\link{ba_traits_details}()},
\code{\link{ba_traits}()}
}
\author{
Reinhard Simon
}
\concept{brapicore}
