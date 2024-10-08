% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ba_observationvariables_details.R
\name{ba_observationvariables_details}
\alias{ba_observationvariables_details}
\title{ba_observationvariables_details}
\usage{
ba_observationvariables_details(
  con = NULL,
  observationVariableDbId = "",
  rclass = c("tibble", "data.frame", "list", "json")
)
}
\arguments{
\item{con}{list, brapi connection object}

\item{observationVariableDbId}{character, the internal database identifier for
an observation variable of which the details
are to be retrieved e.g. "MO_123:100002";
\strong{REQUIRED ARGUMENT} with default: ""}

\item{rclass}{character, class of the object to be returned; default:
"tibble", possible other values: "data.frame"/"list"/"json"}
}
\value{
An object of class as defined by rclass containing the variable
        details the requested observation variable identifier on the BrAPI
        compliant database server.
}
\description{
Retrieve variable details on a BrAPI compliant database server.
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

  ba_observationvariables_details(con)

}
}
\references{
\href{https://github.com/plantbreeding/API/blob/V1.2/Specification/ObservationVariables/VariableDetails.md}{github}
}
\seealso{
Other observationvariables: 
\code{\link{ba_observationvariables_datatypes}()},
\code{\link{ba_observationvariables_ontologies}()},
\code{\link{ba_observationvariables}()}

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
\code{\link{ba_observationvariables_ontologies}()},
\code{\link{ba_observationvariables}()},
\code{\link{ba_phenotypes_search_post}()},
\code{\link{ba_programs_search_post}()},
\code{\link{ba_programs}()},
\code{\link{ba_studies_germplasm_details}()},
\code{\link{ba_traits_details}()},
\code{\link{ba_traits}()}
}
\author{
Reinhard Simon, Maikel Verouden
}
\concept{brapicore}
\concept{observationvariables}
