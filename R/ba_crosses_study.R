#' ba_crosses_study
#'
#' Gets cross details for a specific study available on a brapi server.
#'
#' @param con list, brapi connection object
#' @param crossingProjectDbId character, the internal database identifier for a study of
#'                  which the cross details are to be retrieved e.g. "1001";
#'                  \strong{REQUIRED ARGUMENT} with default: ""
#' @param pageSize integer, items per page to be returned; default: 1000
#' @param page integer, the requested page to be returned; default: 0 (1st page)
#' @param rclass character, class of the object to be returned;  default: "tibble"
#'               , possible other values: "json"/"list"/"data.frame"
#'
#' @return An object of class as defined by rclass containing the cross
#'         details for a requested study.
#'
#' @note Tested against: sweetpotatobase, test-server
#' @note BrAPI Version: 1.0
#' @note BrAPI Status: deprecated
#'
#' @author Reinhard Simon
#' @references
#' @family germplasm
#' @family brapicore
#' @example
#' @import tibble
#' @export

ba_crosses_study <- function(con = NULL,
                             crossingProjectDbId = "",
                             pageSize = 1000,
                             page = 0,
                             rclass = c(
                               "tibble", "data.frame",
                               "list", "json"
                             )) {
  # ba_check(con = con, verbose = FALSE, brapi_calls = "germplasm/id")
  # check_character(germplasmDbId)
  # check_req(germplasmDbId)
  rclass <- match.arg(rclass)

  callurl <- get_brapi(con = con) %>% paste0(
    "crosses?crossingProjectDbId=", crossingProjectDbId,
    "&page=", page, "&pageSize=", pageSize
  )

  try({
    resp <- brapiGET(url = callurl, con = con)
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    out <- NULL
    if (rclass %in% c("json", "list")) {
      out <- dat2tbl(res = cont, rclass = rclass)
    }
    if (rclass == "data.frame") {
      out <- gp2tbl(cont)
    }
    if (rclass == "tibble") {
      out <- gp2tbl(cont) %>% tibble::as_tibble(validate = FALSE)
    }
    class(out) <- c(class(out), "ba_germplasm_details")

    show_metadata(resp)
    return(out)
  })
}
