#' ba_seedlots_details
#'
#' Gets seedlots details.
#'
#' @param con list, brapi connection object
#' @param rclass character, class of the object to be returned;  default: "tibble"
#'               , possible other values: "data.frame"/"list"/"json"
#' @param pageSize integer, items per page to be returned; default: 1000
#' @param page integer, the requested page to be returned; default: 0 (1st page)
#'
#' @return An object of class as defined by rclass containing the germplasm
#'         details.
#'
#' @note Tested against: sweetpotatobase, test-server, genesys
#' @note BrAPI Version: 1.1, 1.2
#' @note BrAPI Status: active
#'
#' @author Reinhard Simon, Maikel Verouden
#' @references \href{https://github.com/plantbreeding/API/blob/V1.2/Specification/Germplasm/Germplasm_GET.md}{github}
#'
#' @family germplasm
#' @family brapicore
#'
#' @example inst/examples/ex-ba_germplasm_details.R
#'
#' @import dplyr
#' @export

ba_seedlots_details <- function(con = NULL,
                                crossName=NULL,
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

  callurl <- get_brapi(con = con) %>% paste0("seedlots?crossName=", crossName, "&page=", page, "&pageSize=", pageSize)

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
    class(out) <- c(class(out), "ba_seedlots_details")

    show_metadata(resp)
    return(out)
  })
}
