
#' @title Return job results
#' @description Return job results
#' @param jobid Job ID of the previously submitted job
#' @return results
#' @export
#' @import httr
#' @import utils
#'
results <- function(jobid) {
  output<-jsonlite::fromJSON(paste0("https://pcats.research.cchmc.org/api/job/",jobid,"/results"))
  output
}
