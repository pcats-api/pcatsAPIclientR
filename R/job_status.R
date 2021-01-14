
#' @title Return job status
#' @description Return status of the previously submitted job
#' @param jobid Job ID of the previously submitted job
#' @return status
#' @export
#' @import httr
#' @import utils
#'
job_status <- function(jobid) {
  if (is.null(jobid)) {
    stop("jobid is null")
  }
  status<-jsonlite::fromJSON(paste0("https://pcats.research.cchmc.org/api/job/",jobid,"/status"))
  status
}
