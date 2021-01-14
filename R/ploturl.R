
#' @title Return plot URL
#' @description Return plot URL
#' @param jobid Job ID of the previously submitted job
#' @return url
#' @export
#' @import httr
#' @import utils
#'
ploturl <- function(jobid, type="") {
  output<-paste(readLines(paste0("https://pcats.research.cchmc.org/api/job/",jobid,"/ploturl")),"\n")
  output
}
