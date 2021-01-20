
#' @title Return plot URL
#' @description Return plot URL
#' @param jobid Job ID of the previously submitted job
#' @param type Plot type
#' @return url
#' @export
#' @import httr
#' @import utils
#'
ploturl <- function(jobid, type="") {
  if (type!="") { type<-paste0("/",type) }
  output<-paste(readLines(paste0("https://pcats.research.cchmc.org/api/job/",jobid,"/ploturl",type)),"\n")
  output
}
