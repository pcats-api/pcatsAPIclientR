
#' @title Return plot URL
#' @description Return plot URL
#' @param jobid Job ID of the previously submitted job
#' @param plottype Plot type
#' @return url
#' @export
#' @import httr
#' @import utils
#'
ploturl <- function(jobid, plottype="") {
  if (plottype!="") { plottype<-paste0("/",plottype) }
  url<-jsonlite::fromJSON(paste0("https://pcats.research.cchmc.org/api/job/",jobid,"/ploturl"))
  output<-paste0(url$url,plottype)
  output
}
