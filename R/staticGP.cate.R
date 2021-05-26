#' @title Get conditional average treatment effect
#' @description Estimate the conditional average treatment effect of user-specified treatment groups.
#' @details The contrast of potential outcomes for the reference group and the treatment group is estimated at each value of x.
#' @note The conditional average treatment effect is estimated based on the sample data. The observations with missing covariates in the model are excluded. For the unspecified variables in the model, the original data is used to estimate the conditional average treatment effect.
#' @param jobid job id of the "staticGP".
#' @param x The name of a categorical variable which may have the heterogeneous treatment effect.
#' @param control.tr The value of the treatment variable as the reference group.
#' @param treat.tr The value of the treatment variable compared to the reference group.
#' @param pr.values An optional vector of user-defined values of c for PrCTE.
#' @return
#' Return jobid
#' @export
#'
staticGP.cate <- function(
                          jobid,
                          x,
                          control.tr,
                          treat.tr,
                          pr.values=NULL,
                          token=NULL,
                          use.cache=F,
                          reuse.cached.jobid=F) {

  headers <- c()
  if (!is.null(token)) { headers<-c(headers, "Authorization"=paste("Bearer",token)) }
  if (!hasArg(use.cache) && Sys.getenv("PCATS_USE_CACHE")!="") use.cache<-Sys.getenv("PCATS_USE_CACHE")
  if (!is.null(use.cache) && (use.cache==T || use.cache=="1")) { headers<-c(headers, "X-API-Cache"="1") }
  if (!is.null(reuse.cached.jobid) && (reuse.cached.jobid==T || reuse.cached.jobid=="1")) { headers<-c(headers, "X-API-Reuse-Cached-Jobid"="1") }

  res <- POST(url=paste0('https://pcats.research.cchmc.org/api/job/',jobid,'/staticgp.cate'),
                  add_headers(headers),
                  encode='multipart',
                  body=list(jobid=jobid,
                            x=x,
                            control.tr=control.tr,
                            treat.tr=treat.tr,
                            pr.values=pr.values
                  ))
  cont <- content(res)
  jobid <- cont$jobid[[1]]
  jobid

}
