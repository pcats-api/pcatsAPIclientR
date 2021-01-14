#' @title Get conditional average treatment effect
#' @description Estimate the conditional average treatment effect of user-specified treatment groups.
#' @details The contrast of potential outcomes for the reference group and the treatment group is estimated at each value of x.
#' @note The conditional average treatment effect is estimated based on the sample data. The observations with missing covariates in the model are excluded. For the unspecified variables in the model, the original data is used to estimate the conditional average treatment effect.
#' @param jobid job id of the "staticGP".
#' @param x The name of a categorical variable which may have the heterogeneous treatment effect.
#' @param control.tr The value of the treatment variable as the reference group.
#' @param treat.tr The value of the treatment variable compared to the reference group.
#' @param pr.values pr.values
#' @return
#' Return an object of class "cate.effect" which contains a list of the following components:
#'  \item{out}{A data frame containing the posterior samples of the estimated conditional average treatment effect.}
#'  \item{hte.est}{The statistical summary results of the conditional average treatment effect.}
#' @export
#'
staticGP.cate <- function(jobid,x,control.tr,treat.tr,pr.values=NULL) {

  res <- POST(url=paste0('https://pcats.research.cchmc.org/api/job/',jobid,'/staticgp.cate'),
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
