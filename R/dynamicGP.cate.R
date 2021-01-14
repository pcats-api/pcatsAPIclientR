#' @title Get conditional average treatment effect for heterogeneous computation data with two time points.
#' @description Estimate the conditional average treatment effect of user-specified treatment groups.
#' @details The contrast of potential outcomes for the reference group and the treatment group is estimated at a list of x values if x is not a factor. If x is a factor, the conditional average treatment effect is estimated at each value of levels of x.
#' @note The conditional average treatment effect is estimated based on the sample data. The observations with missing covariates in the model are excluded. For the unspecified variables in the model, the observed data is used to estimate the conditional average treatment effect.
#' @param jobid job id of the "dynamicGP".
#' @param x The name of variable which may have the heterogeneous treatment effect. x should be a categorical variable.
#' @param control.tr A vector of the values of the treatment variables at all stages as the reference group.
#' @param treat.tr A vector of the values of the treatment variables at all stages compared to the reference group.
#' @return Return an object of class "cate.effect" which contains a list of the following components:
#'   \item{out}{A data frame containing the posterior samples of the estimated conditional average treatment effect.}
#'   \item{hte.est}{The statistical summary results of the conditional average treatment effect}
#' @export
#'
dynamicGP.cate <- function(jobid,x,control.tr,treat.tr,pr.values=NULL) {

    res <- POST(url=paste0('https://pcats.research.cchmc.org/api/job/',jobid,'/dynamicgp.cate'),
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

