#' @title Fit a linear model using Gaussian Process.
#' @description FIXME: GPMatch is used to fit a linear model within a Bayesian framework.
#'   Gaussian process (GP) prior covariance function is utilized as a matching tool
#'   which accomplishes matching and flexible outcome modeling in a single step.
#' @param datafile File to upload (.csv or .xls)
#' @param dataref Reference to already uploaded file.
#' @param outcome The name of the outcome variable.
#' @param treatment The vector of the name of the treatment variables.
#' @param x.explanatory The vector of the name of the explanatory variables.
#' @param x.confounding The vector of the name of the confounding variables.
#' @param tr.hte An optional vector specifying variables which may have heterogeneous treatment effect with the first treatment variable
#' @param tr2.hte An optional vector specifying variables which may have heterogeneous treatment effect with the second treatment variable
#' @param burn.num numeric; the number of MCMC 'burn-in' samples, i.e. number of MCMC to be discarded.
#' @param mcmc.num numeric; the number of MCMC samples after 'burn-in'.
#' @param outcome.type Outcome type ("Continuous" or "Discrete").
#' @param outcome.bound_censor logical; if TRUE, outcomes are bounded. If FALSE, outcomes are not bounded.
#' @param outcome.censor logical; if TRUE, outcomes are bounded. If FALSE, outcomes are not bounded.
#' @param outcome.censor.lv outcome.censor.lv
#' @param outcome.censor.uv outcome.censor.uv
#' @param outcome.censor.yn outcome.censor.yn
#' @param outcome.link outcome.link
#' @param tr.type tr.type
#' @param tr2.type tr2.type
#' @param x.categorical x.categorical
#' @param method method
#' @param outcome.lb Lower bound if \code{outcome.censor} is TRUE.
#' @param outcome.ub Upper bound if \code{outcome.censor} is TRUE.
#' @param tr.values user-defined values for the calculation of ATE if the first treatment variable is continuous
#' @param tr2.values user-defined values for the calculation of ATE if the second treatment variable is continuous
#' @param pr.values pr.values
#' @param categorical Ensure the specified fields are categorical
#' @param mi.datafile File to upload (.csv or .xls) that contains the imputed data in the model.
#' @param mi.dataref Reference to already uploaded file that contains the imputed data in the model.
#' @param sheet If \code{dataurl} points to Excel file this variable specifies which sheet to load.
#' @param mi.sheet If \code{mi.dataurl} points to Excel file this variable specifies which sheet to load.
#' @return jobid
#' @export
#' @import httr
#' @import utils
staticGP <- function(
                     datafile=NULL,
                     dataref=NULL,
                     outcome, treatment,
                     x.explanatory=NULL, x.confounding=NULL,
                     tr.hte=NULL, tr2.hte=NULL,
                     burn.num=500, mcmc.num=500,
                     outcome.lb=NULL, outcome.ub=NULL,
                     outcome.bound_censor="neither",
                     outcome.type="Continuous",
                     outcome.censor.lv=NULL, outcome.censor.uv=NULL,
                     outcome.censor.yn=NULL,
                     outcome.link="identity",
                     tr.type="Discrete",
                     tr2.type="Discrete",
                     tr.values=NULL, tr2.values=NULL,
                     pr.values=NULL,
                     x.categorical=NULL,
                     method="BART",
                     mi.datafile=NULL,
                     mi.dataref=NULL,
                     sheet=NULL,mi.sheet=NULL) {

  data<-NULL
  if (!is.null(datafile)) {
    data <- httr::upload_file(datafile)
  }
  mi.data<-NULL
  if (!is.null(mi.datafile)) {
    mi.data <- httr::upload_file(mi.datafile)
  }
  res <- POST(url='https://pcats.research.cchmc.org/api/staticgp',
              encode='multipart',
              body=list(data=data,
                        dataref=dataref,
                        outcome=outcome,
                        treatment=treatment,
                        x.explanatory=x.explanatory,
                        x.confounding=x.confounding,
                        tr.hte=tr.hte,
                        tr2.hte=tr2.hte,
                        burn.num=burn.num, mcmc.num=mcmc.num,
                        outcome.lb=outcome.lb,
                        outcome.ub=outcome.ub,
                        outcome.bound_censor=outcome.bound_censor,
                        outcome.type=outcome.type,
                        outcome.censor.lv=outcome.censor.lv,
                        outcome.censor.uv=outcome.censor.uv,
                        outcome.censor.yn=outcome.censor.yn,
                        outcome.link=outcome.link,
                        tr.type=tr.type,
                        tr2.type=tr2.type,
                        tr.values=tr.values,
                        tr2.values=tr2.values,
                        pr.values=pr.values,
                        x.categorical=x.categorical,
                        method=method,
                        mi.data=mi.data,
                        mi.dataref=mi.dataref,
                        sheet=sheet,
                        mi.sheet=mi.sheet))

  cont <- content(res)
  jobid <- cont$jobid[[1]]
  jobid
}
