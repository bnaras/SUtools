#' @title SUtools Control Arguments
#' @description Details to the \emph{control} parameters.
#' @param fix_allocate a flag denoting whether Jerome Friedman's allocate statements have to be fixed, default `FALSE`
#' @param verbose a flag asking for progress notes, default `TRUE`
#' @return a list containing the control parameters.
#' @export sutools_control
sutools_control <- function(fix_allocate = FALSE, verbose = TRUE) {
  as.list(environment())
}

update_default_controls <- function(control) {
  cntrl <- sutools_control()
  nam <- names(control)[names(control) %in% names(cntrl)]
  cntrl[nam] <- control[nam]
  cntrl
}
