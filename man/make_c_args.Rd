% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/registration.R
\name{make_c_args}
\alias{make_c_args}
\title{Make C argument types}
\usage{
make_c_args(...)
}
\arguments{
\item{...}{list of variable names, as is}
}
\description{
This uses implicit fortran conventions, that is
integers for variables prefixed with i through n and reals for others
}
\examples{
make_c_args(no,ni,x,y,w,theta,ng,mg,aa,ne,nx,nlam,ulam,thr,maxit,verbose,ao,ia,kin,nlp,jerr)


}
