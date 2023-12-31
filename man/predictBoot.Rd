% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictBoot.R
\name{predictBoot}
\alias{predictBoot}
\title{predictBoot}
\usage{
predictBoot(
  bEstimate,
  estimate.list,
  estimate.input.list,
  bootcorrectionR,
  DataMatrix.list,
  SelParmValues,
  subdata
)
}
\arguments{
\item{bEstimate}{model coefficients generated in \code{estimateBootstraps.R}}

\item{estimate.list}{list output from \code{estimate.R}}

\item{estimate.input.list}{named list of sparrow_control settings: ifHess, s_offset,
NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars}

\item{bootcorrectionR}{value of 1}

\item{DataMatrix.list}{named list of 'data' and 'beta' matrices and 'data.index.list'
for optimization}

\item{SelParmValues}{selected parameters from parameters.csv using condition
\verb{ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" &  parmMin>=0) | parmType!="SOURCE")}}

\item{subdata}{data.frame input data (subdata)}
}
\value{
\code{predictBoots.list} contains parametric bootstrap predictions for load and yield.
For more details see documentation Section 5.3.2.3
}
\description{
Calculates all conditioned and unconditioned model predictions for reaches for
each bootstrap iteration, for the control setting if_boot_predict<-"yes".  \cr \cr
Executed By: predictBootstraps.R \cr
Executes Routines: \itemize{\item getVarList.R
\item named.list.R
\item unPackList.R
\item deliv_fraction.for
\item mptnoder.for
\item ptnoder.for} \cr
}
