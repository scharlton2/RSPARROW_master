% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictSummaryOutCSV.R
\name{predictSummaryOutCSV}
\alias{predictSummaryOutCSV}
\title{predictSummaryOutCSV}
\usage{
predictSummaryOutCSV(
  file.output.list,
  estimate.input.list,
  SelParmValues,
  estimate.list,
  predict.list,
  subdata,
  class.input.list
)
}
\arguments{
\item{file.output.list}{list of control settings and relative paths used for input and
output of external files.  Created by \code{generateInputList.R}}

\item{estimate.input.list}{named list of sparrow_control settings: ifHess, s_offset,
NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars}

\item{SelParmValues}{selected parameters from parameters.csv using condition
\verb{ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" &  parmMin>=0) | parmType!="SOURCE")}}

\item{estimate.list}{list output from \code{estimate.R}}

\item{predict.list}{archive with all load and yield prediction variables to provide for
the efficient access and use of predictions in subsequent execution
of the parametric bootstrap predictions and uncertainties, mapping,
and scenario evaluations.  For more details see documentation Section
5.3.1.5}

\item{subdata}{data.frame input data (subdata)}

\item{class.input.list}{list of control settings related to classification variables}
}
\description{
Calculates and outputs to the ~/estimate/(run_id)_summary_predictions.csv file
the percentile summaries of reach predictions of load, yield, concentration, and delivery
fraction for the control settings if_estimate<-"yes" and if_predict<-"yes", and outputs percentile
summaries for the yields  by predominant land-use type specified in the 'class_landuse' control setting. \cr \cr
Executed By: estimate.R \cr
Executes Routines: \itemize{\item calcClassLandusePercent.R
\item unPackList.R} \cr
}
