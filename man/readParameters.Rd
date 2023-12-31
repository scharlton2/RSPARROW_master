% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readParameters.R
\name{readParameters}
\alias{readParameters}
\title{readParameters}
\usage{
readParameters(
  file.output.list,
  if_estimate,
  if_estimate_simulation,
  batch_mode
)
}
\arguments{
\item{file.output.list}{list of control settings and relative paths used for input and
output of external files.  Created by \code{generateInputList.R}}

\item{if_estimate}{yes/no indicating whether or not estimation is run}

\item{if_estimate_simulation}{character string setting from sparrow_control.R indicating
whether estimation should be run in simulation mode only.}

\item{batch_mode}{yes/no character string indicating whether RSPARROW is being run in batch
mode}
}
\value{
\code{betavalues} data.frame of model parameters from parameters.csv
}
\description{
Reads the 'parameters.csv' file. \cr \cr
Executed By: startModelRun.R \cr
Executes Routines: \itemize{\item errorOccurred.R
\item getVarList.R
\item importCSVcontrol.R
\item unPackList.R} \cr
}
