% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importCSVcontrol.R
\name{importCSVcontrol}
\alias{importCSVcontrol}
\title{importCSVcontrol}
\usage{
importCSVcontrol(
  filein,
  Ctype,
  NAMES,
  strEndMessage,
  file.output.list,
  exitRun,
  batch_mode
)
}
\arguments{
\item{filein}{path to input file in csv format}

\item{Ctype}{column class designation for required columns}

\item{NAMES}{column names for required columns}

\item{strEndMessage}{message string to be evaluated}

\item{file.output.list}{list of control settings and relative paths used for input and
output of external files.  Created by \code{generateInputList.R}}

\item{exitRun}{TRUE/FALSE states whether code should terminate}

\item{batch_mode}{yes/no character string indicating whether RSPARROW is being run in batch
mode}
}
\value{
\code{data} data object imported from csv control file
}
\description{
Imports csv control file and checks/corrects the number of columns used by the
'syncVarNames' function.  \cr \cr
Executed By: \itemize{\item addVars.R
\item createInitialParameterControls.R
\item createMasterDataDictionary.R
\item read_dataDictionary.R
\item readDesignMatrix.R
\item readParameters.R
\item syncVarNames.R} \cr
Executes Routines: \itemize{\item errorOccurred.R
\item unPackList.R} \cr
}
