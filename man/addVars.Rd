% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addVars.R
\name{addVars}
\alias{addVars}
\title{addVars}
\usage{
addVars(file.output.list, batch_mode)
}
\arguments{
\item{file.output.list}{list of control settings and relative paths used for input and
output of external files.  Created by \code{generateInputList.R}}

\item{batch_mode}{yes/no character string indicating whether RSPARROW is being run in batch
mode}
}
\description{
Tests for sparrowNames found in parameters.csv, but not in dataDictionary.csv
or design_matrix.csv. Edits dataDictionary.csv and/or design_matrix.csv adding missing
sparrowNames and opens dataDictionary.csv, design_matrix.csv and userModifyData.R for edit. \\cr \\cr
Executed By: executeRSPARROW.R \cr
Executes Routines: \itemize{\item errorOccurred.R
\item getVarList.R
\item importCSVcontrol.R
\item unPackList.R} \cr
}
