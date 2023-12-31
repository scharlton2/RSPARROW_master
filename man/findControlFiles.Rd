% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findControlFiles.R
\name{findControlFiles}
\alias{findControlFiles}
\title{findControlFiles}
\usage{
findControlFiles(
  path_user,
  if_userModifyData,
  create_initial_dataDictionary,
  create_initial_parameterControlFiles
)
}
\arguments{
\item{path_user}{character string path to RSPARROW user directory containing results, data,
and gis subdirectories}

\item{if_userModifyData}{yes/no indicating whether or not the userModifyData.R control file
is to be applied}

\item{create_initial_dataDictionary}{yes/no control setting indicating whether a
dataDictionary.csv control file will be created based on the variables found in the data1 file}

\item{create_initial_parameterControlFiles}{yes/no indicating if new parameter files should
be generated based on the dataDictionary.csv control file}
}
\description{
checks to make sure all necessary control files are present in the results
directory \cr \cr
Executed By: executeRSPARROW.R \cr
Executes Routines: errorOccurred.R \cr
}
