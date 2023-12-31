% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makePaths.R
\name{makePaths}
\alias{makePaths}
\title{makePaths}
\usage{
makePaths(
  path_user,
  path_master,
  run_id,
  results_directoryName,
  data_directoryName,
  gis_directoryName,
  envir = .GlobalEnv
)
}
\arguments{
\item{path_user}{character string path to RSPARROW user directory containing results, data,
and gis subdirectories}

\item{path_master}{character string path to RSPARROW_master directory.  Internally reset to
'RSPARROW_master/R/' subdirectory}

\item{run_id}{character string control setting indicating the current model name}

\item{results_directoryName}{character string indicating the users results subdirectory name}

\item{data_directoryName}{character string control setting indicating the name of the data
directory}

\item{gis_directoryName}{character string control setting indicating the name of the gis
directory}
}
\description{
creates all internal path variables for file input/output \cr \cr
Executed By: executeRSPARROW.R \cr
Executes Routines: \itemize{\item errorOccurred.R
\item named.list.R} \cr
}
