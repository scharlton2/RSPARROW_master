% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setNLLSWeights.R
\name{setNLLSWeights}
\alias{setNLLSWeights}
\title{setNLLSWeights}
\usage{
setNLLSWeights(
  NLLS_weights,
  run_id,
  subdata,
  sitedata,
  data_names,
  minimum_reaches_separating_sites,
  batch_mode
)
}
\arguments{
\item{NLLS_weights}{character string control setting to select regression weights}

\item{run_id}{character string control setting indicating the current model name}

\item{subdata}{data.frame input data (subdata)}

\item{sitedata}{Sites selected for calibration using \code{subdata[(subdata$depvar > 0 & subdata$calsites==1), ]}. The object contains the dataDictionary
‘sparrowNames’ variables, with records sorted in hydrological
(upstream to downstream) order (see the documentation Chapter
sub-section 5.1.2 for details)}

\item{data_names}{data.frame of variable metadata from data_Dictionary.csv file}

\item{minimum_reaches_separating_sites}{number indicating the minimum number of reaches
separating sites}

\item{batch_mode}{yes/no character string indicating whether RSPARROW is being run in batch
mode}
}
\value{
\code{Csites.weights.list} regression weights as proportional to incremental area size
}
\description{
Assigns the user-defined weights to the system variable 'weight', used to
execute a weighted NLLS model estimation. Also calculates the variable 'tiarea' (sum of the
incremental drainage area of reaches located between monitoring sites), used to calculate weighting
expressions based on the size of the intervening areas between sites. \cr \cr
Executed By: startModelRun.R \cr
Executes Routines: \itemize{\item assignIncremSiteIDs.R
\item errorOccurred.R
\item named.list.R
\item unPackList.R} \cr
}
