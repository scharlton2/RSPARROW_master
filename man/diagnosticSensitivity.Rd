% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnosticSensitivity.R
\name{diagnosticSensitivity}
\alias{diagnosticSensitivity}
\title{diagnosticSensitivity}
\usage{
diagnosticSensitivity(
  file.output.list,
  classvar,
  estimate.list,
  DataMatrix.list,
  SelParmValues,
  reach_decay_specification,
  reservoir_decay_specification,
  subdata,
  sitedata.demtarea.class,
  mapping.input.list
)
}
\arguments{
\item{file.output.list}{list of control settings and relative paths used for input and
output of external files.  Created by \code{generateInputList.R}}

\item{classvar}{character vector of user specified spatially contiguous discrete
classification variables from sparrow_control.  First element is reach classification variable.}

\item{estimate.list}{list output from \code{estimate.R}}

\item{DataMatrix.list}{named list of 'data' and 'beta' matrices and 'data.index.list'
for optimization}

\item{SelParmValues}{selected parameters from parameters.csv using condition
\verb{ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" &  parmMin>=0) | parmType!="SOURCE")}}

\item{reach_decay_specification}{the SAS IML reach decay function code from sparrow_control}

\item{reservoir_decay_specification}{the SAS IML reservoir decay function code from
sparrow_control}

\item{subdata}{data.frame input data (subdata)}

\item{sitedata.demtarea.class}{Total drainage area classification variable for calibration
sites.}

\item{mapping.input.list}{Named list of sparrow_control settings for mapping: lat_limit,
lon_limit, master_map_list, lineShapeName, lineWaterid,
polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext,
convertShapeToBinary.list, map_siteAttributes.list,
residual_map_breakpoints, site_mapPointScale,
if_verify_demtarea_maps}
}
\description{
Calculates the parameter sensitivities (change in load predictions for a a 1\%
unit change in the explanatory variables). Outputs plots to
~/estimate/(run_id)_diagnostic_sensitivity.pdf. Outputs \code{sensitivities.list} as binary file to ~/estimate/(run_id)_sensitivities.list. \cr \cr
Executed By: estimate.R \cr
Executes Routines: \itemize{\item named.list.R
\item predictSensitivity.R
\item unPackList.R} \cr
}
