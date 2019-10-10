#'@title getNumSett
#'@description list of all numeric type control settings \\cr \\cr
#'Executed By: \\itemize\{\\item executeRSPARROW.R
#'             \\item outputSettings.R
#'             \\item runRsparrow.R
#'             \\item testCosmetic.R
#'             \\item testSettings.R\} \\cr
#'@return `numericSettings` list of all numeric type control settings



getNumSett<-function(){
  numericSettings<-c("minimum_headwater_site_area",
                     "minimum_reaches_separating_sites",
                     "minimum_site_incremental_area",
                     "s_offset",
                     "pvalidate",
                     "biters",
                     "iseed",
                     "ConcFactor",
                     "lat_limit",
                     "lon_limit",
                     "scenario_factors",
                     "class_landuse_percent",
                     "niters",
                     "bayes_iseed",
                     "maxTree",
                     "adapt_delta",
                     
                     "predictionTitleSize",
                     "predictionLegendSize",
                     "predictionClassRounding",
                     "lineWidth",
                     "residual_map_breakpoints",
                     "ratio_map_breakpoints",
                     "residualTitleSize",
                     "residualLegendSize",
                     "residualPointStyle",
                     "residualPointSize_breakpoints",
                     "residualPointSize_factor",
                     "siteAttrTitleSize",
                     "siteAttrLegendSize",
                     "siteAttrClassRounding",
                     "siteAttr_mapPointStyle",
                     "siteAttr_mapPointSize",
                     "diagnosticPlotPointSize",
                     "diagnosticPlotPointStyle",
                     
                     "confInterval",
                     "bayes_iseed",
                     "niters",
                     "nchains",
                     "select_targetReachWatersheds"
  )
  return(numericSettings)
}
