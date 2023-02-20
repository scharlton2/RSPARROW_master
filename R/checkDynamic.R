#'@title checkDynamic
#'@description Determine whether the model is dynamic or static using the
#'             presence of "year" and/or "season" data in subdata \cr \cr
#'Executed By: \itemize{\item createVerifyReachAttr.R
#'                       \item diagnosticPlotsNLLS.Rmd
#'                       \item diagnosticPlotsNLLS_timeSeries.Rmd
#'                       \item estimate.R
#'                       \item estimateNLLSmetrics.R
#'                       \item predictScenariosPrep.R
#'                       \item readForecast.R
#'                       \item startModelRun.R} \cr
#'@param subdata data.frame input data (subdata)
#'@return logical TRUE/FALSE indicating whether the data is dynamic

checkDynamic<-function(subdata){
  dynamic<-TRUE
  if (length(names(subdata)[names(subdata) %in% c("year","season")])==0){
    dynamic<-FALSE
  }else if (length(names(subdata)[names(subdata)=="year"])!=0){
    if (all(is.na(subdata$year))){
      if (length(names(subdata)[names(subdata)=="season"])!=0){
        if(all(is.na(subdata$season))){
          dynamic==FALSE
        }
      }else{#no season found
        dynamic<-FALSE
      }
    }
  }else{# no year found
    if (length(names(subdata)[names(subdata)=="season"])!=0){
      if(all(is.na(subdata$season))){
        dynamic==FALSE
      }
    }else{#noseason found
      dynamic<-FALSE
    }
  }
  return(dynamic)
}