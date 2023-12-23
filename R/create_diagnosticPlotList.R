#'@title create_diagnosticPlotList
#'@description Create a list of diagnostic plot functions to be applied to data
#'             at a specific timestep. \cr \cr
#'Executed By: \itemize{\item diagnosticClassLandChild.Rmd
#'                       \item diagnosticClassvarChild.Rmd
#'                       \item diagnosticContiguousChild.Rmd
#'                       \item diagnosticCorrChild.Rmd
#'                       \item diagnosticPlotsNLLS.Rmd
#'                       \item diagnosticPlotsNLLS_dyn.R
#'                       \item diagnosticPlotsNLLS_dynOut.Rmd
#'                       \item diagnosticSensitivity.Rmd
#'                       \item diagnosticSensParamChild.Rmd
#'                       \item diagnosticSpatialAutoCorr.Rmd} \cr
#'Executes Routines: \itemize{\item named.list.R
#'                            \item unPackList.R
#'                            \item diagnosticPlots_4panel_A.R
#'                            \item diagnosticPlots_4panel_B.R
#'                            \item addMarkerText.R
#'                            \item plotlyLayout.R
#'                            \item fixDupLatLons.R} \cr
#'@return list of the following diagnostic plots (by plot title) 
#'        \itemize{\item ModEstimation_Obs_v_Pred
#'                 \item ModEstimation_Box_and_Quantile_Resid
#'                 \item ModEstimation_Conditioned_v_Unconditioned_loads
#'                 \item ModEstimation_Correlations_ExplanitoryVariables
#'                 \item ModEstimation_DrainageArea_DecileClass_Box
#'                 \item ModEstimation_Classvar_Decile_Box
#'                 \item ModEstimation_LanduseClass_Decile_Box
#'                 \item ModEstimation_4panel_Classvar
#'                 \item ModSimulation_Obs_v_Pred
#'                 \item ModSimulation_Box_and_Quantile_Resid
#'                 \item ModSimulation_Correlations_ExplanitoryVariables
#'                 \item ModSimulation_DrainageArea_DecileClass_Box
#'                 \item ModSimulation_Classvar_Decile_Box
#'                 \item ModSimulation_LanduseClass_Decile_Box
#'                 \item ModSimulation_4panel_Classvar
#'                 \item DiagSensitivity_by_Param
#'                 \item DiagSensitivity1%_by_Param
#'                 \item DiagSensitivity1%_by_Param_logScale
#'                 \item CDF_of_Station_Hydrological_Distances
#'                 \item CDF_of_Station_Euclidean_Distances
#'                 \item Morans_I_by_river_basins
#'                 \item Morans_I_by_Class_variable}
#' @importFrom plotly subplot

create_diagnosticPlotList<-function(){
  plotList<-list()
  plotList$p1<-list()
  plotList$p1$title<-"ModEstimation_Obs_v_Pred"
  plotList$p1$header<-"# Model Estimation Performance Diagnostics
Diagnostics are based on the use of conditioned (monitoring-adjusted) predictions. These predictions provide the most accurate reach predictions for use in calibrating the model. The associated residuals and observed to predicted ratios shown in the following section provide the most relevant measures of the accuracy of the model fit to observed loads.

## Observed vs. predicted for loads and yields and log residuals vs. predicted loads and yields\n"
  plotList$p1$vPlot<-FALSE
  plotList$p1$sPlot<-FALSE
  plotList$p1$sacPlot<-FALSE
  plotList$p1$plotParams<-"named.list(diagnosticPlotPointStyle,diagnosticPlotPointSize,
                                     predict,Obs,add_plotlyVars,sitedata,loadUnits,showPlotGrid,
                                     markerList,pnch,markerCols,yldpredict,yldobs,yieldUnits,Resids,hline,validation)"
plotList$p1$plotFunc<-function(plotParams.list){
  unPackList(lists = list(plotParams.list = plotParams.list),
             parentObj = list(NA))
  assign("validation",validation,env = parent.frame())
  
   if (!validation){ 
     
     plotObs<-Obs
     plotpredict<-predict
     plotyldobs<-yldobs
     plotyldpredict<-yldpredict
     plotResids<-Resids
     
     p<-diagnosticPlots_4panel_A(plotpredict,plotObs,plotyldpredict,plotyldobs,sitedata,plotResids,plotclass=NA,
                                plotTitles = c("'MODEL ESTIMATION PERFORMANCE \n(Monitoring-Adjusted Predictions) \nObserved vs    Predicted Load'",
                                               "'MODEL ESTIMATION PERFORMANCE \nObserved vs Predicted Yield'",
                                               "'Residuals vs Predicted \nLoad'",
                                               "'Residuals vs Predicted \nYield'"),
                                loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                pnch,markerCols,hline,filterClass = NA)

  return(p)
  }
}#end plotIndex_1$plotFunc  

  plotList$p2<-list()
  plotList$p2$title<-"ModEstimation_Box_and_Quantile_Resid"
  plotList$p2$header<-"# Model Estimation Performance Diagnostics
Diagnostics are based on the use of conditioned (monitoring-adjusted) predictions. These predictions provide the most accurate reach predictions for use in calibrating the model. The associated residuals and observed to predicted ratios shown in the following section provide the most relevant measures of the accuracy of the model fit to observed loads.

## Boxplots of residuals and observed/predicted ratios, normal quantile plot of standardized residuals, and plot of squared residuals vs. predicted loads\n"  
  plotList$p2$vPlot<-FALSE
  plotList$p2$sPlot<-FALSE
  plotList$p2$sacPlot<-FALSE
  plotList$p2$plotParams<-"named.list(Resids,ratio.obs.pred,standardResids,
                                     predict,add_plotlyVars,sitedata,loadUnits,showPlotGrid,
                                     markerList,pnch,markerCols,validation)"

  plotList$p2$plotFunc<-function(plotParams.list){
    
      unPackList(lists = list(plotParams.list = plotParams.list),
                 parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
     if (!validation){  
       plotResids<-Resids
       plot.ratio.obs.pred<-ratio.obs.pred
       plot.standardResids<-standardResids
       plotpredict<-predict
       
      p<-diagnosticPlots_4panel_B(sitedata,plotResids,plot.ratio.obs.pred,plot.standardResids,plotpredict,
                                         plotTitles = c("'MODEL ESTIMATION PERFORMANCE \nResiduals'",
                                                        "'MODEL ESTIMATION PERFORMANCE \nObserved / Predicted Ratio'",
                                                        "'Normal Q-Q Plot'",
                                                        "'Squared Residuals vs Predicted Load'"),
                                         loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                         pnch,markerCols,hline)
        assign("Resids2",Resids2,env = parent.frame())

      return(p)
    }
  }#end p2 func
  
  
  plotList$p3<-list()
  plotList$p3$title<-"ModEstimation_Conditioned_v_Unconditioned_loads"
  plotList$p3$header<-"# Model Estimation Performance Diagnostics
Diagnostics are based on the use of conditioned (monitoring-adjusted) predictions. These predictions provide the most accurate reach predictions for use in calibrating the model. The associated residuals and observed to predicted ratios shown in the following section provide the most relevant measures of the accuracy of the model fit to observed loads.

## Conditioned prediction loads vs. unconditioned (simulated) prediction loads\n"
  plotList$p3$vPlot<-FALSE
  plotList$p3$sPlot<-FALSE
  plotList$p3$sacPlot<-FALSE
  plotList$p3$plotParams<-"named.list(ppredict,
                                     predict,add_plotlyVars,sitedata,loadUnits,showPlotGrid,
                                     markerList,pnch,markerCols,validation)"

  plotList$p3$plotFunc<-function(plotParams.list){
    
      unPackList(lists = list(plotParams.list = plotParams.list),
                 parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
       if (!validation){
      markerText<-"~paste('</br> Simulated Load: ',ppredict,
                   '</br> Predicted Load: ',predict"
      data<-data.frame(ppredict = ppredict,predict = predict)
      
      markerText<-addMarkerText(markerText,add_plotlyVars,data, sitedata)$markerText
      data<-addMarkerText(markerText,add_plotlyVars, data,sitedata)$mapData
      
      p <- plotlyLayout(ppredict,predict, log = "xy", nTicks = 5, digits = 0,
                        xTitle = paste0("Simulated Load (",loadUnits,")"), xZeroLine = FALSE,
                        yTitle = paste0("Monitoring-Adjusted Load (",loadUnits,")"), yZeroLine = FALSE,
                        plotTitle = "Monitoring-Adjusted vs. Simulated Loads",
                        legend = FALSE,showPlotGrid = showPlotGrid)
      p <- p %>% add_trace(data = data, x = ~ppredict, y = ~predict, 
                           type = "scatter", 
                           mode = "markers",
                           marker = eval(parse(text = markerList)),
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
      p <- p %>% add_trace(x = ppredict, y = ppredict, 
                           type = "scatter", 
                           mode = "lines",
                           color = I("red"),
                           hoverinfo = 'text',
                           text = "Simulated Load")
      return(p)
    }
  }#end p3 func
  ##############################################
  plotList$p4<-list()
  plotList$p4$title<-"ModEstimation_Correlations_ExplanitoryVariables"
  plotList$p4$header<-  "# Observed to predicted ratio vs. the area-weighted mean values of the user-selected explanatory variables for the incremental areas between calibration sites \n
Output only if control setting if_corrExplanVars<-'yes' selected and a value of 1 entered for 'parmCorrGroup' column in the 'parameters.csv' file.
"
  plotList$p4$vPlot<-FALSE
  plotList$p4$sPlot<-FALSE
  plotList$p4$sacPlot<-FALSE
  plotList$p4$plotParams<-"c(named.list(i,Cor.ExplanVars.list,
                            markerList,add_plotlyVars,sitedata,showPlotGrid,
                                     pnch,markerCols,validation),list(corrData=ratio.obs.pred))"

  plotList$p4$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    if (!validation){
    
    if(min(Cor.ExplanVars.list$cmatrixM_all[,i])<0 | max(Cor.ExplanVars.list$cmatrixM_all[,i])<0) {
      logStr<-""
    }else{
      logStr<-"x"
    }
    
    markerText<-"~paste('</br>',Cor.ExplanVars.list$names[i],': ',xvar,
                   '</br> RATIO OBSERVED TO PREDICTED: ',corrData"
    data<-data.frame(xvar = Cor.ExplanVars.list$cmatrixM_all[,i],corrData = corrData)
    
    markerText<-addMarkerText(markerText,add_plotlyVars,data, sitedata)$markerText
    data<-addMarkerText(markerText,add_plotlyVars, data,sitedata)$mapData
    
    p<-plotlyLayout( Cor.ExplanVars.list$cmatrixM_all[,i],corrData, log = logStr, nTicks = 5, digits = 0,
                  xTitle = paste0("AREA-WEIGHTED EXPLANATORY VARIABLE (",Cor.ExplanVars.list$names[i],")"), 
                  xZeroLine = FALSE,
                  yTitle = "RATIO OBSERVED TO PREDICTED", yZeroLine = FALSE,
                  plotTitle = paste("Observed to Predicted Ratio vs Area-Weighted Explanatory Variable \nFor Incremental Areas between Calibration Sites; Variable Name = ",Cor.ExplanVars.list$names[i]) ,
                  legend = FALSE,showPlotGrid = showPlotGrid) %>%
     add_trace(data = data, x = ~xvar, y = ~corrData, 
                type = "scatter", 
                mode = "markers",
                marker = eval(parse(text = markerList)),
                hoverinfo = 'text',
                text = eval(parse(text = markerText)))
    
    return(p)
    }
  }#end p4 func
  ##################################################
  plotList$p5<-list()
  plotList$p5$title<-"ModEstimation_DrainageArea_DecileClass_Box"
  plotList$p5$header<-"## Boxplots of the observed to predicted loads vs. the decile classes of the total drainage area for the calibration sites\n"  
  plotList$p5$vPlot<-FALSE
  plotList$p5$sPlot<-FALSE
  plotList$p5$sacPlot<-FALSE
  plotList$p5$plotParams<-"named.list(sitedata.demtarea.class,ratio.obs.pred,showPlotGrid,hline,validation)"

  plotList$p5$plotFunc<-function(plotParams.list){
    
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    #if (!validation){ 
 
      # sitedata.demtarea.class regions
      
      vvar <- sitedata.demtarea.class
      
      p <- plotlyLayout(NA,ratio.obs.pred, log = "y", nTicks = 7, digits = 0,
                        xTitle = "Upper Bound for Total Drainage Area Deciles (km2)",  
                        xZeroLine = FALSE,xLabs = sort(as.numeric(unique(vvar))),
                        yTitle ="Observed to Predicted Ratio",  yZeroLine = FALSE,
                        plotTitle = "Ratio Observed to Predicted by Deciles",
                        legend = FALSE,showPlotGrid = showPlotGrid)
      p <- p %>% add_trace(y = ratio.obs.pred,x = vvar, type = 'box', color = I("black"), 
                           fillcolor = "white")
      ##SRC## p <- p %>% layout(shapes = list(hline(1)))
      return(p)
   # }
    }#p5 func
  
  ############################################
  plotList$p6<-list()
  plotList$p6$title<-"ModEstimation_Classvar_Decile_Box"
  plotList$p6$header<-  "## Boxplots of the observed to predicted loads vs. the contiguous spatial classes specified by users in the 'classvar' control setting (e.g., HUC-4) \n"
  plotList$p6$vPlot<-FALSE
  plotList$p6$sPlot<-FALSE
  plotList$p6$sacPlot<-FALSE
  plotList$p6$plotParams<-"c(named.list(k,sitedata,classvar,showPlotGrid,hline,validation),
                            list(boxvar=ratio.obs.pred))"
  
  plotList$p6$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    
  
  vvar <- as.numeric(eval(parse(text=paste0("sitedata$",classvar[k]) )))
  
  p<-plotlyLayout(NA,boxvar, log = "y", nTicks = 7, digits = 0,
               xTitle = classvar[k],  
               xZeroLine = FALSE,xLabs = sort(as.numeric(unique(vvar))),
               yTitle ="Observed to Predicted Ratio",  yZeroLine = FALSE,
               plotTitle = "Ratio Observed to Predicted",
               legend = FALSE,showPlotGrid = showPlotGrid) %>%
    add_trace(y = boxvar,x = vvar, type = 'box', color = I("black"), 
              fillcolor = "white") %>%
    layout(shapes = list(hline(1)))
    ##SRC## plotly::layout(shapes = list(hline(1)))
  return(p)
    
  }#p6 func
  
  
  ###############################################
  plotList$p7<-list()
  plotList$p7$title<-"ModEstimation_LanduseClass_Decile_Box"
  plotList$p7$header<-  "## Boxplots of the observed to predicted loads vs. the deciles of the land-use class variable specified by users in the 'class_landuse' control setting\n
The land-use classes expressed as a percentage of the incremental drainage area extending from the calibration site to the nearest upstream site locations
"
  plotList$p7$vPlot<-FALSE
  plotList$p7$sPlot<-FALSE
  plotList$p7$sacPlot<-FALSE
  plotList$p7$plotParams<-"c(named.list(k,sitedata.landuse,classvar2,showPlotGrid,hline,validation),
                            list(boxvar=ratio.obs.pred))"
  
  plotList$p7$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    
      vvar <- as.numeric(eval(parse(text=paste0("sitedata.landuse$",classvar2[k]) )))
      iprob<-10
      chk <- unique(quantile(vvar, probs=0:iprob/iprob))
      chk1 <- 11 - length(chk)
      if(chk1 == 0) {
        qvars <- as.integer(cut(vvar, quantile(vvar, probs=0:iprob/iprob), include.lowest=TRUE)) 
        avars <- quantile(vvar, probs=0:iprob/iprob)
        qvars2 <- numeric(length(qvars))
        for (j in 1:10) {
          for (i in 1:length(qvars)){
            if(qvars[i] == j) {
              qvars2[i] <- round(avars[j+1],digits=0)
            }
          }
        }
        xxlab <- paste0("Upper Bound for ",classvar2[k])  
        
        p<-plotlyLayout(NA,boxvar, log = "y", nTicks = 7, digits = 0,
                     xTitle = xxlab,  
                     xZeroLine = FALSE,xLabs = sort(as.numeric(unique(qvars2))),
                     yTitle ="Observed to Predicted Ratio",  yZeroLine = FALSE,
                     plotTitle = "Ratio Observed to Predicted by Deciles",
                     legend = FALSE,showPlotGrid = showPlotGrid) %>%
          add_trace(y = boxvar,x = qvars2, type = 'box', color = I("black"), 
                    fillcolor = "white") %>%
          layout(shapes = list(hline(1)))
        
        
      } else {  # non-unique classes
        
        p<-plotlyLayout(NA,boxvar, log = "y", nTicks = 7, digits = 0,
                     xTitle = classvar2[k],  
                     xZeroLine = FALSE,xLabs = sort(as.numeric(unique(vvar))),
                     yTitle ="Observed to Predicted Ratio",  yZeroLine = FALSE,
                     plotTitle = "Ratio Observed to Predicted",
                     legend = FALSE,showPlotGrid = showPlotGrid) %>%
          add_trace(y = boxvar,x = vvar, type = 'box', color = I("black"), 
                    fillcolor = "white") %>%
          layout(shapes = list(hline(1)))
   
      }
      return(p)
    
    
  }#p7 func
 
  ###############################################
  plotList$p8<-list()
  plotList$p8$title<-"ModEstimation_4panel_Classvar"
  plotList$p8$header<-  "## Four-plot panels reported separately for each of the contiguous spatial classes specified for the first variable entry for the 'classvar[1]' control setting\n
The panels include:  observed vs. predicted loads, observed vs. predicted yields, log residuals vs. predicted loads, and log residuals vs. predicted yields 
"
  plotList$p8$vPlot<-FALSE
  plotList$p8$sPlot<-FALSE
  plotList$p8$sacPlot<-FALSE
  plotList$p8$plotParams<-"named.list(i,grp,class,xx,Obs,predict,yldobs,yldpredict,Resids,add_plotlyVars,showPlotGrid,
                                     markerList,pnch,markerCols,sitedata,hline,validation)"

  
  plotList$p8$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
      
       #if (!validation){
      
      xmrb<-grp[i]
      
      xmrb <- as.double(xmrb)
      

        plotclass<-class[,1]
        plotObs<-Obs
        plotpredict<-predict
        plotyldobs<-yldobs
        plotyldpredict<-yldpredict
        plotResids<-Resids
      
     p<-diagnosticPlots_4panel_A(plotpredict,plotObs,plotyldpredict,plotyldobs,sitedata,plotResids,plotclass,
                                          plotTitles = c("paste0('Observed vs Predicted Load \nCLASS Region = ',filterClass,'(n=',nsites,')')",
                                                         "'Observed vs Predicted \nYield'",
                                                         "'Residuals vs Predicted \nLoad'",
                                                         "'Residuals vs Predicted \nYield'"),
                                loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                          pnch,markerCols,hline,filterClass = xmrb)
     return(p)
      
      

  }#p8 func
 #############################################
  plotList$p9<-list()
  plotList$p9$title<-"ModSimulation_Obs_v_Pred"
  plotList$p9$header<-"# Model Simulation Performance Diagnostics
Diagnostics are based on the use of unconditioned predictions (i.e., predictions that are not adjusted for monitoring loads). These predictions (and the associated residuals and observed to predicted ratios shown in the following section) provide the best measure of the predictive skill of the estimated model in simulation mode. The simulated predictions are computed using mean coefficients from the NLLS model estimated with monitoring-adjusted (conditioned) predictions. \n
Four-plot panel for observed vs. predicted for loads and yields, and log residuals vs. predicted loads and yields"
  plotList$p9$vPlot<-TRUE
  plotList$p9$sPlot<-FALSE
  plotList$p9$sacPlot<-FALSE
  plotList$p9$plotParams<-"named.list(diagnosticPlotPointStyle,diagnosticPlotPointSize,
                                     ppredict,Obs,add_plotlyVars,sitedata,loadUnits,showPlotGrid,
                                     markerList,pnch,markerCols,pyldpredict,pyldobs,yieldUnits,pResids,hline,validation)"

  plotList$p9$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    #if (!validation){ 
      
      plotObs<-Obs
      plotpredict<-ppredict
      plotyldobs<-pyldobs
      plotyldpredict<-pyldpredict
      plotResids<-pResids
      
      p<-diagnosticPlots_4panel_A(plotpredict,plotObs,plotyldpredict,plotyldobs,sitedata,plotResids,plotclass=NA,
                                 plotTitles = c("'MODEL SIMULATION PERFORMANCE \nObserved vs Predicted Load'",
                                                "'MODEL SIMULATION PERFORMANCE \nObserved vs Predicted Yield'",
                                                "'Residuals vs Predicted \nLoad'",
                                                "'Residuals vs Predicted \nYield'"),
                                 loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                 pnch,markerCols,hline,filterClass = NA)
      
      return(p)
    #}
  }#end p9 func
  #######################################################
  plotList$p10<-list()
  plotList$p10$title<-"ModSimulation_Box_and_Quantile_Resid"
  plotList$p10$header<-"# Model Simulation Performance Diagnostics
Diagnostics are based on the use of unconditioned predictions (i.e., predictions that are not adjusted for monitoring loads). These predictions (and the associated residuals and observed to predicted ratios shown in the following section) provide the best measure of the predictive skill of the estimated model in simulation mode. The simulated predictions are computed using mean coefficients from the NLLS model estimated with monitoring-adjusted (conditioned) predictions. \n
Four-plot panel for boxplots of residuals and observed/predicted ratios, normal quantile plot of standardized residuals, and plot of squared residuals vs. predicted loads"  
  plotList$p10$vPlot<-TRUE
  plotList$p10$sPlot<-FALSE
  plotList$p10$sacPlot<-FALSE
  plotList$p10$plotParams<-"named.list(pResids,pratio.obs.pred,
                                     ppredict,add_plotlyVars,sitedata,loadUnits,showPlotGrid,
                                     markerList,pnch,markerCols,validation)"

  plotList$p10$plotFunc<-function(plotParams.list){
    
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    assign("validation",validation,env = parent.frame())
    
    #if (!validation){  
      plotResids<-pResids
      plot.ratio.obs.pred<-pratio.obs.pred
      plot.standardResids<-NA
      plotpredict<-ppredict
      
      p<-diagnosticPlots_4panel_B(sitedata,plotResids,plot.ratio.obs.pred,plot.standardResids,plotpredict,
                                 plotTitles = c("'MODEL SIMULATION PERFORMANCE \nResiduals'",
                                                "'MODEL SIMULATION PERFORMANCE \nObserved / Predicted Ratio'",
                                                "'Normal Q-Q Plot'",
                                                "'Squared Residuals vs Predicted Load'"),
                                 loadUnits,yieldUnits,showPlotGrid,markerList,add_plotlyVars,
                                 pnch,markerCols,hline)
      assign("Resids2",Resids2,env = parent.frame())
      
      return(p)
    #}
  }#end p2 func
  
  plotList$p11<-plotList$p4
  plotList$p11$title<-gsub("Estimation","Simulation",plotList$p11$title)
  plotList$p11$plotParams<-"c(named.list(i,Cor.ExplanVars.list,
                            markerList,add_plotlyVars,sitedata,showPlotGrid,
                                     pnch,markerCols,validation),list(corrData=pratio.obs.pred))"
  plotList$p11$vPlot<-FALSE
  plotList$p11$sPlot<-FALSE
  plotList$p11$sacPlot<-FALSE
  
  plotList$p12<-plotList$p5
  plotList$p12$title<-gsub("Estimation","Simulation",plotList$p12$title)
  plotList$p12$plotParams<-"c(named.list(sitedata.demtarea.class,showPlotGrid,hline,validation),
                            list(ratio.obs.pred=pratio.obs.pred))"
  plotList$p12$vPlot<-TRUE
  plotList$p12$sPlot<-FALSE
  plotList$p12$sacPlot<-FALSE
  
  plotList$p13<-plotList$p6
  plotList$p13$title<-gsub("Estimation","Simulation",plotList$p13$title)
  plotList$p13$vPlot<-TRUE
  plotList$p13$sPlot<-FALSE
  plotList$p13$sacPlot<-FALSE
  plotList$p13$plotParams<-"c(named.list(k,sitedata,classvar,showPlotGrid,hline,validation),
                            list(boxvar=pratio.obs.pred))"
  
  
  plotList$p14<-plotList$p7
  plotList$p14$title<-gsub("Estimation","Simulation",plotList$p14$title)
  plotList$p14$vPlot<-TRUE
  plotList$p14$sPlot<-FALSE
  plotList$p14$sacPlot<-FALSE
  plotList$p14$plotParams<-"c(named.list(k,sitedata.landuse,classvar2,showPlotGrid,hline,validation),
                            list(boxvar=pratio.obs.pred))"
  
  
  plotList$p15<-plotList$p8
  plotList$p15$title<-gsub("Estimation","Simulation",plotList$p15$title)
  plotList$p15$plotParams<-"c(named.list(i,grp,class,xx,Obs,add_plotlyVars,showPlotGrid,
                                     markerList,pnch,markerCols,sitedata,hline,validation),
  list(predict=ppredict,yldobs=pyldobs,yldpredict=pyldpredict,Resids=pResids))"
  plotList$p15$vPlot<-TRUE
  plotList$p15$sPlot<-FALSE
  plotList$p15$sacPlot<-FALSE
  
  #####################################
  ############sensitivity plots#######
  plotList$p16<-list()
  plotList$p16$title<-"DiagSensitivity_by_Param"
  plotList$p16$header<-  ""
  plotList$p16$vPlot<-FALSE
  plotList$p16$sPlot<-TRUE
  plotList$p16$sacPlot<-FALSE
  plotList$p16$plotParams<-"named.list(i,apredict_sum,depvar,xclass,Parmnames,showPlotGrid)"
  
  
  plotList$p16$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    
    
    x1 <- apredict_sum[,i]
    xx <- data.frame(x1,depvar,xclass)
    parmsens <- xx[(xx$depvar > 0), ] 
    
    p<-plotlyLayout(NA,parmsens$x1, log = "", nTicks = 5, digits = 0,
                    xTitle = "",  xZeroLine = FALSE, xLabs = parmsens$xclass,
                    yTitle = "Prediction Change (%) Relative to 1% Change",  yZeroLine = FALSE,
                    plotTitle = paste0("Parameter Sensitivity:  ",Parmnames[i]),
                    legend = FALSE,showPlotGrid = showPlotGrid) %>%
      add_trace(y = parmsens$x1,x = parmsens$xclass, type = 'box', name = Parmnames[i], color = I("black"), 
                fillcolor = "white")
    return(p)
    
    
    
  }#p16 func
  
  plotList$p17<-list()
  plotList$p17$title<-"DiagSensitivity1%_by_Param"
  plotList$p17$header<-  ""
  plotList$p17$vPlot<-FALSE
  plotList$p17$sPlot<-TRUE
  plotList$p17$sacPlot<-FALSE
  plotList$p17$plotParams<-"named.list(Estimate,apredict_sum,depvar,xclass,xvalue2,xiqr,xmed,xparm,xsens,Parmnames,showPlotGrid)"
  
  
  plotList$p17$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    
    
    for (i in 1:length(Estimate)) {
      x1 <- apredict_sum[,i]
      xx <- data.frame(x1,depvar,xclass)
      parmsens <- xx[(xx$depvar > 0), ] 
      xvalue2[i] <- i
      xiqr[,i] <- quantile(parmsens$x1, c(0.05,0.25,0.75,0.95)) 
      xmed[i] <- median(parmsens$x1)
      xparm[i] <- Parmnames[i]
      xsens[,i] <- parmsens$x1   # sensitivities for all calibration sites
    }  
    
    # Plot median and IQR for each parameter
    xx <- xiqr[1,(xiqr[1,]>0)]  
    xminimum <- min(xx)
    xminimum <- ifelse(is.infinite(xminimum),0,xminimum)
    xmed <- ifelse( xmed == 0,xminimum,xmed)
    xiqr <- ifelse( xiqr == 0,xminimum,xiqr)
    
    xupper <- xiqr[3,] - xmed
    xlower <- xmed - xiqr[2,]
    supper <- xiqr[4,] - xmed
    slower <- xmed - xiqr[1,]
    
    xupper <- ifelse(xupper == 0,xminimum,xupper)
    supper <- ifelse(supper == 0,xminimum,supper)
    xlower <- ifelse(xlower == 0,xminimum,xlower)
    slower <- ifelse(slower == 0,xminimum,slower)
    
    xx <- data.frame(xmed,xlower,xupper,supper,slower,xparm)
    xx <- xx[with(xx,order(xx$xmed)), ]  
    
    ymin <- min(xiqr)
    ymax <- max(xiqr)
    
    # Arithmetic y axis
    data<-data.frame(x = xvalue2)
    data<-cbind(data,xx)
    
    p<-plotlyLayout(NA,data$xmed, log = "", nTicks = 5, digits = 0,
                 xTitle = "",  xZeroLine = FALSE, xLabs = as.character(data$xparm),
                 yTitle = "CHANGE IN PREDICTED VALUES (%)",  yZeroLine = FALSE,ymin = ymin, ymax = ymax,
                 plotTitle = "PARAMETER SENSITIVITY TO 1% CHANGE",
                 legend = TRUE,showPlotGrid = showPlotGrid) %>%
      add_trace(data = data, x = ~xparm, y = ~xmed, type = 'scatter', mode = 'markers',color = I("#0000FF"),
                name = '90% Interval',
                error_y = ~list(symetric = FALSE,
                                array = supper,
                                arrayminus = slower,
                                color = "#0000FF")) %>%
      add_trace(data  = data, x = ~xparm, y = ~xmed, type = 'scatter', mode = 'markers',color = I("#FF0000"),
                name = '50% Interval',
                error_y = ~list(symetric = FALSE,
                                array = xupper,
                                arrayminus = xlower,
                                color = "#FF0000")) %>%
      add_trace(data  = data, x = ~xparm, y = ~xmed, type = 'scatter', mode = 'markers',color = I("black"),
                name = 'median')
    return(p)
    
    
    
  }#p17 func
  
  plotList$p18<-list()
  plotList$p18$title<-"DiagSensitivity1%_by_Param_logScale"
  plotList$p18$header<-  ""
  plotList$p18$vPlot<-FALSE
  plotList$p18$sPlot<-TRUE
  plotList$p18$sacPlot<-FALSE
  plotList$p18$plotParams<-"named.list(Estimate,apredict_sum,depvar,xclass,xvalue2,xiqr,xmed,xparm,xsens,Parmnames,showPlotGrid)"
  
  
  plotList$p18$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    
    
    for (i in 1:length(Estimate)) {
      x1 <- apredict_sum[,i]
      xx <- data.frame(x1,depvar,xclass)
      parmsens <- xx[(xx$depvar > 0), ] 
      xvalue2[i] <- i
      xiqr[,i] <- quantile(parmsens$x1, c(0.05,0.25,0.75,0.95)) 
      xmed[i] <- median(parmsens$x1)
      xparm[i] <- Parmnames[i]
      xsens[,i] <- parmsens$x1   # sensitivities for all calibration sites
    }  
    
    # Plot median and IQR for each parameter
    xx <- xiqr[1,(xiqr[1,]>0)]  
    xminimum <- min(xx)
    xminimum <- ifelse(is.infinite(xminimum),0,xminimum)
    xmed <- ifelse( xmed == 0,xminimum,xmed)
    xiqr <- ifelse( xiqr == 0,xminimum,xiqr)
    
    xupper <- xiqr[3,] - xmed
    xlower <- xmed - xiqr[2,]
    supper <- xiqr[4,] - xmed
    slower <- xmed - xiqr[1,]
    
    xupper <- ifelse(xupper == 0,xminimum,xupper)
    supper <- ifelse(supper == 0,xminimum,supper)
    xlower <- ifelse(xlower == 0,xminimum,xlower)
    slower <- ifelse(slower == 0,xminimum,slower)
    
    xx <- data.frame(xmed,xlower,xupper,supper,slower,xparm)
    xx <- xx[with(xx,order(xx$xmed)), ]  
    
    ymin <- min(xiqr)
    ymax <- max(xiqr)
    
    # Arithmetic y axis
    data<-data.frame(x = xvalue2)
    data<-cbind(data,xx)
    
    # Log y axis
    p<-plotlyLayout(NA,data$xmed, log = "y", nTicks = 5, digits = 0,
                 xTitle = "",  xZeroLine = FALSE, xLabs = as.character(data$xparm),
                 yTitle = "CHANGE IN PREDICTED VALUES (%)",  yZeroLine = FALSE,ymin = ymin, ymax = ymax,
                 plotTitle = "PARAMETER SENSITIVITY TO 1% CHANGE",
                 legend = TRUE,showPlotGrid = showPlotGrid) %>%
      add_trace(data = data, x = ~xparm, y = ~xmed, type = 'scatter', mode = 'markers',color = I("#0000FF"),
                name = '90% Interval',
                error_y = ~list(symetric = FALSE,
                                array = supper,
                                arrayminus = slower,
                                color = "#0000FF")) %>%
      add_trace(data  = data, x = ~xparm, y = ~xmed, type = 'scatter', mode = 'markers',color = I("#FF0000"),
                name = '50% Interval',
                error_y = ~list(symetric = FALSE,
                                array = xupper,
                                arrayminus = xlower,
                                color = "#FF0000")) %>%
      
      add_trace(data  = data, x = ~xparm, y = ~xmed, type = 'scatter', mode = 'markers',color = I("black"),name = 'median')#,
    return(p)
    
    
    
  }#p18 func
  
  plotList$p19<-list()
  plotList$p19$title<-"CDF_of_Station_Hydrological_Distances"
  plotList$p19$header<-  "CDF of Station Hydrological Distances (units of 'length' variable)\n"
  plotList$p19$vPlot<-FALSE
  plotList$p19$sPlot<-FALSE
  plotList$p19$sacPlot<-TRUE
  plotList$p19$plotParams<-"named.list(data, subdata,data.index.list, hline,showPlotGrid)"
  
  
  plotList$p19$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    unPackList(lists = list(data.index.list = data.index.list),
               parentObj = list(NA))
    set.ZeroPolicyOption(TRUE)
    nreach <- length(data[,1])
    nstas <- sum(ifelse(data[,jdepvar] > 0,1,0))  # jdepvar site load index
    
    nnode <- max(data[,jtnode],data[,jfnode])
    
    # sort file in upstream order
    updata1 <- subdata      # requires 'staidseq' and 'staid'
    updata <- updata1[with(updata1,order(-updata1$hydseq)), ]
    
    # Make site, distance, area upstream transfers
    #  (note:  distance and area are only for hydrologic flow paths; do not include tributary drainage
    #           which would increase incremental area between sites)
    snode <- array(0,dim=nnode)  # 2-digit ID
    stnode <- array(0,dim=nnode) # 8-digit ID
    dnode <- array(0,dim=nnode)  # distance (km)
    anode <- array(0,dim=nnode)  # incremental area
    tanode <- array(0,dim=nnode)  # total area
    dnsite <- numeric(nstas)
    upsite <- numeric(nstas)
    siteid <- numeric(nstas)
    dist <- numeric(nstas)
    area <- numeric(nstas)
    totarea <- numeric(nstas)
    shydseq <- numeric(nstas)
    site_tarea <- array(0,dim=nstas)
    is <- 0
    
    for (k in 1:nreach) {
      tnode <- updata$tnode[k]
      fnode <- updata$fnode[k]
      sitereach <- updata$staidseq[k]    # Station ID value obtained from siteincr.R execution
      
      if (updata$staid[k] > 0) {              # check station sequence number (1-6-2015)
        is <- is+1
        #  store site transition
        dnsite[is] <- snode[tnode]
        siteid[is] <- stnode[tnode]
        upsite[is] <- sitereach
        dist[is] <- dnode[tnode]
        area[is] <- anode[tnode]
        shydseq[is] <- updata$hydseq[k]
        totarea[is] <- tanode[tnode]
        site_tarea[sitereach] <- updata$demtarea[k]  # total area indexed by site ID
        
        #  reset transfer values to current reach containing site
        iarea <- updata$demiarea[k]
        tarea2 <- updata$demtarea[k]
        idist <- updata$length[k] 
        sitereach <- updata$staidseq[k]     # Station ID value obtained from siteincr.R execution
        siteid2 <- updata$staid[k]          # station ID sequence number assigned in hydrologic order
      } else {
        
        #  transfer values to upstream reach
        iarea <- updata$demiarea[k] + anode[tnode]
        tarea2 <- tanode[tnode]
        idist <- (updata$length[k] + dnode[tnode]) * updata$frac[k]
        sitereach <- snode[tnode]
        siteid2 <- stnode[tnode]
      }  # end site check
      
      anode[fnode] <- iarea
      tanode[fnode] <- tarea2
      dnode[fnode] <- idist
      snode[fnode] <- sitereach   # from siteincr.R execution
      stnode[fnode] <- siteid2
    }
    
    #############################################################
    # RESORT BY HYDSEQ to track downstream connections....
    #   run sequentially - no multiple divergences will exist.
    
    sdata <- data.frame(siteid,dnsite,upsite,dist,area,totarea,shydseq)
    sdata <- sdata[with(sdata,order(sdata$shydseq)), ]

    #############################################################
    # Create site matrix of hydrologic distances from SITE MATRIX (sdistance is nstas x nstas matrix)
    # track each site fully upstream recording each site and distance found
    
    sdistance <- matrix(0,nrow=nstas,ncol=nstas)
    for (i in 1:nstas) {
      if (sdata$dnsite[i] > 0) {
        dns <- sdata$dnsite[i]
        dnd <- dist[i]
        sdistance[sdata$upsite[i],dns] <- dnd   # record site for tracking downstream
        if (i < nstas) {
          for (j in (i+1):nstas) {
            if (dns == sdata$upsite[j]) {
              dns <- sdata$dnsite[j]
              dnd <- dnd + sdata$dist[j]
              sdistance[sdata$upsite[i],dns] <- dnd   # record next downstream site
            }
          }
        }
      }
    }
    
    # Station linkages in 'sdistance' matrix
    
    scount <- numeric(nstas)
    for (i in 1:nstas) {
      for (j in 1:nstas) {
        if(sdistance[j,i] > 0) { 
          scount[i] <- scount[i] + 1   # upstream sites linked with site i
        }
      }
    }
    
    sdist <- numeric(sum(scount))
    is <- 0
    for (i in 1:nstas) {
      for (j in 1:nstas) {
        if(sdistance[j,i] > 0) { 
          is <- is+1
          sdist[is] <- sdistance[j,i]
        }
      }
    }
    
    Fn<-ecdf(sdist)
    y<-Fn(sdist)
    plotData<-data.frame(sdist=sdist, y = y)
    plotData<-plotData[order(plotData$sdist),]
    p<-plotlyLayout(plotData$sdist,plotData$y, log = "", nTicks = 7, digits = 0,
                 xTitle = "Distance Between Sites", xZeroLine = FALSE,xminTick = 0,
                 yTitle = "Fn(x)",  yZeroLine = FALSE,ymax = 1,ymin = 0,ymaxTick = 1,
                 plotTitle = "Station Hydrologic Distances",
                 legend = FALSE,showPlotGrid = showPlotGrid) %>%
      
      add_trace(x  = plotData$sdist, y = plotData$y,type="scatter",mode="lines",color=I('black'),
                line = list(color=I('black'))) %>%
      layout(shapes = list(hline(1, color = "black", dash = 'dash'),
                           hline(0, color = "black",dash = 'dash')))
    
    return(p)
    
    
    
  }#p19 func
  
  plotList$p20<-list()
  plotList$p20$title<-"CDF_of_Station_Euclidean_Distances"
  plotList$p20$header<-  "CDF of Station Euclidean Distances (kilometers)\n"
  plotList$p20$vPlot<-FALSE
  plotList$p20$sPlot<-FALSE
  plotList$p20$sacPlot<-TRUE
  plotList$p20$plotParams<-"named.list(data, subdata,sitedata,data.index.list, hline,showPlotGrid)"
  
  
  plotList$p20$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    unPackList(lists = list(data.index.list = data.index.list),
               parentObj = list(NA))
    set.ZeroPolicyOption(TRUE)
    nreach <- length(data[,1])
    nstas <- sum(ifelse(data[,jdepvar] > 0,1,0))  # jdepvar site load index
    
    nnode <- max(data[,jtnode],data[,jfnode])
    
    # sort file in upstream order
    updata1 <- subdata      # requires 'staidseq' and 'staid'
    updata <- updata1[with(updata1,order(-updata1$hydseq)), ]
    
    # Make site, distance, area upstream transfers
    #  (note:  distance and area are only for hydrologic flow paths; do not include tributary drainage
    #           which would increase incremental area between sites)
    snode <- array(0,dim=nnode)  # 2-digit ID
    stnode <- array(0,dim=nnode) # 8-digit ID
    dnode <- array(0,dim=nnode)  # distance (km)
    anode <- array(0,dim=nnode)  # incremental area
    tanode <- array(0,dim=nnode)  # total area
    dnsite <- numeric(nstas)
    upsite <- numeric(nstas)
    siteid <- numeric(nstas)
    dist <- numeric(nstas)
    area <- numeric(nstas)
    totarea <- numeric(nstas)
    shydseq <- numeric(nstas)
    site_tarea <- array(0,dim=nstas)
    is <- 0
    
    for (k in 1:nreach) {
      tnode <- updata$tnode[k]
      fnode <- updata$fnode[k]
      sitereach <- updata$staidseq[k]    # Station ID value obtained from siteincr.R execution
      
      if (updata$staid[k] > 0) {              # check station sequence number (1-6-2015)
        is <- is+1
        #  store site transition
        dnsite[is] <- snode[tnode]
        siteid[is] <- stnode[tnode]
        upsite[is] <- sitereach
        dist[is] <- dnode[tnode]
        area[is] <- anode[tnode]
        shydseq[is] <- updata$hydseq[k]
        totarea[is] <- tanode[tnode]
        site_tarea[sitereach] <- updata$demtarea[k]  # total area indexed by site ID
        
        #  reset transfer values to current reach containing site
        iarea <- updata$demiarea[k]
        tarea2 <- updata$demtarea[k]
        idist <- updata$length[k] 
        sitereach <- updata$staidseq[k]     # Station ID value obtained from siteincr.R execution
        siteid2 <- updata$staid[k]          # station ID sequence number assigned in hydrologic order
      } else {
        
        #  transfer values to upstream reach
        iarea <- updata$demiarea[k] + anode[tnode]
        tarea2 <- tanode[tnode]
        idist <- (updata$length[k] + dnode[tnode]) * updata$frac[k]
        sitereach <- snode[tnode]
        siteid2 <- stnode[tnode]
      }  # end site check
      
      anode[fnode] <- iarea
      tanode[fnode] <- tarea2
      dnode[fnode] <- idist
      snode[fnode] <- sitereach   # from siteincr.R execution
      stnode[fnode] <- siteid2
    }
    
    #############################################################
    # RESORT BY HYDSEQ to track downstream connections....
    #   run sequentially - no multiple divergences will exist.
    
    sdata <- data.frame(siteid,dnsite,upsite,dist,area,totarea,shydseq)
    sdata <- sdata[with(sdata,order(sdata$shydseq)), ]
    
    #############################################################
    # Create site matrix of hydrologic distances from SITE MATRIX (sdistance is nstas x nstas matrix)
    # track each site fully upstream recording each site and distance found
    
    sdistance <- matrix(0,nrow=nstas,ncol=nstas)
    for (i in 1:nstas) {
      if (sdata$dnsite[i] > 0) {
        dns <- sdata$dnsite[i]
        dnd <- dist[i]
        sdistance[sdata$upsite[i],dns] <- dnd   # record site for tracking downstream
        if (i < nstas) {
          for (j in (i+1):nstas) {
            if (dns == sdata$upsite[j]) {
              dns <- sdata$dnsite[j]
              dnd <- dnd + sdata$dist[j]
              sdistance[sdata$upsite[i],dns] <- dnd   # record next downstream site
            }
          }
        }
      }
    }
    
    # Station linkages in 'sdistance' matrix
    
    scount <- numeric(nstas)
    for (i in 1:nstas) {
      for (j in 1:nstas) {
        if(sdistance[j,i] > 0) {
          scount[i] <- scount[i] + 1   # upstream sites linked with site i
        }
      }
    }
    
    #####################################
    # obtain station header information  (change subdata to updata1)
    
    staname <- character(nstas)
    ttarea <- numeric(nstas)
    stano <- numeric(nstas)
    shydseq <- numeric(nstas)
    ssta <- numeric(nstas)
    xlon <- numeric(nstas)
    xlat <- numeric(nstas)
    
    is <- 0
    for (i in 1:nreach) {
      if (updata1$staid[i]>0) {
        is <- is+1
        staname[is] <- updata1$station_name[i]
        ttarea[is] <- updata1$demtarea[i]
        stano[is] <- updata1$staid[i]
        shydseq[is] <- updata1$hydseq[i]
        xlon[is] <- updata1$lon[i]
        xlat[is] <- updata1$lat[i]
      }
    }
    index <- rep(1:nstas)
    siteheader <- data.frame(index,ssta,shydseq,stano,staname,ttarea,xlon,xlat)
    
    dname <- "  Inverse distance weight function: "
    dd <- data.frame(dname,MoranDistanceWeightFunc)
    colnames(dd)<-c(" "," ")
    row.names(dd) <- c(" ")
    
    # Sorted list of stations with upstream station counts (smallest to largest)
    xx <- data.frame(sitedata$station_name,sitedata$station_id,sitedata$staid,scount)
    xx <- xx[with(xx,order(xx$scount,xx$sitedata.staid)), ]
    x1 <- xx[(xx$scount >=  1), ]  # replace subset
    nest_sites <- length(x1$scount)/ length(xx$scount)    # fraction of total sites that are nested 
    
    ###############################################################
    # obtain Euclidean distance for all station pairs
    Lat <- siteheader$xlat
    Lon <- siteheader$xlon
    
    ##############################
    
    
    Lat <- fixDupLatLons(Lat)  # add small random increment to duplicates
    Lon <- fixDupLatLons(Lon)
    
    #########################################################
    
    edistance <- matrix(0,nrow=nstas,ncol=nstas)
    
    for (i in 1:(nstas-1)) {
      for (j in (i+1):nstas) {
        lat1 <- Lat[i] * pi / 180
        lat2 <- Lat[j] * pi / 180
        lon1 <- Lon[i] * pi / 180
        lon2 <- Lon[j] * pi / 180
        R <- 6371    # radius of the earth in km
        x <- (lon2 - lon1) * cos( 0.5*(lat2+lat1) )
        y <- lat2 - lat1
        edistance[i,j] <- R * sqrt( x*x + y*y )    # Euclidean kilometer distance
      }
    }
    
    edist <- numeric((nstas*nstas-1)/2)    # 2957 sites gives 4,371,924 pairs
    is <- 0
    for (i in 1:nstas) {
      for (j in 1:nstas) {
        if(edistance[j,i] > 0) { 
          is <- is+1
          edist[is] <- edistance[j,i]
        }
      }
    }
    Fn<-ecdf(edist)
    y<-Fn(edist)
    plotData<-data.frame(edist=edist, y = y)
    plotData<-plotData[order(plotData$edist),]
    p<-plotlyLayout(plotData$edist,plotData$y, log = "", nTicks = 7, digits = 0,
                 xTitle = "Distance Between Sites", xZeroLine = FALSE,xminTick = 0,
                 yTitle = "Fn(x)",  yZeroLine = FALSE,ymax = 1,ymin = 0,ymaxTick = 1,
                 plotTitle = "Station Euclidean Distances (kilometers)",
                 legend = FALSE,showPlotGrid = showPlotGrid) %>%
      
      add_trace(x  = plotData$edist, y = plotData$y,type="scatter",mode="lines",color=I('black'),
                line = list(color = I('black'))) %>%
      layout(shapes = list(hline(1, color = "black", dash = 'dash'),
                           hline(0, color = "black",dash = 'dash')))
    
    
    return(p)
    
    
    
  }#p20 func
  
  
  plotList$p21<-list()
  plotList$p21$title<-"Morans_I_by_river_basins"
  plotList$p21$header<-  "Four panel plot with Moran's I results by river basin:\n
      + P-value (Euclidean weights)\n
      + Standard deviate (Euclidean weights)\n
      + P-value (Hydrological weights)\n
      + Standard deviate (Hydrological weights)\n"
  plotList$p21$vPlot<-FALSE
  plotList$p21$sPlot<-FALSE
  plotList$p21$sacPlot<-TRUE
  plotList$p21$plotParams<-"named.list(data, subdata,sitedata,Mdiagnostics.list,data.index.list, hline,showPlotGrid)"
  
  
  plotList$p21$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    unPackList(lists = list(data.index.list = data.index.list,
                            Mdiagnostics.list = Mdiagnostics.list),
               parentObj = list(NA,
                                NA))
    set.ZeroPolicyOption(TRUE)
    nreach <- length(data[,1])
    nstas <- sum(ifelse(data[,jdepvar] > 0,1,0))  # jdepvar site load index
    
    nnode <- max(data[,jtnode],data[,jfnode])
    
    # sort file in upstream order
    updata1 <- subdata      # requires 'staidseq' and 'staid'
    updata <- updata1[with(updata1,order(-updata1$hydseq)), ]
    
    # Make site, distance, area upstream transfers
    #  (note:  distance and area are only for hydrologic flow paths; do not include tributary drainage
    #           which would increase incremental area between sites)
    snode <- array(0,dim=nnode)  # 2-digit ID
    stnode <- array(0,dim=nnode) # 8-digit ID
    dnode <- array(0,dim=nnode)  # distance (km)
    anode <- array(0,dim=nnode)  # incremental area
    tanode <- array(0,dim=nnode)  # total area
    dnsite <- numeric(nstas)
    upsite <- numeric(nstas)
    siteid <- numeric(nstas)
    dist <- numeric(nstas)
    area <- numeric(nstas)
    totarea <- numeric(nstas)
    shydseq <- numeric(nstas)
    site_tarea <- array(0,dim=nstas)
    is <- 0
    
    for (k in 1:nreach) {
      tnode <- updata$tnode[k]
      fnode <- updata$fnode[k]
      sitereach <- updata$staidseq[k]    # Station ID value obtained from siteincr.R execution
      
      if (updata$staid[k] > 0) {              # check station sequence number (1-6-2015)
        is <- is+1
        #  store site transition
        dnsite[is] <- snode[tnode]
        siteid[is] <- stnode[tnode]
        upsite[is] <- sitereach
        dist[is] <- dnode[tnode]
        area[is] <- anode[tnode]
        shydseq[is] <- updata$hydseq[k]
        totarea[is] <- tanode[tnode]
        site_tarea[sitereach] <- updata$demtarea[k]  # total area indexed by site ID
        
        #  reset transfer values to current reach containing site
        iarea <- updata$demiarea[k]
        tarea2 <- updata$demtarea[k]
        idist <- updata$length[k] 
        sitereach <- updata$staidseq[k]     # Station ID value obtained from siteincr.R execution
        siteid2 <- updata$staid[k]          # station ID sequence number assigned in hydrologic order
      } else {
        
        #  transfer values to upstream reach
        iarea <- updata$demiarea[k] + anode[tnode]
        tarea2 <- tanode[tnode]
        idist <- (updata$length[k] + dnode[tnode]) * updata$frac[k]
        sitereach <- snode[tnode]
        siteid2 <- stnode[tnode]
      }  # end site check
      
      anode[fnode] <- iarea
      tanode[fnode] <- tarea2
      dnode[fnode] <- idist
      snode[fnode] <- sitereach   # from siteincr.R execution
      stnode[fnode] <- siteid2
    }
    
    #############################################################
    # RESORT BY HYDSEQ to track downstream connections....
    #   run sequentially - no multiple divergences will exist.
    
    sdata <- data.frame(siteid,dnsite,upsite,dist,area,totarea,shydseq)
    sdata <- sdata[with(sdata,order(sdata$shydseq)), ]
    
    #############################################################
    # Create site matrix of hydrologic distances from SITE MATRIX (sdistance is nstas x nstas matrix)
    # track each site fully upstream recording each site and distance found
    
    sdistance <- matrix(0,nrow=nstas,ncol=nstas)
    for (i in 1:nstas) {
      if (sdata$dnsite[i] > 0) {
        dns <- sdata$dnsite[i]
        dnd <- dist[i]
        sdistance[sdata$upsite[i],dns] <- dnd   # record site for tracking downstream
        if (i < nstas) {
          for (j in (i+1):nstas) {
            if (dns == sdata$upsite[j]) {
              dns <- sdata$dnsite[j]
              dnd <- dnd + sdata$dist[j]
              sdistance[sdata$upsite[i],dns] <- dnd   # record next downstream site
            }
          }
        }
      }
    }
    
    # Station linkages in 'sdistance' matrix
    
    scount <- numeric(nstas)
    for (i in 1:nstas) {
      for (j in 1:nstas) {
        if(sdistance[j,i] > 0) {
          scount[i] <- scount[i] + 1   # upstream sites linked with site i
        }
      }
    }
    
    #####################################
    # obtain station header information  (change subdata to updata1)
    
    staname <- character(nstas)
    ttarea <- numeric(nstas)
    stano <- numeric(nstas)
    shydseq <- numeric(nstas)
    ssta <- numeric(nstas)
    xlon <- numeric(nstas)
    xlat <- numeric(nstas)
    
    is <- 0
    for (i in 1:nreach) {
      if (updata1$staid[i]>0) {
        is <- is+1
        staname[is] <- updata1$station_name[i]
        ttarea[is] <- updata1$demtarea[i]
        stano[is] <- updata1$staid[i]
        shydseq[is] <- updata1$hydseq[i]
        xlon[is] <- updata1$lon[i]
        xlat[is] <- updata1$lat[i]
      }
    }
    index <- rep(1:nstas)
    siteheader <- data.frame(index,ssta,shydseq,stano,staname,ttarea,xlon,xlat)
    
    dname <- "  Inverse distance weight function: "
    dd <- data.frame(dname,MoranDistanceWeightFunc)
    colnames(dd)<-c(" "," ")
    row.names(dd) <- c(" ")
    
    # Sorted list of stations with upstream station counts (smallest to largest)
    xx <- data.frame(sitedata$station_name,sitedata$station_id,sitedata$staid,scount)
    xx <- xx[with(xx,order(xx$scount,xx$sitedata.staid)), ]
    x1 <- xx[(xx$scount >=  1), ]  # replace subset
    nest_sites <- length(x1$scount)/ length(xx$scount)    # fraction of total sites that are nested 
    
    ###############################################################
    # obtain Euclidean distance for all station pairs
    Lat <- siteheader$xlat
    Lon <- siteheader$xlon
    
    ##############################
    
    
    Lat <- fixDupLatLons(Lat)  # add small random increment to duplicates
    Lon <- fixDupLatLons(Lon)
    

    
    #############################################################
    # Calculate all site correlations between Residuals for hydrologically connected sites
    
    # Extract Residuals into matrix:  mres(nstas), indexed by site number
    mres <- numeric(nstas)
    mbias <- numeric(nstas)
    mobsd <- numeric(nstas)
    myld <- numeric(nstas)
    siteindex <- numeric(nstas)
    for (k in 1:nstas) {
      mres[k] <- Resids[k]
      mbias[k] <- Obs[k] / predict[k]
      mobsd[k] <- Obs[k] 
      myld[k] <- yldobs[k]
    }
    
    
    ############################################################
    
    #    Moran's I computed separately for each river basin
    
    checkdist <- sdistance
    checkdist <- ifelse(sdistance > 0,1,0)
    checkcount <- scount
    for (j in nstas:1) {      # reverse hydrologic order to identify most downstream site in river basin
      if (scount[j] > 4 & sum(checkdist[,j]) == scount[j] ) {  # minimum of 5 sites gives 10 pairwise comparisons
        checkcount[j] <- scount[j]   # downstream site identifier 
        for (i in 1:nstas) {
          if(checkdist[i,j] > 0) {
            checkdist[i,] <- 0       # zero all matches with this site in the river basin
          }
        }
      } else {
        checkcount[j] <- 0
      }
    }
    xx <- checkcount[checkcount>0]   # replace subset
    
    pmoran <- numeric(length(xx))   
    pmoran_dev <- numeric(length(xx))
    bpmoran <- numeric(length(xx))   
    bpmoran_dev <- numeric(length(xx))
    ind <- rep(1:(length(pmoran)))
    cind <- character(length(ind))
    dsiteid <- numeric(length(xx))
    
    # transfer river basin sites info for Moran test
    #  test executed and reported for only the most downstream site
    ibasin <- 0
    for (j in 1:nstas) {                
      if (checkcount[j] > 0) { # downstream site identified
        ibasin <- ibasin+1
        dsiteid[ibasin] <- j
        is <- 0
        xresids <- numeric(checkcount[j]+1)
        xLat <- numeric(checkcount[j]+1)
        xLon <- numeric(checkcount[j]+1)
        ires <- numeric(checkcount[j]+1)
        
        bdistance <- matrix(0,nrow=checkcount[j]+1,ncol=checkcount[j]+1)
        bres <- numeric(checkcount[j]+1)
        bsites <- numeric(checkcount[j]+1)
        
        #  add the initial downstream site to the vectors
        is <- is+1
        ires[is] <- is
        xresids[is] <- mres[j]
        xLat[is] <- Lat[j]
        xLon[is] <- Lon[j]
        
        bres[is] <- mres[j]
        bsites[is] <- j
        
        for (i in 1:nstas) {
          if (sdistance[i,j] > 0) {    # obtain sites upstream of outlet site j
            is <- is+1
            ires[is] <- is
            xresids[is] <- mres[i]
            xLat[is] <- Lat[i]
            xLon[is] <- Lon[i]
            
            bres[is] <- mres[i]
            bsites[is] <- i
          }
        }
        
        
        # River basin Euclidean distance weights for Moran's
        xmoran <- data.frame(ires,xresids,xLat,xLon)
        xmoran.dists <- as.matrix(dist(cbind(xmoran$xLon, xmoran$xLat)),method = "euclidean")   # planar coordinates
        
        distance <- xmoran.dists
        eval(parse(text=paste0("xmoran.dists.inv <-",MoranDistanceWeightFunc)))
        diag(xmoran.dists.inv) <- 0
        
        cind[ibasin] <- as.character(j)
        
        # convert w to a row standardised general weights object
        lw <- mat2listw(xmoran.dists.inv)
        lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W")
        morantest.obj <- moran.test(xmoran$xresids, lwW, alternative="two.sided")    # SPDEP
        
        pmoran[ibasin] <- morantest.obj$p.value
        pmoran_dev[ibasin] <- morantest.obj$statistic
        
        # River basin hydrological distance weights for Moran's 
        bdistance[1:is,1:is] <- sdistance[bsites,bsites]
        # fill-in cross-tabs
        for (i in 1:is) {
          for (k in 1:is) {
            if(bdistance[i,k]==0) {
              bdistance[i,k] <- bdistance[k,i]           
            }
          }
        }
        
        # Hydrologic distance weighting
        distance <- bdistance
        eval(parse(text=paste0("xmoran.dists.inv <- ifelse(!distance==0,",MoranDistanceWeightFunc,",0)")))
        diag(xmoran.dists.inv) <- 0
        
        # convert w to a row standardised general weights object (same standardization as used in ape::Moran.I)
        lw <- mat2listw(xmoran.dists.inv)
        lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W",zero.policy=TRUE)
        
        # mres[1:nstas] residuals
        morantest.obj <- moran.test(bres, lwW, alternative="two.sided",adjust.n=TRUE,na.action=na.exclude,zero.policy=TRUE)    # SPDEP
        
        bpmoran[ibasin] <- morantest.obj$p.value
        bpmoran_dev[ibasin] <- morantest.obj$statistic    
        
      } # end loop for selecting the most downstream site for execution of Moran's
    } # end site loop
    
    # Create plots
    
    # Euclidean weighted results
    pmoran <- ifelse(pmoran == 0.0,min(pmoran[pmoran > 0]),pmoran)  # apply minimum non-zero to zero values
    if (length(pmoran)==0){
      cind<-character(0)
    }
    p <- plotlyLayout(NA,pmoran, log = "", nTicks = 7, digits = 1,
                      xTitle = "River Basin ID Index",ymin = 0, ymax = 1,
                      xZeroLine = FALSE,xLabs = sort(as.numeric(unique(cind))),
                      yTitle ="P Value (Euclidean distance weighting within basin)",  yZeroLine = FALSE,
                      plotTitle = "Moran's I P Value by River Basin",
                      legend = FALSE,showPlotGrid = showPlotGrid)
    p <- p %>% add_trace(y = pmoran,x = as.numeric(cind), type = 'scatter', color = I("black"),
                         mode="markers", marker = list(symbol='line-ew-open', size=15,line = list(color = 'black',
                                                                                                  width = 3)))
    p1 <- p %>% layout(shapes = list(hline(0.1)))
    
    
    if (length(pmoran_dev)==0){
      cind<-character(0)
    }
    
    p <- plotlyLayout(NA,pmoran_dev, log = "", nTicks = 7, digits = 1,
                      xTitle = "River Basin ID Index",ymin = 0, ymax = 1,
                      xZeroLine = FALSE,xLabs = sort(as.numeric(unique(cind))),
                      yTitle ="Standard Deviate (Euclidean distance weighting\n within basin)",  yZeroLine = FALSE,
                      plotTitle = "Moran's I Standard Deviate by River Basin",
                      legend = FALSE,showPlotGrid = showPlotGrid)
    p <- p %>% add_trace(y = pmoran_dev,x = as.numeric(cind), type = 'scatter', color = I("black"),
                         mode="markers", marker = list(symbol='line-ew-open', size=15,line = list(color = 'black',
                                                                                                  width = 3)))
    p2 <- p %>% layout(shapes = list(hline(0.1)))
    # p<-subplot(p1,p2,nrows = 1, widths = c(0.5,0.5),
    #            titleX = TRUE, titleY=TRUE, margin = 0.08)
    
    # Hydrological weighted results
    bpmoran <- ifelse(bpmoran == 0.0,min(bpmoran[bpmoran > 0]),bpmoran)  # apply minimum non-zero to zero values
    
    if (length(bpmoran)==0){
      cind<-character(0)
    }
    
    p <- plotlyLayout(NA,bpmoran, log = "", nTicks = 7, digits = 1,
                      xTitle = "River Basin ID Index",ymin = 0, ymax = 1,
                      xZeroLine = FALSE,xLabs = sort(as.numeric(unique(cind))),
                      yTitle ="P Value (Hydrologic distance weighting)",  yZeroLine = FALSE,
                      plotTitle = "Moran's I P Value by River Basin",
                      legend = FALSE,showPlotGrid = showPlotGrid)
    p <- p %>% add_trace(y = bpmoran,x = as.numeric(cind), type = 'scatter', color = I("black"),
                         mode="markers", marker = list(symbol='line-ew-open', size=15,line = list(color = 'black',
                                                                                                  width = 3)))
    p3 <- p %>% layout(shapes = list(hline(0.1)))
    
    if (length(bpmoran_dev)==0){
      cind<-character(0)
    }
    p <- plotlyLayout(NA,bpmoran_dev, log = "", nTicks = 7, digits = 1,
                      xTitle = "River Basin ID Index",ymin = 0, ymax = 1,
                      xZeroLine = FALSE,xLabs = sort(as.numeric(unique(cind))),
                      yTitle ="Standard Deviate (Hydrologic distance weighting)",  yZeroLine = FALSE,
                      plotTitle = "Moran's I Standard Deviate by River Basin",
                      legend = FALSE,showPlotGrid = showPlotGrid)
    p <- p %>% add_trace(y = bpmoran_dev,x = as.numeric(cind), type = 'scatter', color = I("black"),
                         mode="markers", marker = list(symbol='line-ew-open', size=15,line = list(color = 'black',
                                                                                                  width = 3)))
    p4 <- p %>% layout(shapes = list(hline(0.1)))
    p<-subplot(p1,p2,p3,p4,nrows = 2, widths = c(0.5,0.5),
               titleX = TRUE, titleY=TRUE, margin = 0.08)
    
    return(p)
    
    
    
  }#p21 func
  
  
  plotList$p22<-list()
  plotList$p22$title<-"Morans_I_by_Class_variable"
  plotList$p22$header<-  "Two panel plot with Moran's I results by Class variable and full domain:\n
      + P-value (Euclidean weights)\n
      + Standard deviate (Euclidean weights)"
  plotList$p22$vPlot<-FALSE
  plotList$p22$sPlot<-FALSE
  plotList$p22$sacPlot<-TRUE
  plotList$p22$plotParams<-"named.list(data, subdata,sitedata,Mdiagnostics.list,data.index.list,class,classvar,numsites,hline,showPlotGrid,path_masterFormat, dynamic)"
  
  
  plotList$p22$plotFunc<-function(plotParams.list){
    unPackList(lists = list(plotParams.list = plotParams.list),
               parentObj = list(NA))
    unPackList(lists = list(data.index.list = data.index.list,
                            Mdiagnostics.list = Mdiagnostics.list),
               parentObj = list(NA,
                                NA))
    set.ZeroPolicyOption(TRUE)
    nreach <- length(data[,1])
    nstas <- sum(ifelse(data[,jdepvar] > 0,1,0))  # jdepvar site load index
    
    nnode <- max(data[,jtnode],data[,jfnode])
    
    # sort file in upstream order
    updata1 <- subdata      # requires 'staidseq' and 'staid'
    updata <- updata1[with(updata1,order(-updata1$hydseq)), ]
    
    # Make site, distance, area upstream transfers
    #  (note:  distance and area are only for hydrologic flow paths; do not include tributary drainage
    #           which would increase incremental area between sites)
    snode <- array(0,dim=nnode)  # 2-digit ID
    stnode <- array(0,dim=nnode) # 8-digit ID
    dnode <- array(0,dim=nnode)  # distance (km)
    anode <- array(0,dim=nnode)  # incremental area
    tanode <- array(0,dim=nnode)  # total area
    dnsite <- numeric(nstas)
    upsite <- numeric(nstas)
    siteid <- numeric(nstas)
    dist <- numeric(nstas)
    area <- numeric(nstas)
    totarea <- numeric(nstas)
    shydseq <- numeric(nstas)
    site_tarea <- array(0,dim=nstas)
    is <- 0
    
    for (k in 1:nreach) {
      tnode <- updata$tnode[k]
      fnode <- updata$fnode[k]
      sitereach <- updata$staidseq[k]    # Station ID value obtained from siteincr.R execution
      
      if (updata$staid[k] > 0) {              # check station sequence number (1-6-2015)
        is <- is+1
        #  store site transition
        dnsite[is] <- snode[tnode]
        siteid[is] <- stnode[tnode]
        upsite[is] <- sitereach
        dist[is] <- dnode[tnode]
        area[is] <- anode[tnode]
        shydseq[is] <- updata$hydseq[k]
        totarea[is] <- tanode[tnode]
        site_tarea[sitereach] <- updata$demtarea[k]  # total area indexed by site ID
        
        #  reset transfer values to current reach containing site
        iarea <- updata$demiarea[k]
        tarea2 <- updata$demtarea[k]
        idist <- updata$length[k] 
        sitereach <- updata$staidseq[k]     # Station ID value obtained from siteincr.R execution
        siteid2 <- updata$staid[k]          # station ID sequence number assigned in hydrologic order
      } else {
        
        #  transfer values to upstream reach
        iarea <- updata$demiarea[k] + anode[tnode]
        tarea2 <- tanode[tnode]
        idist <- (updata$length[k] + dnode[tnode]) * updata$frac[k]
        sitereach <- snode[tnode]
        siteid2 <- stnode[tnode]
      }  # end site check
      
      anode[fnode] <- iarea
      tanode[fnode] <- tarea2
      dnode[fnode] <- idist
      snode[fnode] <- sitereach   # from siteincr.R execution
      stnode[fnode] <- siteid2
    }
    
    #############################################################
    # RESORT BY HYDSEQ to track downstream connections....
    #   run sequentially - no multiple divergences will exist.
    
    sdata <- data.frame(siteid,dnsite,upsite,dist,area,totarea,shydseq)
    sdata <- sdata[with(sdata,order(sdata$shydseq)), ]
    
    #############################################################
    # Create site matrix of hydrologic distances from SITE MATRIX (sdistance is nstas x nstas matrix)
    # track each site fully upstream recording each site and distance found
    
    sdistance <- matrix(0,nrow=nstas,ncol=nstas)
    for (i in 1:nstas) {
      if (sdata$dnsite[i] > 0) {
        dns <- sdata$dnsite[i]
        dnd <- dist[i]
        sdistance[sdata$upsite[i],dns] <- dnd   # record site for tracking downstream
        if (i < nstas) {
          for (j in (i+1):nstas) {
            if (dns == sdata$upsite[j]) {
              dns <- sdata$dnsite[j]
              dnd <- dnd + sdata$dist[j]
              sdistance[sdata$upsite[i],dns] <- dnd   # record next downstream site
            }
          }
        }
      }
    }
    
    # Station linkages in 'sdistance' matrix
    
    scount <- numeric(nstas)
    for (i in 1:nstas) {
      for (j in 1:nstas) {
        if(sdistance[j,i] > 0) {
          scount[i] <- scount[i] + 1   # upstream sites linked with site i
        }
      }
    }
    
    #####################################
    # obtain station header information  (change subdata to updata1)
    
    staname <- character(nstas)
    ttarea <- numeric(nstas)
    stano <- numeric(nstas)
    shydseq <- numeric(nstas)
    ssta <- numeric(nstas)
    xlon <- numeric(nstas)
    xlat <- numeric(nstas)
    
    is <- 0
    for (i in 1:nreach) {
      if (updata1$staid[i]>0) {
        is <- is+1
        staname[is] <- updata1$station_name[i]
        ttarea[is] <- updata1$demtarea[i]
        stano[is] <- updata1$staid[i]
        shydseq[is] <- updata1$hydseq[i]
        xlon[is] <- updata1$lon[i]
        xlat[is] <- updata1$lat[i]
      }
    }
    index <- rep(1:nstas)
    siteheader <- data.frame(index,ssta,shydseq,stano,staname,ttarea,xlon,xlat)
    
    dname <- "  Inverse distance weight function: "
    dd <- data.frame(dname,MoranDistanceWeightFunc)
    colnames(dd)<-c(" "," ")
    row.names(dd) <- c(" ")
    
    # Sorted list of stations with upstream station counts (smallest to largest)
    xx <- data.frame(sitedata$station_name,sitedata$station_id,sitedata$staid,scount)
    xx <- xx[with(xx,order(xx$scount,xx$sitedata.staid)), ]
    x1 <- xx[(xx$scount >=  1), ]  # replace subset
    nest_sites <- length(x1$scount)/ length(xx$scount)    # fraction of total sites that are nested 
    
    ###############################################################
    # obtain Euclidean distance for all station pairs
    Lat <- siteheader$xlat
    Lon <- siteheader$xlon
    
    ##############################
    
    
    Lat <- fixDupLatLons(Lat)  # add small random increment to duplicates
    Lon <- fixDupLatLons(Lon)
    
    
    
    #############################################################
    # Calculate all site correlations between Residuals for hydrologically connected sites
    
    # Extract Residuals into matrix:  mres(nstas), indexed by site number
    mres <- numeric(nstas)
    mbias <- numeric(nstas)
    mobsd <- numeric(nstas)
    myld <- numeric(nstas)
    siteindex <- numeric(nstas)
    for (k in 1:nstas) {
      mres[k] <- Resids[k]
      mbias[k] <- Obs[k] / predict[k]
      mobsd[k] <- Obs[k] 
      myld[k] <- yldobs[k]
    }
    
    
    ############################################################
    
    #    Moran's I computed separately for each river basin
    
    checkdist <- sdistance
    checkdist <- ifelse(sdistance > 0,1,0)
    checkcount <- scount
    for (j in nstas:1) {      # reverse hydrologic order to identify most downstream site in river basin
      if (scount[j] > 4 & sum(checkdist[,j]) == scount[j] ) {  # minimum of 5 sites gives 10 pairwise comparisons
        checkcount[j] <- scount[j]   # downstream site identifier 
        for (i in 1:nstas) {
          if(checkdist[i,j] > 0) {
            checkdist[i,] <- 0       # zero all matches with this site in the river basin
          }
        }
      } else {
        checkcount[j] <- 0
      }
    }
    xx <- checkcount[checkcount>0]   # replace subset
    
    pmoran <- numeric(length(xx))   
    pmoran_dev <- numeric(length(xx))
    bpmoran <- numeric(length(xx))   
    bpmoran_dev <- numeric(length(xx))
    ind <- rep(1:(length(pmoran)))
    cind <- character(length(ind))
    dsiteid <- numeric(length(xx))
    
    # transfer river basin sites info for Moran test
    #  test executed and reported for only the most downstream site
    ibasin <- 0
    for (j in 1:nstas) {                
      if (checkcount[j] > 0) { # downstream site identified
        ibasin <- ibasin+1
        dsiteid[ibasin] <- j
        is <- 0
        xresids <- numeric(checkcount[j]+1)
        xLat <- numeric(checkcount[j]+1)
        xLon <- numeric(checkcount[j]+1)
        ires <- numeric(checkcount[j]+1)
        
        bdistance <- matrix(0,nrow=checkcount[j]+1,ncol=checkcount[j]+1)
        bres <- numeric(checkcount[j]+1)
        bsites <- numeric(checkcount[j]+1)
        
        #  add the initial downstream site to the vectors
        is <- is+1
        ires[is] <- is
        xresids[is] <- mres[j]
        xLat[is] <- Lat[j]
        xLon[is] <- Lon[j]
        
        bres[is] <- mres[j]
        bsites[is] <- j
        
        for (i in 1:nstas) {
          if (sdistance[i,j] > 0) {    # obtain sites upstream of outlet site j
            is <- is+1
            ires[is] <- is
            xresids[is] <- mres[i]
            xLat[is] <- Lat[i]
            xLon[is] <- Lon[i]
            
            bres[is] <- mres[i]
            bsites[is] <- i
          }
        }
        
        
        # River basin Euclidean distance weights for Moran's
        xmoran <- data.frame(ires,xresids,xLat,xLon)
        xmoran.dists <- as.matrix(dist(cbind(xmoran$xLon, xmoran$xLat)),method = "euclidean")   # planar coordinates
        
        distance <- xmoran.dists
        xmoran.dists.inv <- eval(parse(text=MoranDistanceWeightFunc))
        diag(xmoran.dists.inv) <- 0
        
        cind[ibasin] <- as.character(j)
        
        # convert w to a row standardised general weights object
        lw <- mat2listw(xmoran.dists.inv)
        lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W")
        morantest.obj <- moran.test(xmoran$xresids, lwW, alternative="two.sided")    # SPDEP
        
        pmoran[ibasin] <- morantest.obj$p.value
        pmoran_dev[ibasin] <- morantest.obj$statistic
        
        # River basin hydrological distance weights for Moran's 
        bdistance[1:is,1:is] <- sdistance[bsites,bsites]
        # fill-in cross-tabs
        for (i in 1:is) {
          for (k in 1:is) {
            if(bdistance[i,k]==0) {
              bdistance[i,k] <- bdistance[k,i]           
            }
          }
        }
        
        # Hydrologic distance weighting
        distance <- bdistance
        xmoran.dists.inv <- ifelse(!distance==0,eval(parse(text=MoranDistanceWeightFunc)),0)
        diag(xmoran.dists.inv) <- 0
        
        # convert w to a row standardised general weights object (same standardization as used in ape::Moran.I)
        lw <- mat2listw(xmoran.dists.inv)
        lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W",zero.policy=TRUE)
        
        # mres[1:nstas] residuals
        morantest.obj <- moran.test(bres, lwW, alternative="two.sided",adjust.n=TRUE,na.action=na.exclude,zero.policy=TRUE)    # SPDEP
        
        bpmoran[ibasin] <- morantest.obj$p.value
        bpmoran_dev[ibasin] <- morantest.obj$statistic    
        
      } # end loop for selecting the most downstream site for execution of Moran's
    } # end site loop
    
    
    # Euclidean weighted results
    pmoran <- ifelse(pmoran == 0.0,min(pmoran[pmoran > 0]),pmoran)  # apply minimum non-zero to zero values
    
    # Hydrological weighted results
    bpmoran <- ifelse(bpmoran == 0.0,min(bpmoran[bpmoran > 0]),bpmoran)  # apply minimum non-zero to zero values
   
    
    # River basin text output
    # Obtain list of river basin outlets with significant Moran's I
    x1 <- data.frame(sitedata$station_name,sitedata$station_id,sitedata$staid,scount)
    x2 <- x1[(x1$scount > 0), ]  # replace subset

    xx <- data.frame(dsiteid,pmoran,pmoran_dev,bpmoran,bpmoran_dev)
    x2$dsiteid <- x2$sitedata.staid
    sites_sigmoran <- merge(x2,xx,by="dsiteid",all.y=TRUE,all.x=TRUE)

    sites_sigmoran <- sites_sigmoran[(!is.na(sites_sigmoran$pmoran)),]
    sites_sigmoran <- sites_sigmoran[,-1]
    colnames(sites_sigmoran) <- c("Site Name"," Site ID"," Downstrm Site ID"," Upstrm Site Count"," P-Value(E)"," Standard Deviate(E)"," P-Value(H)"," Standard Deviate(H)")

    ################################################################################
    # Full Domain:  Hydrologic channel distance weighting for Moran's I test

    for (i in 1:nstas) {
      for (k in 1:nstas) {
        if(sdistance[i,k]==0) {
          sdistance[i,k] <- sdistance[k,i]
        }
      }
    }

    distance <- sdistance
    xmoran.dists.inv <- ifelse(!distance==0,eval(parse(text=MoranDistanceWeightFunc)),0)
    diag(xmoran.dists.inv) <- 0

    # convert w to a row standardised general weights object
    lw <- mat2listw(xmoran.dists.inv)
    lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W",zero.policy=TRUE)

    # mres[1:nstas] residuals
    morantest.obj <- moran.test(mres, lwW, alternative="two.sided",adjust.n=TRUE,na.action=na.exclude,zero.policy=TRUE)    # SPDEP

    pmoran <- morantest.obj$p.value
    pmoran_dev <- morantest.obj$statistic

    moranOut <- data.frame(pmoran,pmoran_dev)
    rownames(moranOut) <- c(" ")
    colnames(moranOut) <- c(" Moran's P-Value"," Moran's Standard Deviate")

    xtext <- paste0("  Fraction of sites that are nested:  ",round(nest_sites,digits=3))


    ################################################################################
    # Moran's I for Euclidean distance within CLASS variable (e.g., HUC-2) and for full domain
    ibasin <- 0

    if(!is.na(classvar[1]) & (classvar[1] != "sitedata.demtarea.class")) {   # process only where class variable designated by user

      # cycle through regions:  CLASS
      # Obtain CLASS region numbers
      mrbgrp <- table(class[,1])   # get labels
      xx <- as.data.frame(mrbgrp)  # convert table to dataframe...
      mrbgrp <- as.numeric(xx$Freq)
      xclass <- as.numeric(xx$Var1)
      xclass <- as.numeric(levels(xx$Var1)[xx$Var1])  # convert factor levels to numeric values

      pmoran <- numeric(length(xclass)+1)
      pmoran_dev <- numeric(length(xclass)+1)
      ind <- rep(1:(length(pmoran)))
      cind <- character(length(pmoran))
      cindLabel <- classvar[1]

      for (j in 1:length(xclass)) {

        ibasin <- ibasin + 1

        # transfer river basin sites info for Moran test

        is <- 0
        xresids <- numeric(mrbgrp[j])
        xLat <- numeric(mrbgrp[j])
        xLon <- numeric(mrbgrp[j])
        ires <- numeric(mrbgrp[j])

        for (i in 1:numsites) {
          if (class[i] == xclass[j]) {
            is <- is+1
            ires[is] <- is
            xresids[is] <- mres[i]
            xLat[is] <- Lat[i]
            xLon[is] <- Lon[i]
          }
        }

        if(is >= 4) {  # only calculate for more than 4 sites
          xmoran <- data.frame(ires,xresids,xLat,xLon)
          xmoran.dists <- as.matrix(dist(cbind(xmoran$xLon, xmoran$xLat)),method = "euclidean")
          distance <- xmoran.dists
          xmoran.dists.inv <- eval(parse(text=MoranDistanceWeightFunc))
          diag(xmoran.dists.inv) <- 0

          cind[ibasin] <- as.character(xclass[j])

          # convert w to a row standardised general weights object
          lw <- mat2listw(xmoran.dists.inv)
          lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W")
          morantest.obj <- moran.test(xmoran$xresids, lwW, alternative="two.sided")    # SPDEP
          pmoran[ibasin] <- morantest.obj$p.value
          pmoran_dev[ibasin] <- morantest.obj$statistic
        }

      }  # end class loop

      ##########################################################################
    }  else {   # case of no designation of class variable
      pmoran <- numeric(1)
      pmoran_dev <- numeric(1)
      cind <- character(1)
      cindLabel <- "Total Area"
    } # end check for designation of class variables by user

    # Full spatial domain

    ibasin <- ibasin + 1

    # transfer river basin sites info for Moran test

    is <- 0
    xresids <- numeric(numsites)
    xLat <- numeric(numsites)
    xLon <- numeric(numsites)
    ires <- numeric(numsites)

    for (i in 1:numsites) {
      is <- is+1
      ires[is] <- is
      xresids[is] <- mres[i]
      xLat[is] <- Lat[i]
      xLon[is] <- Lon[i]
    }

    xmoran <- data.frame(ires,xresids,xLat,xLon)
    xmoran.dists <- as.matrix(dist(cbind(xmoran$xLon, xmoran$xLat)),method = "euclidean")
    distance <- xmoran.dists
    xmoran.dists.inv <- eval(parse(text=MoranDistanceWeightFunc))
    diag(xmoran.dists.inv) <- 0

    cind[ibasin] <- "Total Area"

    # convert w to a row standardised general weights object
    lw <- mat2listw(xmoran.dists.inv)
    lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W")
    morantest.obj <- moran.test(xmoran$xresids, lwW, alternative="two.sided")    # SPDEP
    pmoran[ibasin] <- morantest.obj$p.value
    pmoran_dev[ibasin] <- morantest.obj$statistic
    
    # 
    # Plot Moran's I by Class variable (e.g., HUC-2)
   pmoran <- ifelse(pmoran == 0.0,min(pmoran[pmoran > 0]),pmoran)  # apply minimum non-zero to zero values

   p <- plotlyLayout(NA,pmoran, log = "", nTicks = 7, digits = 1,
                     xTitle = cindLabel,ymin = 0, ymax = 1,
                     xZeroLine = FALSE,xLabs = sort(as.numeric(unique(cind))),
                     yTitle ="Moran's P Value (Euclidean distance weighting)",  yZeroLine = FALSE,
                     plotTitle = "Moran's I P Value by CLASS Variable",
                     legend = FALSE,showPlotGrid = showPlotGrid)
   p <- p %>% add_trace(y = pmoran,x = as.numeric(cind), type = 'scatter', color = I("black"),
                        mode="markers", marker = list(symbol='line-ew-open', size=15,line = list(color = 'black',
                                                                                                 width = 3)))
   p1 <- p %>% layout(shapes = list(hline(0.1)))

   p <- plotlyLayout(NA,pmoran_dev, log = "", nTicks = 7, digits = 1,
                     xTitle = cindLabel,ymin = 0, ymax = 1,
                     xZeroLine = FALSE,xLabs = sort(as.numeric(unique(cind))),
                     yTitle ="Moran's Standard Deviate\n (Euclidean distance weighting)",  yZeroLine = FALSE,
                     plotTitle = "Moran's I Standard Deviate by CLASS Variable",
                     legend = FALSE,showPlotGrid = showPlotGrid)
   p <- p %>% add_trace(y = pmoran_dev,x = as.numeric(cind), type = 'scatter', color = I("black"),
                        mode="markers", marker = list(symbol='line-ew-open', size=15,line = list(color = 'black',
                                                                                                 width = 3)))
   p2 <- p %>% layout(shapes = list(hline(0.1)))
   if (!dynamic){
     h<-c(1)
   }else{
     h<-c(0.5)
   }
   p<-subplot(p1,p2,nrows = 1, widths = c(0.5,0.5),heights = h,
              titleX = TRUE, titleY=TRUE, margin = 0.08)
    
    
    
    if (!dynamic){
    # output moran's I p values to text file 
    
    if(!is.na(classvar[1]) & (classvar[1] != "sitedata.demtarea.class")) {   # process only where class variable designated by user
      nmrbout <- numeric(length(xclass)+1)
      nmrbout[1:length(mrbgrp)] <- mrbgrp[1:length(mrbgrp)]
      nmrbout[length(mrbgrp)+1] <- sum(mrbgrp)
    } else {
      nmrbout <- numeric(1)
      nmrbout[1] <- numsites
    }
    
    class_sigmoran <- data.frame(cind,nmrbout,pmoran,pmoran_dev)
    colnames(class_sigmoran) <- c(cindLabel," Number Stations"," Moran's P-Value"," Moran's Standard Deviate")
    
    fileCSV<-paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"summaryCSV",.Platform$file.sep)
    fileout<-paste0(fileCSV,"EuclideanMoransI.csv")
    fwrite(class_sigmoran,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    
    
    saveList<-named.list(dd,sites_sigmoran,moranOut,xtext,class_sigmoran)
    save(saveList,file = paste0(path_masterFormat,"tempDiagSpat.RData"))
    }
    
    return(p)
    
    
    
  }#p22 func
  ############################################
 return(plotList)
}