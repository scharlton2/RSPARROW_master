#'@title diagnosticPlotsNLLS_dyn
#'@description run diagnostic plots and maps at each selected timestep for 
#'             dynamic data \cr \cr
#'Executed By: \itemize{\item diagnosticPlotsNLLS.R
#'             \item diagnosticPlotsValidate.R
#'             \item diagnosticSensitivity.R
#'             \item diagnosticSpatialAutoCorr.R} \cr
#'Executes Routines: \itemize{\item unPackList.R
#'                            \item create_diagnosticPlotList.R
#'                            \item diagnosticPlotsNLLS_dynOut.Rmd
#'                            \item diagnosticPlotsNLLS_timeSeries.Rmd} \cr
#'@param diagnostic_params list of all parameters required for dynamic diagnostics.
#'                         Includes: \itemize{\item validation - TRUE/FALSE indicating 
#'                                                               whether validation should 
#'                                                               be run
#'                                            \item sensitivity - TRUE/FALSE indicating 
#'                                                                whether sensitivity analysis
#'                                                                should be run
#'                                            \item spatialAutoCorr - TRUE/FALSE indicating whether
#'                                                                    spatial auto correlation should
#'                                                                    be run
#'                                            \item file.output.list - list of settings for file 
#'                                                                     output.  created by 
#'                                                                     generateInputLists()
#'                                            \item path_diagnosticMapAttrChild - normalized path to 
#'                                                                                diagnosticMapAttrChild.Rmd
#'                                            \item path_diagnosticCorrChild - normalized path to 
#'                                                                             diagnosticCorrChild.Rmd 
#'                                            \item path_diagnosticClassvarChild - normalized path to 
#'                                                                             diagnosticClassvarChild.Rmd 
#'                                            \item path_diagnosticClassLandChild - normalized path to 
#'                                                                             diagnosticClassLandChild.Rmd
#'                                            \item path_diagnosticContiguousChild - normalized path to 
#'                                                                             diagnosticContiguousChild.Rmd
#'                                            \item path_diagnosticDiagMapChild - normalized path to 
#'                                                                             diagnosticDiagMapChild.Rmd
#'                                            \item path_outputMapsChild - normalized path to 
#'                                                                             outputMapsChild.Rmd
#'                                            \item path_outputMaps - normalized path to 
#'                                                                             outputMaps.Rmd
#'                                            \item class.input.list - list of class relate settings
#'                                                                     created by generateInputLists()
#'                                            \item sitedata.demtarea.class - numeric vector indicating decile 
#'                                                                            class labeled with the total
#'                                                                            output by calcDemtareaClass()
#'                                            \item sitedata - data.frame subset of subdata where depvar>0 and
#'                                                             calsites=1
#'                                            \item sitedata.landuse - data.frame incremental land-use percentages 
#'                                                                     calculated by class_landuse output by 
#'                                                                     calcIncremLandUse()
#'                                            \item estimate.list - output list of estimation results output by 
#'                                                                  estimate()
#'                                            \item mapping.input.list - list of mapping settings output by
#'                                                                       generateInputLists()
#'                                            \item Csites.weights.list - regression weights as proportional to 
#'                                                                        incremental area size output by 
#'                                                                        setNLLSWeights()
#'                                            \item Cor.ExplanVars.list - list of explantory correlations
#'                                                                        output by correlationMatrix()
#'                                            \item data_names - data.frame of dataDictionary.csv
#'                                            \item add_vars - user setting indicating variables to add to
#'                                                             output data.frames
#'                                            \item batch_mode - yes/no indicating whether batch mode operation is
#'                                                               in effect}



diagnosticPlotsNLLS_dyn<-function(diagnostic_params){
  unPackList(lists = list(diagnostic_params = diagnostic_params),
             parentObj = list(NA))
  
  plotList<-create_diagnosticPlotList()
  
  if (validation){
    vPlot.list<-list()
    for (n in 1:length(names(plotList))){
      vTest2<-list()
      vTest<-plotList[[n]]
      if (vTest$vPlot==TRUE){
        eval(parse(text=paste0("vTest2$",names(plotList)[n],"<-vTest")))
        vPlot.list<-c(vPlot.list,vTest2)
      }
    }
    plotList<-vPlot.list
  }
  
  
    sPlot.list<-list()
    for (n in 1:length(names(plotList))){
      sTest2<-list()
      sTest<-plotList[[n]]
      if (sTest$sPlot==TRUE){
        eval(parse(text=paste0("sTest2$",names(plotList)[n],"<-sTest")))
        sPlot.list<-c(sPlot.list,sTest2)
      }
    }  
    
    sacPlot.list<-list()
    for (n in 1:length(names(plotList))){
      sTest2<-list()
      sTest<-plotList[[n]]
      if (sTest$sacPlot==TRUE){
        eval(parse(text=paste0("sTest2$",names(plotList)[n],"<-sTest")))
        sacPlot.list<-c(sacPlot.list,sTest2)
      }
    }  
  
    if (sensitivity){
    plotList<-sPlot.list
  }else if (spatialAutoCorr){
    plotList<-sacPlot.list
    }else{
    #remove sensitivity plots and spatialAutoCorr plots
    plotList<-plotList[which(!names(plotList) %in% c(names(sPlot.list),names(sacPlot.list)))]
  }
  
  for (n in names(plotList)){
    if (validation){
      htmlFile<-paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"validation_plots_dynamic",.Platform$file.sep,
                       eval(parse(text = paste0("plotList$",n,"$title"))),".html")
    }else if (sensitivity){
      htmlFile<-paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnostic_sensitivity_dynamic",.Platform$file.sep,
                       eval(parse(text = paste0("plotList$",n,"$title"))),".html")
    }else if (spatialAutoCorr){
      htmlFile<-paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnostic_spatialautocor_dynamic",.Platform$file.sep,
                       eval(parse(text = paste0("plotList$",n,"$title"))),".html")
      
      }else{
     htmlFile<-paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep,
                            eval(parse(text = paste0("plotList$",n,"$title"))),".html") 
    }
    

    rmdTitle<-eval(parse(text = paste0("plotList$",n,"$title")))
    path_diagnosticPlotsNLLS_dynOutChild<-file_path_as_absolute(paste0(path_master,"diagnosticPlotsNLLS_dynChild.Rmd"))
    
    rmarkdown::render(paste0(path_master,"diagnosticPlotsNLLS_dynOut.Rmd"),
                      params = list(
                        rmdTitle = rmdTitle,
                        diagnostic_params = diagnostic_params,
                        plotIndex = n,
                        path_diagnosticPlotsNLLS_dynOutChild = path_diagnosticPlotsNLLS_dynOutChild
                      ),
                      output_file = htmlFile, quiet = TRUE
    )
    
    
  }
  
if (!validation & !sensitivity){
  htmlFile<-paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep,
                   "Obs_v_Pred_TimeSeriesPlots.html")
  rmdTitle<-"Observed vs. Predicted Time Series Plots"
  path_diagnosticPlotsNLLS_timeSeriesChild<-file_path_as_absolute(paste0(path_master,"diagnosticPlotsNLLS_timeSeriesChild.Rmd"))
  
  rmarkdown::render(paste0(path_master,"diagnosticPlotsNLLS_timeSeries.Rmd"),
                    params = list(
                      rmdTitle = rmdTitle,
                      diagnostic_params = diagnostic_params,
                      path_diagnosticPlotsNLLS_timeSeriesChild = path_diagnosticPlotsNLLS_timeSeriesChild
                    ),
                    output_file = htmlFile, quiet = TRUE
  )
}
  
}