#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))
if (length(res)!=0){
  load(gsub("batchMaps.R","batch.RData",res))
  
  
  if (RSPARROW_errorOption=="yes"){
    #errorhandle
    backupOptions<-list(error = options()$error,
                        show.error.locations = options()$show.error.locations,
                        keep.source = options()$keep.source)
    options(error = quote({
      #print custom message to console
      message("\nRSPARROW SYSTEM ERROR OCCURRED");
      #instruct the user to reset their options
      message('To reset user options in R use options(backupOptions)'); 
      #First dump error stack to file; not accessible by the R session.
      dump.frames("errorDump", to.file=TRUE, include.GlobalEnv=TRUE); 
      #sink to file
      sink(file=paste0(path_results,"error.log")); 
      #print custom error message to file
      cat("RSPARROW SYSTEM ERROR OCCURRED\n");
      #instruct the user to reset their options
      cat('To reset user options in R use options(backupOptions)\n \n'); 
      #Dump again to get error message and write it to error log; 
      #accessible by the R session.
      dump.frames(); 
      #Print simple error message to file
      cat(attr(last.dump,"error.message")); 
      cat('\nTraceback:');
      cat('\n'); #line space
      # Print full traceback of function calls. 
      #The '2' omits the outermost two function calls in the traceback.
      traceback(2); 
      shell.exec(paste0(path_results,"error.log"));
      sink() #end sink
    }),
    #show line numbers in traceback (shown as 'from #4')
    #line numbers count from the function call (i.e. `nestedFunc<-function(){` is line 1)
    show.error.locations = TRUE,keep.source = TRUE)
  }#end Error 
  
  
  
  #load subroutines
  routines<-c("checkBinaryMaps.R",
              "errorOccurred.R",
              "predictMaps.R",
              "mapBreaks.R",
              "named.list.R",
              "unPackList.R",
              "addMarkerText.R",
              "setupDynamicMaps.R",
              "test_addPlotlyvars.R",
              "aggDynamicMapdata.R",
              "mapLoopStr.R",
              "predictMaps_single.R")
  for (r in routines){
    source(paste0(path_main,.Platform$file.sep,"R",.Platform$file.sep,r))
  }
  
  unPackList(lists = list(file.output.list = file.output.list,
                          scenario.input.list = scenario.input.list),
             parentObj = list(NA,NA)) 
  
  #load libraries
  suppressWarnings(suppressMessages(library(sp)))
  suppressWarnings(suppressMessages(library(sf)))
  suppressWarnings(suppressMessages(library(plotly)))
  suppressWarnings(suppressMessages(library(rmarkdown)))
  suppressWarnings(suppressMessages(library(knitr)))
  suppressWarnings(suppressMessages(library(tools)))
  suppressWarnings(suppressMessages(library(gridExtra)))
  suppressWarnings(suppressMessages(library(ggplot2)))
  suppressWarnings(suppressMessages(library(gplots)))
  suppressWarnings(suppressMessages(library(mapview)))
  suppressWarnings(suppressMessages(library(leaflet)))
  suppressWarnings(suppressMessages(library(leaflet.extras)))
  suppressWarnings(suppressMessages(library(htmltools)))
  suppressWarnings(suppressMessages(library(htmlwidgets)))
  
  
  #start sink
  sink(file=paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"batch",.Platform$file.sep,run_id,"_log.txt"),split=FALSE)
  cat("\n \n")
  cat("RSPARROW MODEL NAME: ",run_id,sep="")
  cat("\n \n")
  if (identical(select_scenarioReachAreas,"yes")){
    cat("SCENARIO NAME: ",scenario_name,sep="")
    cat("\n \n")
  }
  cat("OUTPUT DIRECTORY: ",path_results,sep="")
  cat("\n \n")
  
  
  #load objects
  ptm <- proc.time()
  
  #method 1
  if (file.exists(paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predictList"))){
    load(paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predictList")) }
  #load individual object previously saved
  
  load(paste0(path_results,.Platform$file.sep,"data",.Platform$file.sep,"subdata"))
  # load all other required objects
  load(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData"))
  
  
  
  #run predictMaps
  
  if (!Rshiny){
    input<-list(mapType=NA, batch="no",scenarioName="",var="")
  }
  if (!mapScenarios){
    scenario_map_list<-NA
    predictScenarios.list<-NA
    scenario_name<-NA
    scenarioFlag<-NA
  }
  
  predictMaps(#Rshiny
    input, allMetrics, output_map_type,Rshiny,
    #regular
    file.output.list,
    #map_uncertainties,BootUncertainties,
    data_names,mapping.input.list,
    #predict.list,
    subdata,
    #scenarios
    mapScenarios,
    scenario_map_list,
    predictScenarios.list,
    scenarioFlag,
    batch_mode)
  
  
  
  #end sink
  sink()
  
  if (RSPARROW_errorOption=="yes"){
    options(backupOptions)  
  }
  
}

