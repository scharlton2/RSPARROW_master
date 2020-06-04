#'@title runShiny
#'@description runs shiny app on previously executed RSPARROW model \\cr \\cr
#'Executed By:  \\cr
#'Executes Routines: \\itemize\{\\item shinyMap2.R
#'             \\item unPackList.R\} \\cr
#'@param path_PastResults path to previously generated results including run_id subdirectory
#'@examples
#'path_master<-'./RSPARROW_master'
#'suppressWarnings(remove(list="runRsparrow"))
#'devtools::load_all(path_master,recompile = FALSE)
#'runShiny("~/UserDirectory/results/customRunId")



#runShiny<-function(file.output.list, enable_ShinyApp,RSPARROW_errorOption){
runShiny<-function(path_PastResults){
 #load shinyArgs

  path_PastResults<-paste0(path_PastResults,"/maps/shinyArgs")
  
  if (file.exists(path_PastResults)){
    load(path_PastResults)
  
    unPackList(lists = list(file.output.list = file.output.list,
                          shinyArgs = shinyArgs),
             parentObj = list(NA,NA)) 
  

    #trigger shiny
  shiny::runApp(shinyMap2(
    #stream/catchment
    file.output.list,map_uncertainties,BootUncertainties,
    data_names,mapping.input.list,
    #predict.list,
    subdata,SelParmValues,
    #site attr
    sitedata,
    #scenarios
    estimate.list,
    ConcFactor,DataMatrix.list,dlvdsgn,
    reach_decay_specification,reservoir_decay_specification,
    scenario.input.list,
    #scenarios out
    add_vars,
    #batchError
    batch_mode,
    RSPARROW_errorOption))
  stopApp()
    
  
    }else{
    message(paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,run_id,.Platform$file.sep,"maps",.Platform$file.sep,"shinyArgs \nFILE NOT FOUND\nRShiny NOT AVAILABLE"))
  }
}
