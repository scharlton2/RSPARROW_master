runBatchShiny<-function(path_PastResults){
  path_PastResults<-paste0(path_PastResults,"/maps/shinyArgs")
  
  if (file.exists(path_PastResults)){
    load(path_PastResults)
    file.copy(path_PastResults,paste0(shinyArgs$file.output.list$path_main,.Platform$file.sep,"batch",.Platform$file.sep,"shinyBatch.RData"))
    
    system(paste(Sys.which("Rscript.exe")," ",
                 file.path(paste(shinyArgs$file.output.list$path_main,.Platform$file.sep,"batch",.Platform$file.sep,"shinyBatch.R",sep="")),sep=""), 
           wait = FALSE, invisible = FALSE)
    
    
  }else{
    message(paste0(path_PastResults,.Platform$file.sep,results_directoryName,.Platform$file.sep,run_id,.Platform$file.sep,"maps",.Platform$file.sep,"shinyArgs \nFILE NOT FOUND\nRShiny NOT AVAILABLE"))
  }
}