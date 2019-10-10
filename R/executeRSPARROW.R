#'@title executeRSPARROW
#'@description Runs manageDirVars functions and then proceeds with model excution either in 
#'            regular mode or batch mode \\cr \\cr
#'Executed By: runRsparrow.R \\cr
#'Executes Routines: \\itemize\{\\item batchRun.R
#'             \\item addVars.R
#'             \\item copyPriorModelFiles.R
#'             \\item createDirs.R
#'             \\item createInitialDataDictionary.R
#'             \\item createInitialParameterControls.R
#'             \\item dataInputPrep.R
#'             \\item deleteFiles.R
#'             \\item errorOccurred.R
#'             \\item findControlFiles.R
#'             \\item generateInputLists.R
#'             \\item getCharSett.R
#'             \\item getNumSett.R
#'             \\item getOptionSett.R
#'             \\item getShortSett.R
#'             \\item getYesNoSett.R
#'             \\item isScriptSaved.R
#'             \\item makePaths.R
#'             \\item named.list.R
#'             \\item openDesign.R
#'             \\item openParameters.R
#'             \\item openVarnames.R
#'             \\item outputSettings.R
#'             \\item removeObjects.R
#'             \\item setMapDefaults.R
#'             \\item setupMaps.R
#'             \\item startModelRun.R
#'             \\item testSettings.R
#'             \\item unPackList.R\} \\cr
#'@param settingValues user input values for all control settings
#'@param settingNames names of all control settings
#'@param activeFile character string path to sparrow_control.R file at currently top level of 
#'       user's results directory



executeRSPARROW<-function(settingValues,settingNames,activeFile, envir = .GlobalEnv){
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  
  settings<-settingValues
  
  names(settings)<- settingNames
  
  
  unPackList(lists = list(settings = settings),
             parentObj = list(NA))   
  
  #copy old model if requested
  if (!is.na(copy_PriorModelFiles)){
    copyPriorModelFiles(activeFile,copy_PriorModelFiles,path_master, batch_mode)
    runOld<-"yes"
  }else{
    runOld<-"no"
  }
  
  #trigger shiny only
  
  
  
  testDir<- paste(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,run_id,.Platform$file.sep,sep="") 
  
  findControlFiles(path_user,if_userModifyData,
                   create_initial_dataDictionary, create_initial_parameterControlFiles)
  
  
  if (runOld=="no"){
    
    #open control files for edit
    if (edit_Parameters=="yes"){openParameters(path_user,results_directoryName)}
    if (edit_DesignMatrix=="yes"){openDesign(path_user,results_directoryName)}
    if (edit_dataDictionary=="yes"){openVarnames(path_user,results_directoryName)}
    
    #define testDir and testPhi for Bayesian
    if (if_Bayesian=="yes"){
      #read parameter file to get Bayesian type (#if not CAR add later)
      parameters<-read.csv(paste(path_user,"/",results_directoryName,"/parameters.csv",sep=""),
                           sep=csv_columnSeparator,dec=csv_decimalSeparator)
      #      testPhi<-sum(ifelse(is.na(parameters$phierarch),0,parameters$phierarch))
      testPhi<-sum(ifelse(parameters$phierarch>=3,1,0))
      testPhi<-ifelse(testPhi==0,"NH","H")
      
      #set ifstatespace and ifadust
      if (regexpr("STSP",modelBayesType)>0){
        ifstatespace <- 1   # NHSTSP
        if (substr(testPhi,1,1)=="H") {ifstatespace <- 2}   # HSTSP
        ifadjust <- 0
      }else{ 
        ifstatespace <- 0
        ifadjust <- 1
      }
      
      
      testPhi<-modelBayesType
      #      }
      remove(parameters)
      testDir<-paste(path_user,"/",results_directoryName,"/bayes",testPhi,"_",run_id,"/",sep="")
      
      #set auto_scaling on
      if_auto_scaling<-"yes"
      
    }else{
      testPhi<-""
      testDir<- paste(path_user,"/",results_directoryName,"/",run_id,"/",sep="") 
      
      #set bayes parameters 0 if no bayesian
      ifstatespace <- 0
      ifadjust <- 0
    } 
    
    #Questions for user 
    {removeObjects(c("saved","runScript","run2","runOld",
                     "data1","GeoLines","lineShape","polyShape","data1_priorImport",
                     "subdata","BootBetaest","predict.list","BootUncertainties",
                     "sparrowEsts","DataMatrix.list","DataMatrixEstimate.list","HesResults","JacobResults"))
      if (activeFile==""){
        message("Please select current control.R file.  Browser window may appear behind Rstudio.")
        activeFile<-file.choose()
        assign("path_user",dirname(dirname(activeFile)),envir = .GlobalEnv)
      }
      saved<-isScriptSaved(activeFile,testDir)
      assign("saved",saved,envir = .GlobalEnv)
      if (!saved){
        cat("Please save active control file :\n",activeFile,"\nRun Execution Terminated.")
      }
      if (saved){
        if(if_Bayesian=="yes"){
          #read parameter file to get Bayesian type (#if not CAR add later)
          parameters<-read.csv(paste(path_user,"/",results_directoryName,"/parameters.csv",sep=""),
                               sep=csv_columnSeparator,dec=csv_decimalSeparator)
          #      testPhi<-sum(ifelse(is.na(parameters$phierarch),0,parameters$phierarch))
          testPhi<-sum(ifelse(parameters$phierarch>=3,1,0))
          testPhi<-ifelse(testPhi==0,"NH","H")
          
          if (substr(testPhi,1,1)!=substr(modelBayesType,1,1)){
            if (substr(modelBayesType,1,1)=="N"){
              message("THE CONTROL SETTING modelBayesType INDICATES A NON-HIERARCHICAL MODEL, \n
BUT SETTINGS IN THE parameters.csv FILE INDICATES A HIERARCHICAL MODEL\nPLEASE INDICATE A CONSISTENT BAYESIAN MODEL TYPE\n
RUN EXECUTION TERMINATED")
            }else{
              message("THE CONTROL SETTING modelBayesType INDICATES A HIERARCHICAL MODEL, \n
BUT SETTINGS IN THE parameters.csv FILE INDICATES A NON-HIERARCHICAL MODEL\nPLEASE INDICATE A CONSISTENT BAYESIAN MODEL TYPE\n
RUN EXECUTION TERMINATED")
            }
            
            runOld<-"yes"
            exit <- function() {
              .Internal(.invokeRestart(list(NULL, NULL), NULL))
            }
            exit()
          }else{
            testPhi<-modelBayesType
          }
          remove(parameters)
        }
        #set path_main
        path_main<-path_master
        #set default values for any missing required mapping settings
        setMapDefaults(settings)
        
        ##test for invalid settings
        #source(paste(path_main,.Platform$file.sep,"R",.Platform$file.sep,"testSettings.R",sep=""))
        badSettings<-testSettings(settings,saved)
        if (nrow(badSettings)!=0){
          runScript<-"no"
          assign("runScript",runScript,envir = .GlobalEnv)
          cat("\n \n")
          print(badSettings)
          cat("\n \n")
          cat("Please fix all invalid settings.\nRun Execution Terminated.")
          cat("\n \n")
        }else{
          #make global paths
          makePaths(path_user,path_master,path_bayesmain,run_id,results_directoryName,
                    data_directoryName,gis_directoryName,if_Bayesian,testPhi)
          
          #rename control files
          runScript<-"yes"
          assign("runScript",runScript,envir = .GlobalEnv)
          #generate input lists
          # source(paste(path_main,.Platform$file.sep,"R",.Platform$file.sep,"generateInputLists.R",sep=""))
          updateSettings<-lapply(ls(envir = .GlobalEnv)[which(ls(envir = .GlobalEnv) %in% c(getCharSett(),
                                                                                            getNumSett(),
                                                                                            getOptionSett(),
                                                                                            getShortSett(),
                                                                                            getYesNoSett()))], get)
          names(updateSettings)<-ls(envir = .GlobalEnv)[which(ls(envir = .GlobalEnv) %in% 
                                                                c(getCharSett(),getNumSett(),getOptionSett(),getShortSett(),getYesNoSett()))]
          generateInputLists(updateSettings)
          unPackList(lists = list(file.output.list = file.output.list,
                                  class.input.list = class.input.list,
                                  min.sites.list = min.sites.list,
                                  scenario.input.list = scenario.input.list,
                                  estimate.input.list = estimate.input.list,
                                  mapping.input.list = mapping.input.list),
                     parentObj = list(NA,
                                      NA,
                                      NA,
                                      NA,
                                      NA,
                                      NA))
          
          #create initial varnames
          if (create_initial_dataDictionary=="yes"){
            createInitialDataDictionary(file.output.list,input_data_fileName,
                                        create_initial_parameterControlFiles)
            exit() 
          }
          #create initial design matrix and betas files
          if (create_initial_parameterControlFiles=="yes"){
            createInitialParameterControls(file.output.list,batch_mode)
            exit() 
          }
          
          
          if (runScript=="yes"){
            
            #test for sparrowNames found in parameters.csv but NOT in dataDictionary.csv and/or design_matrix.csv
            #terminate if missing found
            addVars(file.output.list,if_Bayesian, batch_mode)
            
            #create binary maps
            if (if_create_binary_maps=="yes"){
              setupMaps(file.output.list,mapping.input.list,batch_mode,RSPARROW_errorOption)
            }
            #create output directories
            dirCreated<-createDirs(file.output.list,if_userModifyData,
                                   if_Bayesian,batch_mode)
            
            
            #delete old files if_estimate or if_estimate_simulation
            if (if_estimate=="yes" | if_estimate_simulation=="yes"){
              deleteFiles(path_results)
            }
            
            ##############################################################
            if (batch_mode=="no"){    
              {cat("\n \n")
                #run2<-ifelse(run_dataImport=="yes" & load_previousDataImport=="no",1,0)
                run2<-ifelse(load_previousDataImport=="no",1,0)
                assign("run2",run2,envir = .GlobalEnv)
                cat("RSPARROW MODEL NAME: ",run_id,sep="")
                cat("\n \n")
                if (if_Bayesian=="yes"){
                  if (testPhi=="H"){
                    cat("BAYESIAN HIERARCHICAL MODEL")
                    cat("\n \n")
                  }else if (testPhi=="NH"){
                    cat("BAYESIAN NON-HIERARCHICAL MODEL")
                    cat("\n \n")
                  }
                }
                if (select_scenarioReachAreas=="yes"){
                  cat("SCENARIO NAME: ",scenario_name,sep="")
                  cat("\n \n")
                }
                cat("OUTPUT DIRECTORY: ",path_results,sep="")
                cat("\n \n")
                if (run2==1){
                  dataInputPrep(#for readData
                    file.output.list,input_data_fileName,
                    #for checkData1NavigationVars
                    if_reverse_hydseq,
                    #for createVerifyNavigationVars
                    if_verify_demtarea,calculate_reach_attribute_list,
                    mapping.input.list,
                    if_Bayesian,
                    #for all
                    batch_mode)
                  
                }#if run2=yes
                if (load_previousDataImport=="yes"){
                  fileName<-strsplit(path_results,.Platform$file.sep)[[1]]
                  fileName<-paste(fileName[1:length(fileName)-1],collapse = .Platform$file.sep)
                  fileName<-paste(fileName,.Platform$file.sep,gsub(".csv","",input_data_fileName),"_priorImport",sep="")
                  #check if file exists
                  if (file.exists(fileName)){
                    load(file=fileName)  
                  }else{
                    message(paste("ERROR : ",fileName," binary file NOT FOUND\n SET load_previousDataImport<-'no'.\n RUN EXECUTION TERMINATED.",sep=""))
                    errorOccurred("executeRSPARROW.R",batch_mode)
                  }
                  
                  
                }
              }#wait for run2 selection
              ###############################################################
              #runRsparrow
              #source(paste(path_main,.Platform$file.sep,"R",.Platform$file.sep,"startModelRun.R",sep=""))
              startModelRun(file.output.list,
                            if_estimate,if_estimate_simulation,
                            if_boot_estimate,if_boot_predict,enable_interactiveMaps,
                            #createSubdataSorted
                            filter_data1_conditions,data1,
                            #applyUserModify
                            if_userModifyData,
                            data_names,
                            #checkClassificationVars
                            class.input.list,
                            #selectCalibrationSites
                            min.sites.list,
                            #selectValidationSites
                            if_validate,iseed,pvalidate,
                            #findMinMaxLatLon
                            mapping.input.list,
                            #controlFileTasksModel
                            estimate.input.list,
                            if_predict,biters,
                            scenario.input.list,
                            #modelCompare
                            compare_models,modelComparison_name,if_spatialAutoCorr,
                            #shinyMap2
                            add_vars,
                            
                            #Bayesian arguments
                            bayes.input.list,     # added 7-9-2018
                            path_rstan,
                            # Maximum tree depth as control on leapfrog steps by the NUTS sampler
                            maxTree,
                            adapt_delta,
                            # Settings for State-Space model
                            ifstatespace,     # 0=Non-state space model (do not include process error), 1=State-space model (include process error)
                            ifadjust,        # 1=Non-state space model (monitoring adjustment applied),0=State-space model
                            
                            batch_mode,
                            RSPARROW_errorOption)
              
              #add to run shiny independently
              shinyArgs<-named.list(file.output.list,map_uncertainties,BootUncertainties,
                                    data_names,mapping.input.list,
                                    #predict.list,
                                    subdata,SelParmValues,
                                    #site attr
                                    sitedata,
                                    #scenarios
                                    estimate.list,
                                    ConcFactor,DataMatrix.list,
                                    reach_decay_specification,reservoir_decay_specification,scenario.input.list,
                                    #scenarios out
                                    add_vars,
                                    #batchError
                                    batch_mode)
              
              save(shinyArgs, file= paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"shinyArgs"))
              
              #remove unnecessary objects from workspace
              removeObjects(c("run2","saved","runScript","runRsparrow","dmatrixin","map_uncertainties"))
              
            }else{#batch run
              cat("\n \n")
              run2<-1
              assign("run2",run2,envir = .GlobalEnv)
              save(list = c(as.character(outputSettings(file.output.list,FALSE)$setting),
                            "RSPARROW_errorOption","runScript","run2","testPhi","ifstatespace",ls()[(regexpr("path_",ls())>0)],
                            ls()[(regexpr("file_",ls())>0)],"bayes.input.list","ifadjust",
                            "estimate.input.list","mapping.input.list",
                            "file.output.list","class.input.list","min.sites.list","scenario.input.list",
                            "path_results","path_data","path_gis","path_rstan"),
                   file=paste(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData",sep=""))
              
              system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batchRun.R",sep="")),sep=""), wait = FALSE, invisible = FALSE)
              cat("Running RSPARROW in batch mode.")
              
              removeObjects(c("run2","saved","runScript","runRsparrow","dmatrixin","map_uncertainties"))
              
            }
          }#if runScript="yes"
          
        }#if no invalid settings
      }#if saved
    }#wait for saved selection 
    
  }#runOld
  
  
}#runNOw = yes




