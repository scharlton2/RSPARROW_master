#'@title shinyMap2
#'@description Modular Shiny app that allows the user to interactively generate Stream, 
#'            Catchment, and Site Attribute maps, as well as execute Source Change Scenarios Uses libraries shiny, 
#'            sp, data.table, shinyWidgets, stringr, and rhandsontable Uses subroutines: 
#'            setup routines : createInteractiveChoices, createInteractiveScenarioChoices, createRTables, UIs : 
#'            streamCatch, shinySiteAttr, shinyScenarios MODS : compileALL, selectAll, updateVariable, 
#'            shinyScenariosMod, goShinyPlot \cr \cr
#'Executed By: \itemize{\item runShiny.R
#'             \item startModelRun.R} \cr
#'Executes Routines: \itemize{\item compileALL.R
#'             \item createInteractiveChoices.R
#'             \item createRTables.R
#'             \item goShinyPlot.R
#'             \item handsOnMod.R
#'             \item selectAll.R
#'             \item shapeFunc.R
#'             \item shinyScenarios.R
#'             \item shinyScenariosMod.R
#'             \item shinySiteAttr.R
#'             \item streamCatch.R
#'             \item testCosmetic.R
#'             \item testRedTbl.R
#'             \item unPackList.R
#'             \item updateVariable.R
#'             \item validCosmetic.R} \cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param map_uncertainties Vector of user selected uncertainty parameters to map, if 
#'       uncertainty analysis was not run NA
#'@param BootUncertainties Uncertainty values if available, if uncertainty analysis was not 
#'       run NA
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param subdata data.frame input data (subdata)
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param estimate.list list output from `estimate.R`
#'@param ConcFactor the concentration conversion factor, computed as Concentration = load / 
#'       discharge * ConcFactor
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@param dlvdsgn design matrix imported from design_matrix.csv
#'@param reach_decay_specification the SAS IML reach decay function code from sparrow_control
#'@param reservoir_decay_specification the SAS IML reservoir decay function code from 
#'       sparrow_control
#'@param scenario.input.list list of control settings related to source change scenarios
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@param RSPARROW_errorOption yes/no control setting indicating where the RPSARROW_errorOption 
#'                            should be applied
#'@return `outshinyInput`  the Shiny Input list with hottables as dataframes and cosmetic 
#'            mapping settings as list objects



shinyMap2<-function(
  #stream/catchment
  file.output.list, map_uncertainties,BootUncertainties,
  data_names,mapping.input.list,
  #predict.list,
  subdata,SelParmValues,
  #site attr
  sitedata,
  #scenarios
  estimate.list,estimate.input.list,
  ConcFactor,DataMatrix.list,dlvdsgn,
  reach_decay_specification,reservoir_decay_specification,
  scenario.input.list,if_predict,
  #scenarios out
  add_vars,
  #batchError
  batch_mode,
  RSPARROW_errorOption){
  

  
  suppressWarnings(suppressMessages(library(shiny)))
  suppressWarnings(suppressMessages(library(shinycssloaders)))
  suppressWarnings(suppressMessages(library(sp)))
  suppressWarnings(suppressMessages(library(sf)))
  suppressWarnings(suppressMessages(library(data.table)))
  suppressWarnings(suppressMessages(library(shinyWidgets)))
  suppressWarnings(suppressMessages(library(stringr)))
  suppressWarnings(suppressMessages(library(rhandsontable)))
  
  suppressWarnings(suppressMessages(library(leaflet)))
  suppressWarnings(suppressMessages(library(leaflet.extras)))
  suppressWarnings(suppressMessages(library(htmlwidgets)))
  suppressWarnings(suppressMessages(library(htmltools)))
  suppressWarnings(suppressMessages(library(plotly)))
  suppressWarnings(suppressMessages(library(mapview)))
  suppressWarnings(suppressMessages(library(magrittr)))
  suppressWarnings(suppressMessages(library(gplots)))
  suppressWarnings(suppressMessages(library(ggplot2)))
  suppressWarnings(suppressMessages(library(gridExtra)))
  
  unPackList(lists = list(file.output.list = file.output.list,
                          scenario.input.list = scenario.input.list,
                          mapping.input.list = mapping.input.list),
             parentObj = list(NA,NA, NA)) 
  
  #load predicitons if available
  if (file.exists(paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list"))){
    load(paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list"))
  }
  
  #estimation objects
  if (file.exists(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults"))){
    if (!exists("JacobResults")){
      load(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults"))
    }
  }
  
  
  
  
  
  #set up variable choices
  choices<-createInteractiveChoices(SelParmValues,exists("predict.list"),subdata, data_names, map_uncertainties)
  
  #map type choices
  if (exists("predict.list") & exists("JacobResults")){
    mapTypeChoices<-c("","Stream","Catchment","Site Attributes","Scenarios for Changes in Sources and/or Delivery Variables (e.g., Climate, Land use)")
    selectSources<-as.character(JacobResults$Parmnames[which(JacobResults$btype %in% c("SOURCE","DELIVF"))])
    
    
  }else{
    mapTypeChoices<-c("","Stream","Catchment","Site Attributes")
    selectSources<-""
  }
  
  if ("year" %in% data_names$sparrowNames){
    yearChoices<-c("mean","median","min","max",na.omit(unique(subdata$year)))
  }else{
    yearChoices<-c("")
  }
  
  if ("season" %in% data_names$sparrowNames){
    seasonChoices<-c("mean","median","min","max",na.omit(unique(subdata$season)))
  }else{
    seasonChoices<-c("")
  }
  
  scenarioRtables<-createRTables(selectSources,data_names,mapping.input.list)
  
  
  
  #setup shiny ui
  shinyApp(  ui=shinyUI(
    
    fluidPage(tags$head(
      tags$style("h5{color: red}")),
      titlePanel(
        h1(paste0("RShiny Decision Support System : ",run_id),h5(div(HTML("DO NOT CLICK ON ITEMS ABOVE THIS POINT!"))))),
      
      sidebarLayout(
        sidebarPanel(width=6,
                     h4("SPARROW Interactive Mapping                     "),
                     br(),
                     
                     #top level user input
                     selectInput("batch","Output Mode",c("Interactive","Batch")),
                     selectInput("enablePlotly","Output Map Format",c("static","plotly","leaflet"),selected = ifelse(mapping.input.list$enable_plotlyMaps=="yes","plotly","static")),
                     textOutput("plotlyExplanation"),
                     br(),
                     selectInput("mapType","Map Type",mapTypeChoices),
                     
                     if (checkDynamic(subdata)){ 
                     conditionalPanel("input.mapType!='Scenarios for Changes in Sources and/or Delivery Variables (e.g., Climate, Land use)'",
                     h5(HTML("<font color = 'black'><strong>Dynamic Mapping Settings</strong></font>")),
                     h6("A maximum of 4 maps can be displayed. The maps are based on selections of individual 'year(s)' and/or 'season(s)', or alternatively, the selection of a summary metric (e.g., 'mean', 'min'), which is either computed across all 'year(s)' for each 'season(s)' selection (up to a maximum of 4 seasons) or computed across all 'season(s)' for each 'year(s)' selection. Note that the selection of a summary metric (e.g., mean) for both 'year(s)' and 'season(s)' will display a single map, based on the computation of the metric across all available observations for the period of record."))},
                     if (checkDynamic(subdata)){ 
                     conditionalPanel("input.mapType=='Scenarios for Changes in Sources and/or Delivery Variables (e.g., Climate, Land use)'",
                                      h5(HTML("<font color = 'black'><strong>Dynamic Mapping Display Settings</strong></font>")),
                                      h6("For 'Change Scenarios', the 'year' and/or 'season' selection(s) control the MAPPING DISPLAY ONLY. To display mapped results for a change scenario, select a display timestep ('year' and/or 'season') that is consistent with the model simulation timestep for the scenario. Use the 'year' and/or 'season' display selection(s) as the timestep in the model simulations by executing the menu options below for 'Run Scenario using...' and/or 'Select Target Reach Watersheds'. The simulation timestep is associated with the base or reference conditions for an explanatory variable(s) during the calibration time period that are either altered (i.e., percentage-change scenarios) or are used as a benchmark to facilitate comparisons with future water-quality predictions based on forecasted changes in watershed conditions (i.e., scenarios with changed values of the explanatory variables entered from an external CSV file). Additional year(s) and/or season(s) may be selected for display to compare historical results with those for the change scenario simulations."))},
                     if (checkDynamic(subdata)){ 
                     fluidRow(dropdownButton(circle = FALSE,
                                      label = "year(s)",
                                      inputId = "yearDrop",
                                      checkboxGroupInput("yearSelect", "Select year(s) to Map", 
                                                         yearChoices,
                                                         selected = as.character(mapping.input.list$map_years),
                                                         inline=FALSE)),
                     dropdownButton(circle = FALSE,
                                    label = "season(s)",
                                    inputId = "seasonDrop",
                                    checkboxGroupInput("seasonSelect", "Select season(s) to Map", 
                                                       seasonChoices,
                                                       selected = as.character(mapping.input.list$map_seasons),
                                                       inline=FALSE)),
                      )},
                     
                     br(),
                     
                     #Stream and Catchment arguments
                     streamCatch("nsStreamCatch", input, choices, map_uncertainties,sitedata,add_plotlyVars),
                     
                     #site Attribute arguments
                     shinySiteAttr("nsSiteAttr",input,choices,sitedata,add_plotlyVars),
                     
                     #scenarios arguments
                     shinyScenarios("nsScenarios",input,choices,sitedata,add_plotlyVars, scenario.input.list),
                     
                     #output shape file ifBatch
                     shapeFunc("nsBatch",input),
                     

                     conditionalPanel(
                       condition = "input.batch=='Interactive'",
                       fluidRow(
                         actionButton("goPlot","Generate Map"),
                         actionButton("savePDF", "Save Map"))       
                       
                     ),
                     
                     conditionalPanel(
                       condition = "input.batch=='Batch'",
                       actionButton("batchPlot","Save Map(s)")      
                     )
        ),
        mainPanel(width = 6,
                 uiOutput("plot")
                  
        )
      )))#end ui function
    ,
    
    ################################################################
    ###############################################################
    ###############################################################
    
    server=shinyServer(function(input, output,session) {
      #update red labels
      observe({
        if (input$mapType!=""){
          updateSelectInput(session, "mapType",
                            label = "Map Type"
          )
        }
        
        #no plotly catchment maps in interactive mode
        currentSelect<-isolate(input$enablePlotly)
        if (input$mapType=="Catchment" & input$batch=="Interactive"){
          updateSelectInput(session,"enablePlotly","Output Map Format",c("static","leaflet"),selected = "static")
          output$plotlyExplanation<-renderText({"Plotly not available for catchment maps in Interactive mode due to long processing time\n to get interactive catchment maps select Batch mode and enable plotly"
          })
        }else if (grepl("Scenarios",input$mapType)){
          if (length(input$`nsScenarios-outType`)==0){
            updateSelectInput(session,"enablePlotly","Output Map Format",c("static","plotly","leaflet"),selected = currentSelect)
            output$plotlyExplanation<-renderText({"Plotly Maps will take longer to render in Interactive mode"})
          }else{
            if (input$`nsScenarios-outType`=="Catchment" & input$batch=="Interactive"){
              updateSelectInput(session,"enablePlotly","Output Map Format",c("static","leaflet"),selected = "static")
              output$plotlyExplanation<-renderText({"Plotly not available for catchment maps in Interactive mode due to long processing time\n to get interactive catchment maps select Batch mode and enable plotly"})
            }else{
              updateSelectInput(session,"enablePlotly","Output Map Format",c("static","plotly","leaflet"),selected = currentSelect)
              output$plotlyExplanation<-renderText({"Plotly Maps will take longer to render in Interactive mode"})
            }
          }
        }else{
          updateSelectInput(session,"enablePlotly","Output Map Format",c("static","plotly","leaflet"),selected = currentSelect)
          output$plotlyExplanation<-renderText({"Plotly Maps will take longer to render in Interactive mode"})
        }
        
        
      })  
      
      
      
      #select all and clear all buttons in drop downs 
      observe({        
        if (input$batch=="Batch"){
          if (input$mapType %in% c("Stream","Catchment")){
            lapply(1:length(as.character(unique(choices$category))), function(c) {
              category<-as.character(unique(choices$category))[c]
              if (category!="Prediction Uncertainties"){
                nsName<-paste0("ns",tolower(str_split(category," ")[[1]][1]),"Drop")
              }else{
                nsName<-"nsuncertaintyDrop"
              }
              callModule(selectAll,nsName, category = category, choices = choices)
            })
          }else{
            choicesScen<-choices[which(!choices$category %in% c("Data Dictionary Variable","Prediction Uncertainties") & regexpr("Monitoring-adjusted",choices$definition)<0),]
            ratioChoices<-data.frame(category = c("Relative Change in Load","Relative Change in Load"),
                                     variable = c("ratio_total","ratio_inc","percent_total","percent_inc"),
                                     definition = c("Ratio of the changed total load to the baseline (unchanged) total load",
                                                    "Ratio of the changed incremental load to the baseline (unchanged) incremental load"))
            choices$category<-ifelse(choices$category=="Load Predictions","Load Predictions for Changed Model Variables",
                                     ifelse(choices$category=="Yield Predictions","Yield Predictions for Changed Model Variables",choices$category))
            choicesScen<-rbind(choicesScen,ratioChoices)
            
            lapply(1:length(as.character(unique(choicesScen$category))), function(c) {
              category<-as.character(unique(choicesScen$category))[c]
              nsName<-paste0("nsScen",tolower(str_split(category," ")[[1]][1]),"Drop")
              callModule(selectAll,nsName, category = category, choices = choicesScen)
            }) 
          }
          if (input$mapType %in% c("Stream","Catchment","Site Attributes")){
            callModule(selectAll,"nsattrDrop", category = "Data Dictionary Variable", choices = choices)
          }
          
        }
      })
      
      #update variable lists according to variable type selection in interactive mode
      observe({
        if (input$batch=="Interactive" & input$mapType %in% c("Stream","Catchment")){     
          callModule(updateVariable,"nsStreamCatch", choices= choices, mapType = input$mapType)
          
        }else if (input$batch=="Interactive" & input$mapType == "Site Attributes"){
          callModule(updateVariable,"nsSiteAttr", choices= choices, mapType = input$mapType)
        }else{
          choicesScen<-choices[which(!choices$category %in% c("Data Dictionary Variable","Prediction Uncertainties") & regexpr("Monitoring-adjusted",choices$definition)<0),]
          ratioChoices<-data.frame(category = c("Relative Change in Load","Relative Change in Load"),
                                   variable = c("ratio_total","ratio_inc","percent_total","percent_inc"),
                                   definition = c("Ratio of the changed total load to the baseline (unchanged) total load",
                                                  "Ratio of the changed incremental load to the baseline (unchanged) incremental load"))
          choices$category<-ifelse(choices$category=="Load Predictions","Load Predictions for Changed Model Variables",
                                   ifelse(choices$category=="Yield Predictions","Yield Predictions for Changed Model Variables",choices$category))
          choicesScen<-rbind(choicesScen,ratioChoices)
          callModule(updateVariable,"nsScenarios", choices= choicesScen, mapType = input$mapType)
        }
      })
      
      
      #rTables 
      
      
      observe({
        if (grepl("Scenarios",input$mapType)){
          callModule(shinyScenariosMod,"nsScenarios",scenarioRtables,path_results,scenario.input.list, mapping.input.list)
          
        }else if (input$mapType %in% c("Stream","Catchment")){
          testRow<-testCosmetic(input, output, session, 
                                DF = as.data.frame(scenarioRtables$cosmeticPred),mapType = input$mapType,
                                scenario.input.list, mapping.input.list)$rowNums
          callModule(validCosmetic,"nsStreamCatch-nsCosmetic", 
                     DF = as.data.frame(scenarioRtables$cosmeticPred),rowNum = testRow)
          
        }else if (input$mapType == "Site Attributes"){
          testRow<-testCosmetic(input, output, session, 
                                DF = as.data.frame(scenarioRtables$cosmeticSite),mapType = input$mapType,
                                scenario.input.list, mapping.input.list)$rowNums
          callModule(validCosmetic,"nsSiteAttr-nsCosmetic", 
                     DF = as.data.frame(scenarioRtables$cosmeticSite),rowNum = testRow)
          
        }
      })
      

      
      
      
      #interactive plot
        p1<-eventReactive(input$goPlot, {
          gc()
          if(exists("errOccured")){remove(errOccured) }
          
          tryCatch({
          
                    output$plotOne<-NULL
                    output$plotlyPlot<-NULL
                    output$leafPlot<-NULL
                    gc()

                    suppressWarnings(remove(p,envir = .GlobalEnv))
                    suppressWarnings(remove(currentP))
                    
          #test bad Settings
          badSettings<-as.data.frame(matrix(0,ncol=4,nrow=0))
          names(badSettings)<-c("Setting","CurrentValue","Type","Test")

          errMsg<-NA
          if (input$mapType %in% c("Stream","Catchment")){
            badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticPred),mapType =input$mapType,
                                      scenario.input.list, mapping.input.list)$badSettings
          }else if (input$mapType == "Site Attributes"){
            badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticSite),mapType =input$mapType,
                                      scenario.input.list, mapping.input.list)$badSettings
          }else{
            errMsg1<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$sourceRed))$errMsg
            errMsg2<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDF))$errMsg
            errMsg3<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDFno))$errMsg
            
            
            errMsg<-na.omit(c(errMsg1,errMsg2, errMsg3))
            if (length(errMsg)==0){
              errMsg<-NA
            }else{
              errMsg<-errMsg[1]
            }
            
            badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticScen), "Source Change Scenarios",
                                      scenario.input.list, mapping.input.list)$badSettings
            
          }
          
         
              currentP<-goShinyPlot(input, output, session, choices,"goPlot", badSettings,errMsg,
                      file.output.list,map_uncertainties,BootUncertainties,
                      data_names,mapping.input.list,
                      #predict.list,
                      subdata,SelParmValues,
                      #site attr
                      sitedata,estimate.list,estimate.input.list,#Mdiagnostics.list,
                      #scenarios
                      JacobResults,
                      ConcFactor,DataMatrix.list,dlvdsgn,
                      reach_decay_specification,reservoir_decay_specification,
                      scenario.input.list,if_predict,
                      #scenarios out
                      add_vars,
                      #batchError
                      batch_mode,
                      RSPARROW_errorOption)

    
          
        
          #print plot size
          env <- environment()
          objs<-data.frame(
            object = ls(env),
            size = unlist(lapply(ls(env), function(x) {
              object.size(get(x, envir = env, inherits = FALSE))})))
          
          print(paste0("Plot size : ",objs[which(objs$object=="currentP"),]$size))
          

          if (input$enablePlotly=="static"){ 
            
            output$plot<-renderUI({
              plotOutput("plotOne", width=900,height=900) %>% withSpinner(color="#0dc5c1")
            })
            #time plot render
            output$plotOne  <- renderPlot({
              grid.arrange(currentP)
            })

            
          }else if (input$enablePlotly=="plotly"){

            output$plot<-renderUI({
              plotlyOutput("plotlyPlot", width=900,height=900) %>% withSpinner(color="#0dc5c1")
            })

            output$plotlyPlot <- renderPlotly({
              isolate(currentP)
            })
            

          }else if (input$enablePlotly=="leaflet"){
            output$plot<-renderUI({
              leafletOutput("leafPlot", width=900,height=900) %>% withSpinner(color="#0dc5c1")
            })

            output$leafPlot<-renderLeaflet({
              isolate(currentP)
            })
          }
          }, error = function(e) {
            cat("Handling error:\n", paste(e, collapse = "\n"))
            errOccured<<-TRUE
          }, warning = function(w) {
            cat("Handling warning:\n", paste(w, collapse = "\n"))
          }, finally = { if(exists("errOccured")){
            cat(paste(traceback(),collapse = "\n"))
          }
          })  

        })

        
        observe({

            p1()

})

        
           

        
        
        
      #pdf output

        p2<-eventReactive(input$savePDF, {
         
          #test bad Settings
        badSettings<-as.data.frame(matrix(0,ncol=4,nrow=0))
        names(badSettings)<-c("Setting","CurrentValue","Type","Test")
        errMsg<-NA
        if (input$mapType %in% c("Stream","Catchment")){
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticPred),mapType =input$mapType,
                                    scenario.input.list, mapping.input.list)$badSettings
        }else if (input$mapType == "Site Attributes"){
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticSite),mapType =input$mapType,
                                    scenario.input.list, mapping.input.list)$badSettings
        }else{
          errMsg1<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$sourceRed))$errMsg
          errMsg2<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDF))$errMsg
          errMsg3<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDFno))$errMsg
          
          
          errMsg<-na.omit(c(errMsg1,errMsg2, errMsg3))
          if (length(errMsg)==0){
            errMsg<-NA
          }else{
            errMsg<-errMsg[1]
          }
          
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticScen), "Source Change Scenarios",
                                    scenario.input.list, mapping.input.list)$badSettings
          
        }
        
        goShinyPlot(input, output, session, choices,"savePDF",badSettings, errMsg,
                    file.output.list, map_uncertainties,BootUncertainties,
                    data_names,mapping.input.list,
                    #predict.list,
                    subdata,SelParmValues,
                    #site attr
                    sitedata,estimate.list,estimate.input.list,#Mdiagnostics.list,
                    #scenarios
                    JacobResults,
                    ConcFactor,DataMatrix.list,dlvdsgn,
                    reach_decay_specification,reservoir_decay_specification,
                    scenario.input.list,if_predict,
                    #scenarios out
                    add_vars,
                    #batchError
                    batch_mode,
                    RSPARROW_errorOption)
      })#end save plot p2
        
        observe({

         p2()


        })
        
      
      #batchplot
        p3<-eventReactive(input$batchPlot, {
        #test bad Settings
        badSettings<-as.data.frame(matrix(0,ncol=4,nrow=0))
        names(badSettings)<-c("Setting","CurrentValue","Type","Test")
        errMsg<-NA
        if (input$mapType %in% c("Stream","Catchment")){
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticPred),mapType =input$mapType,
                                    scenario.input.list, mapping.input.list)$badSettings
        }else if (input$mapType == "Site Attributes"){
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticSite),mapType =input$mapType,
                                    scenario.input.list, mapping.input.list)$badSettings
        }else{
          errMsg1<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$sourceRed))$errMsg
          errMsg2<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDF))$errMsg
          errMsg3<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDFno))$errMsg
          
          
          errMsg<-na.omit(c(errMsg1,errMsg2, errMsg3))
          if (length(errMsg)==0){
            errMsg<-NA
          }else{
            errMsg<-errMsg[1]
          }
          
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticScen), "Source Change Scenarios",
                                    scenario.input.list, mapping.input.list)$badSettings
          
        }
        
        goShinyPlot(input, output, session, choices,"batchPlot",badSettings,errMsg,
                    file.output.list, map_uncertainties,BootUncertainties,
                    data_names,mapping.input.list,
                    #predict.list,
                    subdata,SelParmValues,
                    #site attr
                    sitedata,estimate.list,estimate.input.list,#Mdiagnostics.list,
                    #scenarios
                    JacobResults,
                    ConcFactor,DataMatrix.list,dlvdsgn,
                    reach_decay_specification,reservoir_decay_specification,
                    scenario.input.list,if_predict,
                    #scenarios out
                    add_vars,
                    #batchError
                    batch_mode,
                    RSPARROW_errorOption)
  
        
       
        })#end batch plot p3
        
        observe({
          p3()
        })
        
      session$onSessionEnded(function() {
        stopApp()
      }) 
    })#end server function
  )#end shinyApp function
}#end ShinyMap2    




