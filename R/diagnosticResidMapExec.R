#'@title diagnosticResidMapExec
#'@description Creates diagnostic Residual maps \cr \cr
#'Executed By: diagnosticPlotsNLLS.Rmd \cr
#'Executes Routines: \itemize{\item diagnosticDiagMapChild.Rmd
#'                              \item aggDynamicMapdata.R
#'                              \item unPackList.R
#'                               \item setupDynamicMaps.R} \cr
#'@param validation TRUE/FALSE indicating whether or not validation is to be run
#'@param existGeolines TRUE/FALSE indicating whether Geolines object exists
#'@param residmapTypes character string indicating what type of residual maps to 
#'                     generate from the following list: "threshold-above",
#'                     "threshold-below","all"
#'@param mapColumn character string name of column with residual data
#'@param strTitle character string map title
#'@param dynamic TRUE/FALSE indicating whether dynamic data is present
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param Resids numeric vector of residuals
#'@param pResids numeric vector of residuals with no monitoring adjustment
#'@param ratio.obs.pred numeric vector ratio of observed vs predicted observations
#'@param pratio.obs.pred numeric vector of observed vs predicted observations with
#'                       no monitoring adjustment
#'@param standardResids numeric vector of standardized residuals
#'@param path_diagnosticDiagMapChild normalized path to diagnosticDiagMapChild.Rmd
#'@param path_outputMapsChild normalized path to outputMapsChild.Rmd
#'@param path_outputMaps normalized path to outputMaps.Rmd
#'@param map_years RSPARROW dynamic user setting indicating how dynamic mapping 
#'                 should proceed with regards to the year timestep.  A numeric 
#'                 vector of years indicates mapping of each year in the vector.  
#'                 A character string equal to "all" indicates that all years 
#'                 present in the data1 file should be mapped. A character string
#'                 from the following list of approved aggregation functions
#'                 c("mean","median","min","max") indicates that the selected
#'                 aggregation function should be applied to the "year" attribute
#'                 to obtain a seasonal mean, median, min or max.  NA indicates that
#'                 the year timestep is not present in data1
#'@param map_seasons RSPARROW dynamic user setting indicating how dynamic mapping 
#'                 should proceed with regards to the season timestep.  A character 
#'                 vector of seasons indicates mapping of each season in the vector.
#'                 Seasons must be from the following list c("winter","spring","summer"
#'                 "fall").A character string equal to "all" indicates that all seasons 
#'                 present in the data1 file should be mapped. A character string
#'                 from the following list of approved aggregation functions
#'                 c("mean","median","min","max") indicates that the selected
#'                 aggregation function should be applied to the "season" attribute
#'                 to obtain an annual mean, median, min or max.  NA indicates that
#'                 the season timestep is not present in data1  
#'@param mapPageGroupBy RSPARROW dynamic user setting indicating grouping variable for 
#'                      maps on a page, select "season","year",or NA for no group
#'@param mapsPerPage number between 1 and 4 indicating maps per page  
#'@param enable_plotlyMaps yes/no indicating whether to enable plotly interactive 
#'                         displays for maps
#'@param add_plotlyVars character vector indicating user selected variables to add to plot hover
#'                      text
#'@param path_gis path to users gis data
#'@param LineShapeGeo character string control settting indicating which binary map file to 
#'       load for the Geolines background layer
#'@param GeoLines Optional geospatial shape file for overlay of lines on output maps
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode


diagnosticResidMapExec<-function(validation,existGeoLines,residmapTypes,mapColumn,strTitle,dynamic,
                                 sitedata, Resids,pResids,ratio.obs.pred,pratio.obs.pred,standardResids,
                                 path_diagnosticDiagMapChild,path_outputMapsChild,path_outputMaps,
                                 map_years,map_seasons,mapPageGroupBy,mapsPerPage,
                                 enable_plotlyMaps,add_plotlyVars,path_gis,LineShapeGeo,GeoLines,
                                 data_names,mapping.input.list,batch_mode){

# Setup GEOLINES basemap, if available

  #8.8.17 if(!is.na(LineShapeGeo)) {
  if(existGeoLines) { 
    
    # residmapTypes<-c("threshold-above","threshold-below","all")
    # mapColumn<-"Resids"
    # 
    # #mapdata<-mapdata
    # strTitle<-"Model Estimation Log Residuals"
    
    
    if (!dynamic){
      mapdata <- data.frame(lat=sitedata$lat,lon=sitedata$lon,Resids,ratio.obs.pred,pResids,pratio.obs.pred,standardResids) 
      rmd <- sapply(
        1:(length(residmapTypes)),
        function(n) {
          
          knit_expand(path_diagnosticDiagMapChild, n = n, mapdata=mapdata)
        }
      )
      rmd <- paste(rmd, collapse = "\n")
      return(rmd)
    }else{#dynamic
      eval(parse(text=paste0("residData<-",mapColumn)))
      MAPID <- eval(parse(text=paste0("sitedata$","waterid_for_RSPARROW_mapping") )) 
      commonvar<-"tempID"
      mapType<-"resid"
      aggFuncs<-c("mean","median","min","max")
      
      if(!identical(NA,map_years)){
      if (identical(map_years,"all") | map_years[1] %in% aggFuncs){
        map_years<-unique(sitedata$year)
      }}
      if(!identical(NA,map_seasons)){
      if (identical(map_seasons,"all") | map_seasons[1] %in% aggFuncs){
        map_seasons<-unique(sitedata$season)
      }}
      
      if(!identical(NA,map_years)){
      if (length(map_years)!=0){
        map_years<-as.numeric(map_years)
      }else if (length(map_years)==0){
        map_years<-NA
      }}
      if(!identical(NA,map_seasons)){
      if (length(map_seasons)==0){
        map_seasons<-NA
      }}
      
      for (n in residmapTypes){
        
        input<-list(var=NA, sizeBatch=NA,size=NA)
        map.list<-n 
        
        agg_map.list<-aggDynamicMapdata(map_years,map_seasons,
                                        enable_plotlyMaps,
                                        add_plotlyVars,
                                        aggFuncs,vvar = residData,MAPID,commonvar,subdata = sitedata)
        unPackList(lists = list(agg_map.list = agg_map.list),
                   parentObj = list(NA)) 
        
        #prep aggdata for plots
        mapdata<-merge(uniqueSubdata,mapdata, 
                       by=names(mapdata)[names(mapdata) %in% names(uniqueSubdata)])
        names(mapdata)[names(mapdata)=="vvar"]<-mapColumn
        if (map_years[1] %in% aggFuncs | map_seasons[1] %in% aggFuncs){
          names(mapdata)[names(mapdata)==commonvar]<-"mapping_waterid"
          add_plotlyVars<-as.character(ifelse(add_plotlyVars=="waterid","mapping_waterid",add_plotlyVars))
        }else{
          names(mapdata)[names(mapdata)==commonvar]<-"waterid_for_RSPARROW_mapping" 
          add_plotlyVars<-as.character(ifelse(add_plotlyVars=="waterid","waterid_for_RSPARROW_mapping",add_plotlyVars))
        }
        
        #mapdata$Resids<-Resids
        eval(parse(text=paste0("mapdata$",mapColumn,"<-",mapColumn)))
        
        
        plots<-setupDynamicMaps(mapdata,map_years,map_seasons,
                                mapPageGroupBy,mapsPerPage, Rshiny=FALSE, 
                                enable_plotlyMaps)
        mapLoopInput.list<-list(plots = plots,
                                map.list=map.list,
                                input = input,
                                path_gis = path_gis, 
                                sitedata = sitedata, 
                                LineShapeGeo = LineShapeGeo,
                                data_names = data_names,
                                Rshiny = FALSE,
                                #regular
                                mapColumn = mapColumn,
                                dmapfinal = mapdata,
                                GeoLines = GeoLines,
                                mapping.input.list = mapping.input.list,
                                strTitle = strTitle,
                                batch_mode = batch_mode)
        if (!validation){
        if (!dir.exists(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep))){
          dir.create(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep))
        }
        if (!dir.exists(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep,"residualMaps",.Platform$file.sep))){
          dir.create(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep,"residualMaps",.Platform$file.sep))
        }
        filename<- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"diagnosticPlotsNLLS_dynamic",.Platform$file.sep,"residualMaps",.Platform$file.sep,paste0(gsub(" ","",strTitle),"_",n),".html")
        }else{
          if (!dir.exists(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"validation_plots_dynamic",.Platform$file.sep))){
            dir.create(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"validation_plots_dynamic",.Platform$file.sep))
          }
          if (!dir.exists(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"validation_plots_dynamic",.Platform$file.sep,"residualMaps",.Platform$file.sep))){
            dir.create(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"validation_plots_dynamic",.Platform$file.sep,"residualMaps",.Platform$file.sep))
          }
          filename<- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"validation_plots_dynamic",.Platform$file.sep,"residualMaps",.Platform$file.sep,paste0(gsub(" ","",strTitle),"_",n),".html")
          
        }
        rmdTitle<-paste0(gsub(" ","",strTitle),"_",n)
        
        rmarkdown::render(path_outputMaps,
                          params = list(
                            rmdTitle = rmdTitle,
                            mapType = "resid",
                            mapLoopInput.list = mapLoopInput.list,
                            path_outputMapsChild = path_outputMapsChild
                          ),
                          envir = new.env(),
                          output_file = filename, quiet = TRUE
        )           
        
      }#resid map types 
      
    }#dynamic
  }#existGeolines

}#end func