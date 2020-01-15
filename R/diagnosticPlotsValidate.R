#'@title diagnosticPlotsValidate
#'@description Creates diagnostic plots and maps for validation sites output to 
#'            ~/estimate/(run_id)_validation_plots.pdf, and saves residual maps as shape files. \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item diagnosticMaps.R
#'             \\item unPackList.R\} \\cr
#'@param vsitedata sitedata for validation. Calculated by `subdata[(subdata$vdepvar > 0), ]`
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



diagnosticPlotsValidate <- function(file.output.list,class.input.list,vsitedata.demtarea.class,
                                    vsitedata,vsitedata.landuse,estimate.list,mapping.input.list,add_vars,
                                    batch_mode) {
  
  
  
  #########################
  # Create Global Variables
  #########################
  
  
  # create global variable from list names (mapping.input.list)
  # create global variable from list names (Mdiagnostics.list)
  unPackList(lists = list(mapping.input.list = mapping.input.list,
                          vMdiagnostics.list = estimate.list$vMdiagnostics.list,
                          file.output.list = file.output.list,
                          class.input.list = class.input.list),
             parentObj = list(NA,
                              NA,
                              NA,
                              NA))
  
  filename <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_validation_plots.html",sep="")
  reportPath<-paste0(path_master,"diagnosticPlotsNLLS.Rmd")
  
  
  path_mapAttrChild <- file_path_as_absolute(paste0(path_master,"diagnosticMapAttrChild.Rmd"))
  path_corrChild <- file_path_as_absolute(paste0(path_master,"diagnosticCorrChild.Rmd"))
  path_classvarChild <- file_path_as_absolute(paste0(path_master,"diagnosticClassvarChild.Rmd"))
  path_classLandChild <- file_path_as_absolute(paste0(path_master,"diagnosticClassLandChild.Rmd"))
  path_contiguousChild<- file_path_as_absolute(paste0(path_master,"diagnosticContiguousChild.Rmd"))
  path_diagMapChild<-file_path_as_absolute(paste0(path_master,"diagnosticDiagMapChild.Rmd"))
  rmarkdown::render(
    reportPath, params = list(
      validation = TRUE,
      file.output.list = file.output.list,
      path_mapAttrChild = path_mapAttrChild,
      path_corrChild = path_corrChild,
      path_classvarChild = path_classvarChild,
      path_classLandChild = path_classLandChild,
      path_contiguousChild = path_contiguousChild,
      path_diagMapChild = path_diagMapChild,
      class.input.list = class.input.list,
      sitedata.demtarea.class = vsitedata.demtarea.class,
      sitedata = vsitedata,
      sitedata.landuse = vsitedata.landuse,
      estimate.list = estimate.list,
      mapping.input.list = mapping.input.list,
      Csites.weights.list = NA,
      Cor.ExplanVars.list = NA,
      data_names = data_names,
      add_vars = add_vars,
      batch_mode = batch_mode
    ),
    output_file = filename, quiet = TRUE
  )
  
 
    #output residuals shapefile
    if (outputESRImaps[3]=="yes"){
      Obsyield <- Obs / vsitedata$demtarea
      predictYield <- ppredict / vsitedata$demtarea
      origWaterid<-vsitedata$waterid_for_RSPARROW_mapping
      
      dd <- data.frame(vsitedata,origWaterid,Obs,ppredict,Obsyield,predictYield,pResids,pratio.obs.pred,xlat,xlon)
      
      keeps <- c("waterid","origWaterid","demtarea","rchname","station_id","station_name","staid",classvar[1],"Obs",
                 "ppredict","Obsyield","predictYield","pResids","pratio.obs.pred","xlat","xlon")
      
      validationResidShape <- dd[keeps]
      
      if (length(na.omit(add_vars))!=0){
        add_data<-data.frame(vsitedata[,which(names(vsitedata) %in% add_vars)])
        if (length(add_vars)==1){
          names(add_data)<-add_vars
        }
        validationResidShape<-cbind(validationResidShape,add_data)
      }
      
      validationResidShape <-SpatialPointsDataFrame(validationResidShape[,c("xlon","xlat")],validationResidShape[,which(!names(validationResidShape) %in% c("xlat","xlon"))],proj4string=CRS(CRStext))
      
      if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
        dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
      }
      if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,sep=""))){
        dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,sep=""),showWarnings = FALSE)
      }
      
      maptools::writeSpatialShape(validationResidShape,paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,"validationResidShape",sep=""))
      cat(showWKT(proj4string(validationResidShape)),file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,"validationResidShape.prj",sep="")) 
      
    }
    
  #}  # end check for existence of line map
  
 # dev.off()  # shuts down current graphics device
#  graphics.off()  # shuts down all open graphics devices
  
  
}#end function
