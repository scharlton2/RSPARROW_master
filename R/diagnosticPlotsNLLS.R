#'@title diagnosticPlotsNLLS
#'@description Creates diagnostic plots and maps output to 
#'            ~/estimate/(run_id)_diagnostic_plots.pdf, and saves residual maps as shape files. \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item diagnosticMaps.R
#'             \\item mapSiteAttributes.R
#'             \\item unPackList.R\} \\cr
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0), ]`
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



diagnosticPlotsNLLS<- function(file.output.list,class.input.list,sitedata.demtarea.class,
                               sitedata,sitedata.landuse,estimate.list,mapping.input.list,Csites.weights.list,
                               Cor.ExplanVars.list,
                               data_names,add_vars,batch_mode) {
  
  
  
  #########################
  # Create Global Variables
  #########################
  
  
  
  # create global variable from list names (Mdiagnostics.list)
  unPackList(lists = list(mapping.input.list = mapping.input.list,
                          Mdiagnostics.list = estimate.list$Mdiagnostics.list,
                          file.output.list = file.output.list,
                          class.input.list = class.input.list),
             parentObj = list(NA,
                              NA,
                              NA,
                              NA))
  
  # contiguous class variables by sites
  class <- array(0,dim=c(nrow=nrow(sitedata),ncol=length(classvar))) 
  for (k in 1:length(classvar)) { 
    for (i in 1:nrow(sitedata)) {
      class[i,k] <- as.numeric(eval(parse(text=paste("sitedata$",classvar[k],"[",i,"]",sep=""))))
    } 
  } 
  
  # Create 'classvar2' for plotting landuse non-contiguous class
  #   following code executes:  classvar2 <- c("forest_pct","agric_pct","urban_pct","shrubgrass_pct")
  if(!is.na( class_landuse[1])){
    classvar2 <- character(length(class_landuse))
    for (i in 1:length(class_landuse)) {
      classvar2[i] <- paste(class_landuse[i],"_pct",sep="")
    }
  }
  
  
 # filename <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_plots.pdf",sep="")
  filename <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_plots.html",sep="")
  reportPath<-paste0(path_master,"diagnosticPlotsNLLS.Rmd")

path_mapAttrChild <- file_path_as_absolute(paste0(path_master,"diagnosticMapAttrChild.Rmd"))
path_corrChild <- file_path_as_absolute(paste0(path_master,"diagnosticCorrChild.Rmd"))
path_classvarChild <- file_path_as_absolute(paste0(path_master,"diagnosticClassvarChild.Rmd"))
path_classLandChild <- file_path_as_absolute(paste0(path_master,"diagnosticClassLandChild.Rmd"))
path_contiguousChild<- file_path_as_absolute(paste0(path_master,"diagnosticContiguousChild.Rmd"))

  rmarkdown::render(
    reportPath, params = list(
      file.output.list = file.output.list,
      path_mapAttrChild = path_mapAttrChild,
      path_corrChild = path_corrChild,
      path_classvarChild = path_classvarChild,
      path_classLandChild = path_classLandChild,
      path_contiguousChild = path_contiguousChild,
      class.input.list = class.input.list,
      sitedata.demtarea.class = sitedata.demtarea.class,
      sitedata = sitedata,
      sitedata.landuse = sitedata.landuse,
      estimate.list = estimate.list,
      mapping.input.list = mapping.input.list,
      Csites.weights.list = Csites.weights.list,
      Cor.ExplanVars.list = Cor.ExplanVars.list,
      data_names = data_names,
      add_vars = add_vars,
      batch_mode = batch_mode
    ),
    output_file = filename, quiet = TRUE
  )
  
  #shell.exec(filename)
  
  
  #output siteAttr shapefile
  if (outputESRImaps[4]=="yes"){
    siteAttrshape<-data.frame(waterid = sitedata$waterid,
                              originalWaterid = sitedata$waterid_for_RSPARROW_mapping,
                              xlat,xlon)
    for (s in 1:length(map_siteAttributes.list)){
      if (length(names(sitedata)[which(names(sitedata)==map_siteAttributes.list[s])])!=0){
        siteAttr<-eval(parse(text= paste("data.frame(",map_siteAttributes.list[s],"=sitedata$",map_siteAttributes.list[s],")",sep="")))
        siteAttrshape<-data.frame(siteAttrshape,siteAttr)
        names(siteAttrshape)[length(siteAttrshape)]<-map_siteAttributes.list[s]
      }
    }
    
    
    
    siteAttrshape<-SpatialPointsDataFrame(siteAttrshape[,c("xlon","xlat")],siteAttrshape[,which(!names(siteAttrshape) %in% c("xlat","xlon"))],proj4string=CRS(CRStext))
    
    if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
      dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
    }
    if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,sep=""))){
      dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,sep=""),showWarnings = FALSE)
    }
    
    maptools::writeSpatialShape(siteAttrshape,paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,"siteAttrshape",sep=""))
    cat(showWKT(proj4string(siteAttrshape)),file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,"siteAttrshape.prj",sep="")) 
  }
  
  #output residuals shapefile
  if (outputESRImaps[3]=="yes"){
    Resids <- estimate.list$sparrowEsts$resid
    Obsyield <- Obs / sitedata$demtarea
    
    predictYield <- predict / sitedata$demtarea
    leverage<-estimate.list$JacobResults$leverage
    boot_resid<-estimate.list$JacobResults$boot_resid
    tiarea<-Csites.weights.list$tiarea
    weight<-Csites.weights.list$weight
    origWaterid<-sitedata$waterid_for_RSPARROW_mapping
    
    dd <- data.frame(sitedata,origWaterid,Obs,predict,Obsyield,predictYield,Resids,standardResids,leverage,boot_resid,weight,tiarea,pResids,ratio.obs.pred,pratio.obs.pred,xlat, xlon)
    keeps <- c("waterid","origWaterid","demtarea","rchname","station_id","station_name","staid",classvar[1],"Obs",
               "predict","Obsyield","predictYield","Resids","standardResids","leverage","boot_resid","weight","tiarea","pResids","ratio.obs.pred","pratio.obs.pred","xlat","xlon")
    residShape <- dd[keeps]
    
    if (length(na.omit(add_vars))!=0){
      add_data<-data.frame(sitedata[,which(names(sitedata) %in% add_vars)])
      if (length(add_vars)==1){
        names(add_data)<-add_vars
      }
      residShape<-cbind(residShape,add_data)
    }
    
    residShape <-SpatialPointsDataFrame(residShape[,c("xlon","xlat")],residShape[,which(!names(residShape) %in% c("xlat","xlon"))],proj4string=CRS(CRStext))
    
    if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
      dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
    }
    if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,sep=""))){
      dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,sep=""),showWarnings = FALSE)
    }
    
    maptools::writeSpatialShape(residShape,paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,"residShape",sep=""))
    cat(showWKT(proj4string(residShape)),file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,"residShape.prj",sep="")) 
    
  }
  
  
}#end function
