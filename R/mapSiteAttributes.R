#'@title mapSiteAttributes
#'@description function to execute site attribute mapping \\cr \\cr
#'Executed By: \\itemize\{\\item interactiveBatchRun.R
#'             \\item diagnosticPlotsNLLS.R
#'             \\item goShinyPlot.R\} \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item mapBreaks.R
#'             \\item named.list.R
#'             \\item replaceNAs.R
#'             \\item unPackList.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param attr character string shiny user input of attribute to map in `mapSiteAttributes.R`
#'@param path_gis path to users gis data
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`
#'@param LineShapeGeo character string control settting indicating which binary map file to 
#'       load for the Geolines background layer
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param Rshiny TRUE/FALSE indicating whether routine is being run from the Shiny app
#'@param mapColumn character string indicating column of data to be mapped
#'@param mapdata input data.frame with lat, long, and column to be mapped
#'@param GeoLines Optional geospatial shape file for overlay of lines on output maps
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param strTitle character string for plot title
#'@param unitAttr character string indicating the unit of the attribute being mapped by 
#'       `mapSiteAttributes.R`
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



mapSiteAttributes<-function(#Rshiny
  input,attr, path_gis, sitedata, LineShapeGeo,data_names,Rshiny,
  #regular
  mapColumn,mapdata,GeoLines,mapping.input.list,
  strTitle,unitAttr,batch_mode){
  
   
  
  if (((input$var!="" |!is.na(attr)) & Rshiny==TRUE)|Rshiny==FALSE){
    
    if (Rshiny==TRUE){
      
      
      #get geoLines
      existGeoLines<-checkBinaryMaps(LineShapeGeo,path_gis,batch_mode)
      if (existGeoLines==TRUE){
        load(paste(path_gis,.Platform$file.sep,"GeoLines",sep=""))
      }
    }
    
    # create global variable from list names (mapping.input.list)
    unPackList(lists = list(mapping.input.list = mapping.input.list),
               parentObj = list(NA))
    
    if (Rshiny==TRUE){
      enable_plotlyMaps<-as.character(input$enablePlotly)
      add_plotlyVars<-as.character(input$plotlyDrop)
      siteAttr_mapPointSize<-as.numeric(input$siteAttr_mapPointSize)
      siteAttrTitleSize<-as.numeric(input$siteAttrTitleSize)
      siteAttrLegendSize<-as.numeric(input$siteAttrLegendSize)
      siteAttrColors<-eval(parse(text=as.character(input$siteAttrColors)))
      siteAttrClassRounding<-as.numeric(input$siteAttrClassRounding)
      siteAttr_mapPointStyle<-as.numeric(input$siteAttr_mapPointStyle)
      siteAttrMapBackground<-gsub("\"","",gsub("'","",as.character(input$siteAttrMapBackground)))
      
      if (input$batch!="Batch"){
        
        
        mapColumn<-as.character(input$var)
      }else{
        
        mapColumn<-as.character(attr)
        
      }
      mapColumnName<-mapColumn 
      siteAttr<-eval(parse(text= paste("data.frame(",mapColumn,"=sitedata$",mapColumn,")",sep="")))
      titleAttr<-data_names[which(data_names$sparrowNames==mapColumn),]
      unitAttr<-titleAttr$varunits
      titleAttr<-as.character(titleAttr$explanation)
      strTitle<-titleAttr
      
      xlat<-sitedata$lat
      xlon<-sitedata$lon
      
      mapdata<-data.frame(xlat,xlon,siteAttr)
      
    }else{ #regular
      #get data
      mapdata <- mapdata
      mapColumnName<-mapColumn 
    }
    
    mapdata$mapColumn<-eval(parse(text=paste("mapdata$",mapColumn,sep="")))
    
    #replace NAs
    mapColumn<-mapdata$mapColumn
    replaceNAs(listColumns = named.list("mapColumn"))
    mapdata$mapColumn<-mapColumn
    
    #set breakpoints
    cls<-unique(mapBreaks(mapdata$mapColumn,siteAttrColors)$brks)
    cls<-round(cls[2:length(cls)],siteAttrClassRounding)
    
    #size and color
    sze<-rep(0.45,length(cls))*siteAttr_mapPointSize
    color<-siteAttrColors[1:length(cls)]
    cbckgrd <- siteAttrMapBackground
    uniqueColsleaf<-colorNumeric(color, 1:length(color))
    

    
    if (enable_plotlyMaps=="no"){
      pnch <- siteAttr_mapPointStyle
      par(mfrow=c(1,1))    # 1 plots on one page
    
    plot(st_geometry(GeoLines),lwd=0.1,xlim=lon_limit,ylim=lat_limit,col = cbckgrd)
    title(strTitle, cex.main =siteAttrTitleSize)
    
    }else{#plotly
      #set marker styling
    pnch<-as.character(pchPlotlyCross[pchPlotlyCross$pch==siteAttr_mapPointStyle,]$plotly)
    markerList<-paste0("list(symbol = pnch, size = sze[1]*10,")

    
    #ititialize text strings for plotly
    markerText<-"~paste('</br> Lat: ',Lat,
                   '</br> Lon: ',Lon,
                   '</br>',mapColumnName,' :',
                   round(mapColumn,siteAttrClassRounding)"
    plotLocStr<-paste0("plotloc <- data.frame(Lat,Lon, mapColumn = map1$mapColumn")
    
    markerText<-addMarkerText(markerText,add_plotlyVars,mapdata, sitedata)$markerText
    mapdata<-addMarkerText(markerText,add_plotlyVars, mapdata,sitedata)$mapData
    
    if (!is.na(add_plotlyVars[1])){
      add_plotlyVars<-as.character(ifelse(add_plotlyVars=="waterid","waterid_for_RSPARROW_mapping",add_plotlyVars))
      
      #add attributes to markerText
      for (m in add_plotlyVars){
        plotLocStr<-paste0(plotLocStr,",",m," = map1$",m)
      }
    }
    
    #wrap up text strings
    plotLocStr<-paste0(plotLocStr,")")
    
 
    #plotly plot
    p<-plot_ly() %>%
      layout(
        showlegend =TRUE,
        xaxis = list(range = lon_limit,
                     showticklabels= TRUE,
                     title = "Longitude"),
        yaxis = list(range = lat_limit,
                     showticklabels = TRUE,
                     title = "Latitude"),
        title = mapColumnName) %>%
      add_sf(data = GeoLines,  mode = "lines", type = "scatter",
             stroke = I("black"),color = I(cbckgrd),
             name = LineShapeGeo)
    }
    
    #first class
    map1 <- mapdata[(mapdata$mapColumn <= cls[1]), ]
    Lat<- map1$xlat
    Lon<- map1$xlon
    strLegend<-paste(round(min(mapdata$mapColumn),siteAttrClassRounding)," to ",cls[1],sep="")
    
    if (enable_plotlyMaps=="no"){
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[1], col=color[1], cex=sze[1])  
      
      }else{#plotly
    
    eval(parse(text = plotLocStr))
        #update markerList for marker styling
        if (regexpr("open",pnch)>0){
          markerList1<-paste0(markerList,"color = uniqueColsleaf(1))")
        }else{
          markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(1)),color = uniqueColsleaf(1))")
        }
      
    p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter",
                         mode = "markers",
                        marker = eval(parse(text = markerList1)),
                         name = paste(round(min(mapdata$mapColumn),siteAttrClassRounding)," to ",cls[1],sep=""),
                         hoverinfo = 'text',
                         text = eval(parse(text = markerText)))
    }
    #middle classes
    for (k in 1:(length(cls)-1)) {
      map1 <- mapdata[(mapdata$mapColumn > cls[k] & mapdata$mapColumn <= cls[k+1]), ]
      Lat<- map1$xlat
      Lon<- map1$xlon
      
      if (k!=(length(cls)-1)){
        strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      }else{
        strlegend<-paste(cls[k]," to ",round(max(mapdata$mapColumn),siteAttrClassRounding),sep="")
      }
      strLegend<-c(strLegend,strlegend)
      
      if (enable_plotlyMaps=="no"){
        plotloc <- data.frame(Lat,Lon)
       points(plotloc$Lon, plotloc$Lat, pch=pnch, col=color[k+1], cex=sze[k+1]) 
        
      }else{#plotly
      eval(parse(text = plotLocStr))
        #update markerList for marker styling
        if (regexpr("open",pnch)>0){
          markerList2<-paste0(markerList,"color = uniqueColsleaf(k+1))")
        }else{
          markerList2<-paste0(markerList,"line = list(color = uniqueColsleaf(k+1)),color = uniqueColsleaf(k+1))")
        }
       
      p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
                           mode = "markers",#color = I(color[k+1]),
                           marker = eval(parse(text = markerList2)),
                           name = strlegend,
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
      }
      
    } #for each middle class k 
    
    #last class
    map1 <- mapdata[(mapdata$mapColumn > cls[(length(cls)-1)]), ]
    Lat<- map1$xlat
    Lon<- map1$xlon
    #strlegend<-paste(">",cls[length(cls)],sep=" ")
    #strLegend<-c(strLegend,strlegend)
    
    if (enable_plotlyMaps=="no"){
      plotloc <- data.frame(Lat,Lon)
     points(plotloc$Lon, plotloc$Lat, pch=pnch, col=color[length(cls)], cex=sze[length(cls)])  
     legend("bottomleft",strLegend,
             bg=siteAttrMapBackground, bty="o",pch = siteAttr_mapPointStyle, 
             pt.cex = sze, col=color,title = unitAttr,cex=siteAttrLegendSize)
     
    }else{#plotly
    #eval(parse(text = plotLocStr))
    #p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
    #                     mode = "markers",color = I(color[length(cls)]),
    #                     name = paste0("> ",round(cls[(length(cls)-1)],siteAttrClassRounding)),
    #                     hoverinfo = 'text',
    #                     text = eval(parse(text = markerText)))
     return(p)
    }
    
  

  }#if attr selected in shiny
  
  
}#end function
