#'@title diagnosticMaps
#'@description Creates diagnostic maps of residuals and site attributes and saves them to 
#'            ~/estimate/(run_id)_diagnostic_plots.pdf. \\cr \\cr
#'Executed By: \\itemize\{\\item diagnosticPlotsNLLS.R
#'             \\item diagnosticPlotsValidate.R\} \\cr
#'Executes Routines: unPackList.R \\cr
#'@param mapColumn character string indicating column of data to be mapped
#'@param mapdata input data.frame with lat, long, and column to be mapped
#'@param GeoLines Optional geospatial shape file for overlay of lines on output maps
#'@param strTitle character string for plot title



diagnosticMaps<-function(mapColumn,mapdata,GeoLines,
                         map.list,strTitle,mapping.input.list,sitedata){
  
  
  # Setup variable lists 
  # create global variable from list names (mapping.input.list)
  unPackList(lists = list(mapping.input.list = mapping.input.list),
             parentObj = list(NA)) 
  
  #get data
  mapdata <- mapdata
  #mapColumnName<-mapColumn 
  #set up according to maptype
  if (regexpr("Resid",mapColumn,ignore.case = TRUE)>0 ){
    #set breakpoints
    if (is.na(residual_map_breakpoints) | length(residual_map_breakpoints)!=7){
      cls <- c(-2.5,-0.75,-0.25,0,0.25,0.75,2.5)  # Residual breakpoints
    }else{
      cls<-residual_map_breakpoints
    }
    
    #set threshold and data column
    threshold<-0
    
  }else{# Ratio breakpoints
    if (is.na(ratio_map_breakpoints) | length(ratio_map_breakpoints)!=7){
      cls <-  c(0.3,0.5,0.8,1,1.25,2,3.3)    # Residual breakpoints
    }else{
      cls<-ratio_map_breakpoints
    }
    
    
    #set threshold and data column
    threshold<-1
    
  }#end setup mapType    
  
  
  #point size and color  
  if (enable_plotlyMaps=="no"){
  sze <- residualPointSize_breakpoints*residualPointSize_factor  # Symbol sizes
  }else{
    sze <- residualPointSize_breakpoints*residualPointSize_factor*10  # Symbol sizes
  }
  
  color <- residualColors  
  uniqueColsleaf<-colorNumeric(color, 1:length(color))
  cbckgrd <- residualMapBackground
  
  # Symbol types:
  
 # pnchPlotly<-c("triangle-up-open","triangle-up-open","circle-open","circle-open","circle-open","circle-open",
#                "triangle-down-open","triangle-down-open")
  
  if (enable_plotlyMaps=="no"){
    pnch <- residualPointStyle
    par(mfrow=c(1,1))    # 1 plots on one page
    
    plot(st_geometry(GeoLines),lwd=0.1,xlim=lon_limit,ylim=lat_limit,col = cbckgrd)
   # title(strTitle, cex.main =siteAttrTitleSize)
    
  }else{#plotly
    pnch<-sapply(residualPointStyle, function(x) as.character(pchPlotlyCross[pchPlotlyCross$pch==x,]$plotly))

    
    
    #ititialize text strings for plotly
    markerText<-paste("~paste('</br> Lat: ',Lat,
    '</br> Lon: ',Lon,
    '</br>',",mapColumn,",' :',
    round(",mapColumn,",siteAttrClassRounding)")
    plotLocStr<-paste0("plotloc <- data.frame(Lat,Lon, ",mapColumn," = map1$",mapColumn)
    
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
                     title = "Latitude")) %>%
      add_sf(data = GeoLines,  mode = "lines", type = "scatter",
             stroke = I("black"),color = I(cbckgrd),
             name = LineShapeGeo)
  }
  
  
  if ("threshold-above" %in% map.list){   
   # par(mfrow=c(1,1), pch=16)    # 1 plots on one page
    #subset data
    above <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn,"<",threshold,"),]",sep="")))  # over predictions (neg residuals)
    nabove <- eval(parse(text=paste("length(above$",mapColumn,")",sep="")))

    #for below threshold
   # plot(st_geometry(GeoLines),lwd=0.1,xlim=lon_limit,ylim=lat_limit,col = cbckgrd)
    if (enable_plotlyMaps=="no"){
   title(bquote(paste(.(strTitle)," - Over Predictions - n=",.(nabove))),cex.main=residualTitleSize)
    }else{
    strTitle2<-paste(strTitle," - Over Predictions - n=",nabove)
    p <- p %>% layout(title = strTitle2)  
    } 
    
    
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn,"<= cls[1]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    
    #plotloc <- data.frame(Lat,Lon)
    #points(plotloc$Lon, plotloc$Lat, pch=pnch[1], col=color[1], cex=sze[1])  
    
    if (enable_plotlyMaps=="no"){
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[1], col=color[1], cex=sze[1])  
      
    }else{#plotly
      
      eval(parse(text = plotLocStr))
      #update markerList for marker styling
      markerList<-paste0("list(symbol = pnch[1], size = sze[1],")
      if (regexpr("open",pnch)>0){
        markerList1<-paste0(markerList,"color = uniqueColsleaf(1))")
      }else{
        markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(1)),color = uniqueColsleaf(1))")
      }
      p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter",
                           mode = "markers",#color = I(color[1]),
                           #marker = list(symbol = pnchPlotly[1],size = sze[1]),
                           marker = eval(parse(text = markerList1)),
                           name = paste("< ",cls[1],sep=""),
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
    }
    
    
    strLegend<-paste("< ",cls[1],sep="")
    
    for (k in 1:3) {
      map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn,"<= cls[k+1]), ]",sep="")))
      Lat<- map1$xlat
      Lon<- map1$xlon
     # plotloc <- data.frame(Lat,Lon)
    #  points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1])
      strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      strLegend<-c(strLegend,strlegend)
      
      if (enable_plotlyMaps=="no"){
        plotloc <- data.frame(Lat,Lon)
        points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1]) 
        
      }else{#plotly
        eval(parse(text = plotLocStr))
        #update markerList for marker styling
        markerList<-paste0("list(symbol = pnch[k+1], size = sze[k+1],")
        if (regexpr("open",pnch)>0){
          markerList1<-paste0(markerList,"color = uniqueColsleaf(k+1))")
        }else{
          markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(k+1)),color = uniqueColsleaf(k+1))")
        }
        p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
                             mode = "markers",#color = I(color[k+1]),
                             #marker = list(symbol = pnchPlotly[k+1],size = sze[k+1]),
                             marker = eval(parse(text = markerList1)),
                             name = strlegend,
                             hoverinfo = 'text',
                             text = eval(parse(text = markerText)))
      }
      
      
      
    }
    
    if (enable_plotlyMaps=="no"){
      legend("bottomleft",strLegend,
             bg=cbckgrd, bty="o",pch = residualPointStyle[1:4], pt.cex = residualPointSize_breakpoints[1:4]*residualPointSize_factor, col=color[1:4]
             ,cex=residualLegendSize)
      
    }else{#plotly
      return(p)
    }
  }else if ("threshold-below" %in% map.list){ 
    below <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn,">",threshold,"),]",sep="")))
    nbelow <- eval(parse(text=paste("length(below$",mapColumn,")",sep="")))
    
    if (enable_plotlyMaps=="no"){
      par(mfrow=c(1,1), pch=1)    # 1 plots on one page
      
      plot(st_geometry(GeoLines),lwd=0.1,xlim=lon_limit,ylim=lat_limit,col = cbckgrd)
      # title(strTitle, cex.main =siteAttrTitleSize)
      
    }else{#plotly
      

      
      #plotly plot
      p<-plot_ly() %>%
        layout(
          showlegend =TRUE,
          xaxis = list(range = lon_limit,
                       showticklabels= TRUE,
                       title = "Longitude"),
          yaxis = list(range = lat_limit,
                       showticklabels = TRUE,
                       title = "Latitude")) %>%
        add_sf(data = GeoLines,  mode = "lines", type = "scatter",
               stroke = I("black"),color = I(cbckgrd),
               name = LineShapeGeo)
    }   
    
    #for above threshold
    #plot(st_geometry(GeoLines),lwd=0.1,xlim=lon_limit,ylim=lat_limit,col = cbckgrd)
    if (enable_plotlyMaps=="no"){
      title(bquote(paste(.(strTitle)," - Under Predictions - n=",.(nbelow))),cex.main=residualTitleSize)
    }else{
      strTitle2<-paste(strTitle," - Under Predictions - n=",nbelow)
      p <- p %>% layout(title = strTitle2)  
    }


    
    strLegend<-vector('character')
    
    for (k in 4:7) {
      map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn," <= cls[k+1]), ]",sep="")))
      Lat<- map1$xlat
      Lon<- map1$xlon
      #plotloc <- data.frame(Lat,Lon)
      #points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1]) 
      if (k!=7){
        strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      }else{
        strlegend<-paste("> ",cls[k],sep="")
      }
      strLegend<-c(strLegend,strlegend)
      
      if (enable_plotlyMaps=="no"){
        plotloc <- data.frame(Lat,Lon)
        points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1]) 
        
      }else{#plotly
        eval(parse(text = plotLocStr))
        #update markerList for marker styling
        markerList<-paste0("list(symbol = pnch[k+1], size = sze[k+1],")
        if (regexpr("open",pnch)>0){
          markerList1<-paste0(markerList,"color = uniqueColsleaf(k+1))")
        }else{
          markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(k+1)),color = uniqueColsleaf(k+1))")
        }
        p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
                             mode = "markers",#color = I(color[k+1]),
                             #marker = list(symbol = pnchPlotly[k+1],size = sze[k+1]),
                             marker = eval(parse(text = markerList1)),
                             name = strlegend,
                             hoverinfo = 'text',
                             text = eval(parse(text = markerText)))
      }
      
      
      
    }
    
    
        map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[7]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
   # plotloc <- data.frame(Lat,Lon)
  #  points(plotloc$Lon, plotloc$Lat, pch=pnch[8], col=color[8], cex=sze[8])  
    if (enable_plotlyMaps=="no"){
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[8], col=color[8], cex=sze[8])  
      
    }else{#plotly
      
      eval(parse(text = plotLocStr))
      #update markerList for marker styling
      markerList<-paste0("list(symbol = pnch[8], size = sze[8],")
      if (regexpr("open",pnch)>0){
        markerList1<-paste0(markerList,"color = uniqueColsleaf(8))")
      }else{
        markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(8)),color = uniqueColsleaf(8))")
      }
      p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter",
                           mode = "markers",#color = I(color[8]),
                           #marker = list(symbol = pnchPlotly[8],size = sze[8]),
                           marker = eval(parse(text = markerList1)),
                           name = paste("> ",cls[7],sep=""),
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
    }
    
    if (enable_plotlyMaps=="no"){
      legend("bottomleft",strLegend,
             bg=cbckgrd, bty="o",pch = residualPointStyle[5:8], pt.cex = residualPointSize_breakpoints[5:8]*residualPointSize_factor, 
             col=color[5:8],cex=residualLegendSize)
      
    }else{#plotly
      return(p)
    }

  }#end if threshold map    
  
  if ("all" %in% map.list){
    #for all cls
    #par(mfrow=c(1,1), pch=16)    # 1 plots on one page
    
    #plot(st_geometry(GeoLines),lwd=0.1,xlim=lon_limit,ylim=lat_limit,col = cbckgrd)
    #title(strTitle,cex.main=residualTitleSize)
    
    if (enable_plotlyMaps=="no"){
      par(mfrow=c(1,1))    # 1 plots on one page
      plot(st_geometry(GeoLines),lwd=0.1,xlim=lon_limit,ylim=lat_limit,col = cbckgrd)
      
    }else{#plotly

      #plotly plot
      p<-plot_ly() %>%
        layout(
          showlegend =TRUE,
          xaxis = list(range = lon_limit,
                       showticklabels= TRUE,
                       title = "Longitude"),
          yaxis = list(range = lat_limit,
                       showticklabels = TRUE,
                       title = "Latitude")) %>%
        add_sf(data = GeoLines,  mode = "lines", type = "scatter",
               stroke = I("black"),color = I(cbckgrd),
               name = LineShapeGeo)
    }   
    

    if (enable_plotlyMaps=="no"){
      title(strTitle,cex.main=residualTitleSize)
    }else{
      strTitle2<-strTitle
      p <- p %>% layout(title = strTitle2)  
    }
    
    
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," <= cls[1]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon

    strLegend<-paste("< ",cls[1],sep="")
    
    if (enable_plotlyMaps=="no"){
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[1], col=color[1], cex=sze[1]) 
      
    }else{#plotly
      eval(parse(text = plotLocStr))
      #update markerList for marker styling
      markerList<-paste0("list(symbol = pnch[1], size = sze[1],")
      if (regexpr("open",pnch)>0){
        markerList1<-paste0(markerList,"color = uniqueColsleaf(1))")
      }else{
        markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(1)),color = uniqueColsleaf(1))")
      }
      
      p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
                           mode = "markers",
                           marker = eval(parse(text = markerList1)),
                           name = strLegend,
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
    }
    
    for (k in 1:7) {
      map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn," <= cls[k+1]), ]",sep="")))
      Lat<- map1$xlat
      Lon<- map1$xlon
      #plotloc <- data.frame(Lat,Lon)
      #points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1]) 
      
      if (k!=7){
        strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      }else{
        strlegend<-paste("> ",cls[k],sep="")
      }
      strLegend<-c(strLegend,strlegend)
    
    
    if (enable_plotlyMaps=="no"){
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1]) 
      
    }else{#plotly
      eval(parse(text = plotLocStr))
      #update markerList for marker styling
      markerList<-paste0("list(symbol = pnch[k+1], size = sze[k+1],")
      if (regexpr("open",pnch)>0){
        markerList1<-paste0(markerList,"color = uniqueColsleaf(k+1))")
      }else{
        markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(k+1)),color = uniqueColsleaf(k+1))")
      }
      p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
                           mode = "markers",
                           marker = eval(parse(text = markerList1)),
                           name = strlegend,
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
    }
    }
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[7]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    #plotloc <- data.frame(Lat,Lon)
    #points(plotloc$Lon, plotloc$Lat, pch=pnch[8], col=color[8], cex=sze[8])  
    
    if (enable_plotlyMaps=="no"){
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[8], col=color[8], cex=sze[8]) 
      legend("bottomleft",strLegend,
           bg=cbckgrd, bty="o",pch = residualPointStyle, 
           pt.cex = sze, col=color,cex=residualLegendSize)
    }else{#plotly
     # eval(parse(text = plotLocStr))
    #  #update markerList for marker styling
    #  markerList<-paste0("list(symbol = pnch[8], size = sze[8],")
    #  if (regexpr("open",pnch)>0){
    #    markerList1<-paste0(markerList,"color = uniqueColsleaf8))")
    #  }else{
    #    markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(8)),color = uniqueColsleaf(8))")
    #  }
    #  p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
    #                       mode = "markers",
    #                       marker = eval(parse(text = markerList1)),
    #                       name = strLegend[length(strLegend)],
    #                       hoverinfo = 'text',
    #                       text = eval(parse(text = markerText)))
      return(p)
    }
    
    
    
  }#end if all  
  
  
}#end function
