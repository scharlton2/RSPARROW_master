mapLoopStr<-function(file.output.list,predictMapType,GeoLines,
                     plotShape,dmapfinal,plots,k,
                     existGeoLines,Rshiny,input,
                     predictionTitleSize,scenario_name,scenario_map_list,
                     master_map_list,predictionLegendSize,mapunits.list,predictionLegendBackground,
                     break1,Mcolors,
                     enable_plotlyMaps,output_map_type,
                     lineWidth,lon_limit,lat_limit,nlty,nlwd,
                     mapdataname,predictionMapColors,add_plotlyVars,
                     mapScenarios,predictionMapBackground,LineShapeGeo,
                     mapvarname,predictionClassRounding,
                     commonvar,
                     map_years,map_seasons,mapsPerPage,mapPageGroupBy,aggFuncs){

map_loop.list<-list(0)

  for (i in unique(plots$plotKey)){
    usedColors<-character(0)
    if (nrow(plots)>1){
      plotSub<-plots[plots$plotKey==i,]
    }else{
      plotSub<-plots
    }
  
  for (j in 1:nrow(plotSub)){
    if (is.na(mapPageGroupBy) & is.na(map_years) & is.na(map_seasons)){#all
      dmapfinal$year<-rep(1,nrow(dmapfinal))
      dmapfinal$season<-rep(1,nrow(dmapfinal))
      y<-unique(dmapfinal$year)
      s<-unique(dmapfinal$season)
    }else if (mapPageGroupBy %in% (c("year",NA)) & is.na(map_seasons) & !is.na(map_years)){
      y<-plotSub[j,]$year
      dmapfinal$season<-rep(1,nrow(dmapfinal))
      s<-unique(dmapfinal$season)
    }else if (mapPageGroupBy %in% (c("season",NA)) & is.na(map_years) & !is.na(map_seasons)){
      dmapfinal$year<-rep(1,nrow(dmapfinal))
      y<-unique(dmapfinal$year)
      s<-plotSub[j,]$season
    }else{
      y<-plotSub[j,]$year
      s<-plotSub[j,]$season 
    }
    
    
    if (!is.na(y[1]) & !is.na(s[1])){
      save(dmapfinal,file="D:/dmapfinal141")
      plotdata<-dmapfinal[dmapfinal$year %in% c(y) & dmapfinal$season %in% c(s),]
      save(y,file="D:/y")
      save(s,file="D:/s")
      save(plotdata,file="D:/plotdata137")
      if ((is.na(map_years) & is.na(map_seasons)) | (!is.na(map_years) & map_years %in% aggFuncs) | (!is.na(map_seasons) & map_seasons %in% aggFuncs)){
        plotdata <- merge(plotShape, plotdata, by.x = commonvar, by.y = commonvar)
      }else{
        plotdata <- merge(plotShape, plotdata, by.x = commonvar, by.y = "mapping_waterid")
        plotdata$mapping_waterid<-eval(parse(text=paste0("plotdata$",commonvar)))
        save(plotdata,file="D:/plotdata149")
      }
      
      
      if (!mapScenarios){
        titleStr<-paste0(master_map_list[k],"\n",mapunits.list[k])
      }else{
        if (!Rshiny){
          titleStr<-paste(scenario_name,scenario_map_list[k],"\n",mapunits.list[k],sep=" ")
        }else{
          titleStr<-paste(input$scenarioName,master_map_list[k],"\n",mapunits.list[k],sep=" ")
        }
      }
      
      if(is.na(map_years) & is.na(map_seasons)){
        subTitle<-""                
      }else if (!is.na(map_years) & map_years %in% aggFuncs & !is.na(map_seasons) & map_seasons %in% aggFuncs){
        if (is.na(map_years)){
          titleStr<-paste(map_seasons,titleStr)
        }else{
          titleStr<-paste(map_years,titleStr)
        }
        subTitle<-titleStr
      }else if (is.na(map_seasons) | map_seasons %in% aggFuncs){
        if (map_seasons %in% aggFuncs){
          titleStr<-paste(map_seasons,titleStr)
        }
        
        if ((is.na(map_seasons) & map_years %in% aggFuncs) | (is.na(map_years) & map_seasons %in% aggFuncs)){
          subTitle<-"" 
        }else{
          subTitle<-y
        }
        
      }else if (is.na(map_years) | map_years %in% aggFuncs){
        if (map_years %in% aggFuncs){
          titleStr<-paste(map_years,titleStr)
        }
        if (is.na(map_years) & map_seasons %in% aggFuncs | (is.na(map_seasons) & map_years %in% aggFuncs)){
          subTitle<-"" 
        }else{
          subTitle<-s
        }
        
      }else{
        subTitle<-paste(y,s)
      }
      
      a <- list(
        text = paste0("<b>",subTitle,"</b>"),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1.00,
        showarrow = FALSE,
        font = list(size = 14)
      )
      
      if (is.na(mapPageGroupBy) & !is.na(map_years) & map_years %in% aggFuncs & !is.na(map_seasons) & map_seasons %in% aggFuncs){
        plotPageData<-dmapfinal
      }else if (is.na(mapPageGroupBy) & !is.na(map_seasons) & map_seasons %in% aggFuncs){
        plotPageData<-dmapfinal[dmapfinal$year %in% plotSub$year,]
      }else if (is.na(mapPageGroupBy) & !is.na(map_years) & map_years %in% aggFuncs){
        plotPageData<-dmapfinal[dmapfinal$season %in% plotSub$season,]
      }else{
        plotPageData<-dmapfinal[dmapfinal$year %in% plotSub$year & dmapfinal$season %in% plotSub$season,]
      }
      pageColors<-unique(plotPageData$color)
      
      
      
      
      p<-plot_ly() %>%
        layout(annotations=a,
               margin =list(t=100),
               xaxis = list(range = lon_limit,
                            showticklabels= TRUE,
                            title = "Longitude"),
               yaxis = list(range = lat_limit,
                            showticklabels = TRUE,
                            title = "Latitude"))
      if (existGeoLines){
        
        
        p <- p %>% add_sf(data = GeoLines,  mode = "lines", type = "scatter",
                          stroke = I("black"),color = I(predictionMapBackground),
                          name = LineShapeGeo, hoverinfo = "none", showlegend=FALSE)
        
        
        
        
      }
      mapvarname <- paste0("MAPCOLORS",k)
      suppressWarnings(remove(list = c(add_plotlyVars)))
      uniqueCols<-eval(parse(text = paste0("as.character(unique(plotPageData$",mapvarname,"))")))
      uniqueCols<-Mcolors[Mcolors %in% uniqueCols]
      
      #for (c in unique(plotPageData$color)){
      for (c in uniqueCols){
        plotdata2<-plotdata
        plotdata2$mapColor<-eval(parse(text = paste0("plotdata2$",mapvarname)))
        plotdata2<-plotdata2[plotdata2$mapColor==c,]
        plotdata2$mapdataname<-eval(parse(text = paste0("plotdata2$",mapdataname)))
        
        lineText<-"~paste('</br> ',master_map_list[k],' :',
                   round(mapdataname,predictionClassRounding)"
        
        lineText<-addMarkerText(lineText,add_plotlyVars,plotdata2, plotdata2)$markerText
        
        if (predictMapType=="stream"){
          if (!c %in% usedColors & c %in% plotdata2$mapColor){
            usedColors<-c(usedColors,c)
            p <- p %>% add_sf(data = plotdata2, mode = "lines", type = "scatter",
                              # color = I(c),
                              color = ~I(mapColor),
                              name = break1[k][[1]][uniqueCols==c],
                              line = list(width = lineWidth),
                              hoverinfo = 'text',
                              text = eval(parse(text = lineText)),
                              legendgroup=c, showlegend=TRUE)
          }else{
            p <- p %>% add_sf(data = plotdata2, mode = "lines", type = "scatter",
                              #color = I(c),
                              color=~I(mapColor),
                              name = break1[k][[1]][uniqueCols==c],
                              line = list(width = lineWidth),
                              hoverinfo = 'text',
                              text = eval(parse(text = lineText)),
                              legendgroup=c,showlegend=FALSE)
          }
        }else{#catchment
          if (!c %in% usedColors & c %in% plotdata2$mapColor){
            usedColors<-c(usedColors,c)
            p <- p %>% add_sf(data = plotdata2[1,],
                              type = "scatter", mode = "lines",
                              # color = toRGB(c),
                              opacity = 1,fillcolor = toRGB(c),
                              line = list(color = toRGB(c),width = 0.8, opacity = 1),
                              name = break1[k][[1]][uniqueCols==c],
                              hoverinfo = 'text',
                              split = eval(parse(text = paste0("~",commonvar))),
                              hoveron = "fills",
                              legendgroup = c,
                              text = eval(parse(text = lineText)),
                              showlegend = TRUE)
            p <- p %>% add_sf(data = plotdata2[2:nrow(plotdata2),],
                              type = "scatter", mode = "lines",
                              # color = toRGB(c),
                              opacity = 1,fillcolor = toRGB(c),
                              line = list(color = toRGB(c),width = 0.8, opacity = 1),
                              hoverinfo = 'text',
                              split = eval(parse(text = paste0("~",commonvar))),
                              hoveron = "fills",
                              legendgroup = c,
                              text = eval(parse(text = lineText)),
                              showlegend = FALSE)
          }else{
            
            p <- p %>% add_sf(data = plotdata2, mode = "lines",
                              #p <- p %>% add_sf(data = plotdata2[2:nrow(plotdata2),],
                              type = "scatter", mode = "lines",
                              # color = toRGB(c),
                              opacity = 1,fillcolor = toRGB(c),
                              line = list(color = toRGB(c),width = 0.8, opacity = 1),
                              hoverinfo = 'text',
                              split = eval(parse(text = paste0("~",commonvar))),
                              hoveron = "fills",
                              legendgroup = c,
                              text = eval(parse(text = lineText)),
                              showlegend = FALSE)
          }
        }
        
        
        
        
        
      }
      
      
      
      eval(parse(text = paste0("p",j,"<-p")))
    }#if plot not missing
  }#j
  
  if (nrow(plotSub[!is.na(plotSub$year),])>1){
    
    pStr<-paste("p",seq(1,nrow(plotSub[!is.na(plotSub$year),]),1),collapse=",",sep="") 
    
    
    if (nrow(plotSub[!is.na(plotSub$year),])>2){
      nrws<-2
    }else{
      nrws<-1
    }
    
    
    eval(parse(text = paste0("p",letters[which(unique(plots$plotKey)==i)],"<-plotly::subplot(",pStr,",nrows = ",nrws,",margin = 0.05) %>% layout(title=list(text='<b>",titleStr,"</b>',xanchor='right',x=0.9))")))
    
    
  }else{
    
    #pStr<-eval(parse(text = paste0("p",letters[which(unique(plots$plotKey)==i)],"<-p1"))) 
    pStr<-"p1"
    eval(parse(text = paste0("p",letters[which(unique(plots$plotKey)==i)],"<-plotly::subplot(",pStr,",nrows = 1,margin = 0.05) %>% layout(title=list(text='<b>",titleStr,"</b>',xanchor='right',x=0.9))")))
    
  }
    
   eval(parse(text=paste0("map_loop.list$p",letters[which(unique(plots$plotKey)==i)],
                          "<-p",letters[which(unique(plots$plotKey)==i)])))
  }#i  

map_loop.list<-map_loop.list[names(map_loop.list)!=""]

return(map_loop.list)
}