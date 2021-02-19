aggDynamicMapdata<-function(map_years,map_seasons,enable_plotlyMaps,add_plotlyVars,
                            aggFuncs,vvar,MAPID,commonvar,subdata){
  #aggregate seasons or years if required
  if (!is.na(map_years) & map_years %in% aggFuncs & (!map_seasons %in% aggFuncs & !is.na(map_seasons))){
    mapdata<-data.frame(subdata[c("mapping_waterid","season")],vvar)
    save(mapdata,file="D:/mapdata453")
    if (map_seasons!="all"){
      mapdata<-mapdata[mapdata$season %in% map_seasons,]
    }
    mapdata<-aggregate(mapdata["vvar"], by=list(mapping_waterid= mapdata$mapping_waterid,season=mapdata$season),
                       FUN=eval(parse(text=map_years)))
    save(mapdata,file="D:/mapdata458")
    MAPID<-mapdata$mapping_waterid
    vvar<-mapdata$vvar
    names(mapdata)[1]<-commonvar
    dmapfinal <- data.frame(mapdata[,names(mapdata)!="vvar"])                                   # added 3-25-2017
    names(dmapfinal)[1] <- c(commonvar)
    
    if (enable_plotlyMaps!="no" & enable_plotlyMaps!="static" & !is.na(add_plotlyVars[1])){
      add_plotlyVars<-test_addPlotlyvars(add_plotlyVars = c(add_plotlyVars,"lat","lon"),
                                         subdata,
                                         groupVar=c("season","mapping_waterid"),
                                         maxUnique=nrow(mapdata))
      uniqueSubdata<-subdata[,names(subdata) %in% c(add_plotlyVars,"season","mapping_waterid")]
      names(uniqueSubdata)[names(uniqueSubdata)=="mapping_waterid"]<-commonvar
      uniqueSubdata<-unique(uniqueSubdata)
      uniqueSubdata$year<-rep(1,nrow(uniqueSubdata))
      if (map_seasons!="all"){
        uniqueSubdata<-uniqueSubdata[uniqueSubdata$season %in% map_seasons,]
      }
    }
    
  }else if (!is.na(map_seasons) & map_seasons %in% aggFuncs & (!map_years %in% aggFuncs & !is.na(map_years))){
    mapdata<-data.frame(subdata[c("mapping_waterid","year")],vvar)
    if (map_years!="all"){
      mapdata<-mapdata[mapdata$year %in% map_years,]
    }
    mapdata<-aggregate(mapdata["vvar"], by=list(mapping_waterid=mapdata$mapping_waterid,year=mapdata$year),
                       FUN=eval(parse(text=map_seasons)))
    MAPID<-mapdata$mapping_waterid
    names(mapdata)[1]<-commonvar
    vvar<-mapdata$vvar
    dmapfinal <- data.frame(mapdata[,names(mapdata)!="vvar"])                                   # added 3-25-2017
    names(dmapfinal)[1] <- c(commonvar)
    
    if (enable_plotlyMaps!="no" & enable_plotlyMaps!="static" & !is.na(add_plotlyVars[1])){
      add_plotlyVars<-test_addPlotlyvars(add_plotlyVars = c(add_plotlyVars,"lat","lon"),
                                         subdata,
                                         groupVar=c("year","mapping_waterid"),
                                         maxUnique=nrow(mapdata))
      uniqueSubdata<-subdata[,names(subdata) %in% c(add_plotlyVars,"year","mapping_waterid")]
      names(uniqueSubdata)[names(uniqueSubdata)=="mapping_waterid"]<-commonvar
      uniqueSubdata<-unique(uniqueSubdata)
      uniqueSubdata$season<-rep(1,nrow(uniqueSubdata))
      if (map_years!="all"){
        uniqueSubdata<-uniqueSubdata[uniqueSubdata$year %in% map_years,]
      }
    }
    
  }else if (!is.na(map_years) & map_years %in% aggFuncs & !is.na(map_seasons) & map_seasons %in% aggFuncs){
    mapdata<-data.frame(subdata[c("mapping_waterid","year")],vvar)
    mapdata<-aggregate(mapdata["vvar"], by=list(mapping_waterid=mapdata$mapping_waterid,year=mapdata$year),
                       FUN=eval(parse(text=map_years)))
    mapdata<-aggregate(mapdata["vvar"], by=list(mapping_waterid=mapdata$mapping_waterid),
                       FUN=eval(parse(text=map_years)))
    MAPID<-mapdata$mapping_waterid
    names(mapdata)[1]<-commonvar
    vvar<-mapdata$vvar
    dmapfinal <- data.frame(mapdata[,names(mapdata)!="vvar"])                                   # added 3-25-2017
    names(dmapfinal)[1] <- c(commonvar)
    
    if (enable_plotlyMaps!="no" & enable_plotlyMaps!="static" & !is.na(add_plotlyVars[1])){
      add_plotlyVars<-test_addPlotlyvars(add_plotlyVars = c(add_plotlyVars,"lat","lon"),
                                         subdata,
                                         groupVar=c("mapping_waterid"),
                                         maxUnique=nrow(mapdata))
      uniqueSubdata<-subdata[,names(subdata) %in% c(add_plotlyVars,"mapping_waterid")]
      names(uniqueSubdata)[names(uniqueSubdata)=="mapping_waterid"]<-commonvar
      uniqueSubdata<-unique(uniqueSubdata)
      uniqueSubdata$season<-rep(1,nrow(uniqueSubdata))
      uniqueSubdata$year<-rep(1,nrow(uniqueSubdata))
    }
    
  }else if ((!is.na(map_years) & map_years %in% aggFuncs) | (!is.na(map_seasons) & map_seasons %in% aggFuncs)){
    mapdata<-data.frame(subdata[c("mapping_waterid")],vvar)
    if (map_years %in% aggFuncs){
      aggFunc<-map_years
    }else{
      aggFunc<-map_seasons
    }
    mapdata<-aggregate(mapdata["vvar"], by=list(mapping_waterid=mapdata$mapping_waterid),
                       FUN=eval(parse(text=aggFunc)))
    MAPID<-mapdata$mapping_waterid
    names(mapdata)[1]<-commonvar
    vvar<-mapdata$vvar
    dmapfinal <- data.frame(mapdata[,names(mapdata)!="vvar"])                                   # added 3-25-2017
    names(dmapfinal)[1] <- c(commonvar)
    
    if (enable_plotlyMaps!="no" & enable_plotlyMaps!="static" & !is.na(add_plotlyVars[1])){
      add_plotlyVars<-test_addPlotlyvars(add_plotlyVars = c(add_plotlyVars,"lat","lon"),
                                         subdata,
                                         groupVar=c("mapping_waterid"),
                                         maxUnique=nrow(mapdata))
      uniqueSubdata<-subdata[,names(subdata) %in% c(add_plotlyVars,"mapping_waterid")]
      names(uniqueSubdata)[names(uniqueSubdata)=="mapping_waterid"]<-commonvar
      uniqueSubdata<-unique(uniqueSubdata)
      uniqueSubdata$season<-rep(1,nrow(uniqueSubdata))
      uniqueSubdata$year<-rep(1,nrow(uniqueSubdata))
    }
    
  }else{
    uniqueSubdata<-subdata
    names(uniqueSubdata)[names(uniqueSubdata)=="waterid_for_RSPARROW_mapping"]<-commonvar
    mapdata<-data.frame(MAPID)
    colnames(mapdata)<-commonvar
    dmapfinal <- data.frame(MAPID)                                   
    colnames(dmapfinal) <- c(commonvar)
  }
  
  map_agg.list<-named.list(uniqueSubdata,mapdata,dmapfinal,add_plotlyVars,MAPID,vvar)
  return(map_agg.list)
}