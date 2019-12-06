addMarkerText<-function(markerText,add_plotlyVars,mapData, sourceData){
  
  if (!is.na(add_plotlyVars[1])){
    add_plotlyVars<-as.character(ifelse(add_plotlyVars=="waterid","waterid_for_RSPARROW_mapping",add_plotlyVars))
    #add attributes to markerText
    for (m in add_plotlyVars){ 
      if (suppressWarnings(!is.na(sourceData))){
       if (m %in% names(sourceData)) {
        markerText<-paste0(markerText,",'</br> ",m," : ',",m)
        markerAttrs<-eval(parse(text= paste("data.frame(",m,"=sourceData$",m,")",sep=""))) 
        #mapData<-cbind(mapData,markerAttrs)
        eval(parse(text = paste0("mapData$",m,"<-markerAttrs")))
       }
      }else{
        if (m %in% names(mapData)){
          markerText<-paste0(markerText,",'</br> ",m," : ',",m) 
        }
       
      }
      }
    }
  
  #wrap up text strings
  markerText<-paste0(markerText,")")

  marker.list<-named.list(markerText, mapData)
  return(marker.list)
}


