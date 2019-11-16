plotlyLayout<-function(x, y, log, nTicks, digits, 
                       xTitle, xZeroLine,xLabs = NA, yTitle, yZeroLine,
                       plotTitle,legend){
  
  #format tickmarks
  for (a in c("x","y")){
    aData<-eval(parse(text = a))
    if (!is.na(aData)){
    #test if scientific notation is needed
    if (min(aData, na.rm = TRUE)>1000 | max(aData,na.rm = TRUE)>100000){
      sciNote<-TRUE
    }else{
      sciNote<-FALSE
    }
    
    if (regexpr(a,log)>0){
      ticks<-axisTicks(log10(range(aData, na.rm = TRUE)), log = TRUE, n = nTicks)
      if (sciNote){
       ticksLab<-formatC(ticks, format = "e", digits = digits) 
      }else{
        ticksLab<-as.character(ticks)
      }
      
      type<-"type = 'log',"
    }else{
      ticks<-axisTicks(range(aData, na.rm = TRUE), log = FALSE, n = nTicks) 
      ticksLab<-as.character(ticks)
      type<-""
    }
    
    strTitle<-eval(parse(text = paste0(a,"Title")))
    strZeroLine<-eval(parse(text = paste0(a,"ZeroLine")))
    
     
    #create axis attribute list
    eval(parse(text = paste0(a,"Axis.list <- list(",type,
                                                  "tickvals = ticks, 
                                                  ticktext = ticksLab,
                                                  showline = TRUE,
                                                  ticks = 'outside',
                                                  title = strTitle,
                                                  zeroline = strZeroLine,
                             yref = 'paper',y=0, titlefont = list(size = 11))"))) 
    }else{
      if (is.na(xLabs)){
       eval(parse(text = paste0(a,"Axis.list <- list(showticklabels = FALSE,
                               showline = TRUE)"))) 
      }else{
        strTitle<-eval(parse(text = paste0(a,"Title")))
        strZeroLine<-eval(parse(text = paste0(a,"ZeroLine")))
        eval(parse(text = paste0(a,"Axis.list <- list(type = 'category',
                                                  categoryoder = 'category ascending',
                                                  categoryarray = xLabs,
                                                  showline = TRUE,
                                                  ticks = 'outside',
                                                  title = strTitle,
                                                  zeroline = strZeroLine,
                             yref = 'paper',y=0, titlefont = list(size = 11))"))) 
      }
       
    }
  }
  
  a <- list(
    text = paste0("<b>",plotTitle,"</b>"),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 0.95,
    showarrow = FALSE,
    font = list(size = 14)
  )

  
 
  #initiate plotly plot
   p<-plot_ly()
   
   
  if (exists("xAxis.list")){
  p <- p %>% layout(xaxis = xAxis.list,
                    yaxis = yAxis.list,
                    showlegend = legend,
                    annotations = a,
                    margin = list(t=100))
  }else{
    p <- p %>% layout(
                      yaxis = yAxis.list,
                      showlegend = legend,
                      annotations = a,
                      margin = list(t=100))
}
  
  return(p)
}