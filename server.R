shinyServer(function(input, output, session) {
  
  output$mymap <- renderLeaflet({ 
    leafletMap
  })
  
 
  
  observeEvent(input$date1, {
    req(input$date1) 
    data<-input$date1
    if(data < data.prima) data<-data.prima
    if(data > data.ultima) data<-data.ultima
    updateSliderInput(session, "giorno", NULL,
                      value = data)
  })
  
  output$dygraph <- renderDygraph({ 
    dygraph(dati.naz1.ts, main=HTML("Hospitalized and in Intensive Care") ) %>% 
      dyAxis("y", label = "Intensive Care",drawGrid = FALSE) %>%
      dyAxis("y2", label = "Hospitalized with symptoms", independentTicks = TRUE,drawGrid = FALSE) %>%
      dySeries( "ricoverati_con_sintomi", label = HTML("Hospitalized with symptoms"),  axis = 'y2', strokeWidth = 2) %>%
      dySeries( "terapia_intensiva", label = "Intensive Care", strokeWidth = 2) %>%
      dyLegend(  labelsSeparateLines=T) %>% dyRangeSelector()
  }) 
  output$dygraph2 <- renderDygraph({ 
    dygraph(dati.naz2.ts, main=HTML("N. of swabs (x1000) and n. of positives") ) %>% 
      dyAxis("y", label = "N. of swabs/tests (x1000)",drawGrid = FALSE) %>%
      dyAxis("y2", label = "Total positive COVID-19", independentTicks = TRUE,drawGrid = FALSE) %>%
      dySeries( "totale_positivi", label = HTML("Total positive COVID-19"),  axis = 'y2',strokeWidth = 2) %>%
      dySeries( "tamponi", label = "N. of swabs/tests (x1000)", strokeWidth = 2) %>%
      dyLegend(  labelsSeparateLines=T) %>% dyRangeSelector()
  }) 
  # observeEvent(input$opac, {
  #   req(input$opac)  
  #   print(input$opac)
  #   creaMapLegend(dt.filtered, 100-as.integer(input$opac )) 
  #   
  #   leafletProxy("mymap") %>%  addWMSTiles(baseUrl = sprintf("https://geolab02.vs-ix.net/cgi-bin/mapserv?uselessCache=%s_%s&map=%s/mapfile.map",
  #                                 input$giorno,input$opac,  pwd   ) ,
  #               options = WMSTileOptions(format = "image/png", transparent = T  ),
  #               layers="Casi_COVID19", layerId =  sprintf("Casi_COVID19"),
  #               attribution = "Provincie ISTAT @CIRGEO" )
  # })
  # 
  observe(  {
    req(input$giorno, dt.sorted2, dt.filtered) 
    updateDateInput(session, "date1", value=input$giorno)
     
    
    if(input$calculus != "totale_casi" && input$scale.funct_!= "linear" ){
      updateSelectInput(session, "scale.funct_", selected  = "linear" )
      shinyjs::runjs("alert('Sorry, cannot apply that scale to negative numbers! Falling back to linear')")
      fn<-get("linear") 
    } else { 
      fn<-get(input$scale.funct_) 
    }
    
    creaMapLegend(input$giorno, input$scale.fixed, fn, input$calculus, input$palette ) 
    
    dt.label<- unname(unlist(dt.filtered[,input$calculus]))
     if( !is.element(input$calculus,c("delta", "totale_casi") ) )  label<-sprintf("%.2f",dt.label)
     else label <- sprintf("%d",   dt.label) 
    
    label[label=="NA"]<-""
    
    currentGiorno<<-as.character(input$giorno)
    
    leafletProxy("mymap") %>% 
      
      addWMSTiles(baseUrl = sprintf("https://geolab02.vs-ix.net/cgi-bin/mapserv?uselessCache=%s%s%s%s%s&map=%s/mapfile.map",
                                    input$giorno, input$scale.fixed, input$scale.funct_, input$calculus,
                                    input$palette, pwd   ) ,
                  options = WMSTileOptions(zIndex = 4, format = "image/png", transparent = T ,
                                           layerId = sprintf("%s_%s", basic.layerlist.list$overlayGroups$Casi_COVID19, input$giorno)  ),
                  layers="Casi_COVID19", 
                  layerId = sprintf("%s_%s", basic.layerlist.list$overlayGroups$Casi_COVID19, input$giorno),
                  group=basic.layerlist.list$overlayGroups$Casi_COVID19,
                  attribution = "Provincie ISTAT @CIRGEO" )  %>% 
      
    addLabelOnlyMarkers(data = as.data.frame(dt.filtered), lng = ~long, lat=~lat,
                        layerId = sprintf("%s%s", basic.layerlist.list$overlayGroups$Casi_COVID19labels, 
                                          dt.filtered$codice_provincia    ),
                        group =  basic.layerlist.list$overlayGroups$Casi_COVID19labels,
    
                        label = label ,
                        labelOptions = leaflet::labelOptions(zIndex = 10,
                          noHide = TRUE,
                         # direction = "bottom",
                          textOnly = T,
                          style= list(   
                            "font-size" = "11px",
                            "font-weight" = "bold",
                            "color"="white",
                            "text-shadow"="0px 0px 3px black" 
                          ),
                          #offset = c(0, -10),
                          opacity = 1
                        )
               ) 
 
     if(!input$scale.fixed) legenddata<-fn(unlist(dt.filtered[,input$calculus]))
     else             legenddata<-fn(unlist(dt.sorted2[,input$calculus]))
        
      leafletProxy("mymap") %>% 
            leaflet::addLegend( "bottomright", pal = pal, values = legenddata,
                             title = sprintf("<div  data-toggle ='tooltip' style='font-size:small;
                             white-space:nowrap; '>N. Cases&nbsp;&nbsp;<font style='color:red; 
                             font-weight:bold;  
                             cursor:pointer;' data-toggle ='tooltip' data-placement='left' 
                                             data-html ='true' title='Scale:&nbsp;%s<br>%s' >&nbsp;(?)</font>
                                             </div>%s<script> $('[data-toggle=\"tooltip\"]').tooltip();</script>", 
                                             functionList.lut[[input$scale.funct_]],
                                             ifelse(input$scale.fixed,
                                                    sprintf("Period: %s to %s<br>(%d&nbsp;days)", data.prima, 
                                                            data.ultima,
                                                            as.integer(data.ultima- data.prima) ), 
                                                    "" ),
                                             sprintf(" %s ", format(input$giorno, "%d&nbsp;%B") )), 
                             layerId="Legend_Casi_COVID19",
                             labFormat = labelFormat(prefix = "", big.mark = " ", 
                                                     transform = inv.funct_[[input$scale.funct_]] ),
                             opacity = 1 )
      
      
 #     leafletProxy("mymap") %>%  removeTiles(sprintf("%s%s",
  #                                                  basic.layerlist.list$overlayGroups$Casi_COVID19, preGiorno)) 
      
      
      preGiorno<<-as.character(input$giorno)
 
  })
  
  
  
} )

 

