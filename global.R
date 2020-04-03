library(shiny)
library(shinyBS)
library(shinyjs)
library(leaflet) 
library(leaflet.extras)
# library(rpostgis)
library(RColorBrewer) 
library(htmltools) 
library(DT)
library(dplyr)
library(htmlwidgets)
library(shinyWidgets)
#devtools::install_github("rstudio/fontawesome")
library(fontawesome)
library(dygraphs)
require(xts)
library(RCurl)

unique(readRDS("nLoginsIP.rds"))

logIPS<-function(ip){
  ips2save<-readRDS("nLoginsIP.rds")
  
  if(!is.null(ips2save[[ip]])){
    ips2save[[ip]]<-c(ips2save[[ip]], format(Sys.time(), ""))
  } else {
    ips2save[[ip]]<- format(Sys.time(), "") 
  }
   
  saveRDS(ips2save, "nLoginsIP.rds")
}
 
residenti.2019<-read.csv("residenti.csv", sep="\t", header = F, strip.white = T )
residenti.2019$V2<-NULL
names(residenti.2019)<-c("denominazione_provincia", "Maschi", "Femmine")
residenti.2019$Tot<-residenti.2019$Maschi + residenti.2019$Femmine
residenti.2019$denominazione_provincia<-as.character(residenti.2019$denominazione_provincia)
rownames(residenti.2019)<-residenti.2019$denominazione_provincia


paletteList.t<-list( 
  
  Person=c("#cccccc", 
           "#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb",
           "#41b6c4", "#1d91c0", "#225ea8", 
           "#6e016b", "#990000", "#d7301f", "#FF0000" ),
  Spectral=c("#cccccc",   rev( rainbow(20)[1:12])),
  YellowOrangeRed= brewer.pal(9,"YlOrRd"),
  RedYellowBlue= brewer.pal(11,"RdYlBu"),
  BlueYellowRed=rev(brewer.pal(11,"RdYlBu")),
  RedWhiteGrey= rev(brewer.pal(11,"RdGy"))
)
paletteList<- (names(paletteList.t))
dt.sorted2<-NULL
dt.filtered<-NULL
data.ultima<-NULL
data.prima<-NULL
currentGiorno<-NULL
preGiorno<-NULL
dati.tot2<-NULL
dati.naz1.ts<-NULL
dati.naz2.ts<-NULL
# n<-10
# image(1:n, 1, as.matrix(1:n),
# col= rainbow(n),
# xlab="", ylab = "", xaxt = "n", yaxt = "n", bty = "n")

##  PREPARAZIONE DATI ############
getData<-function(){
  
  ### Leggo su GITHUB ma anche fallback su CSV local  ------
  dati.prov<- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"
  if(!RCurl::url.exists(dati.prov,
                    .opts=list(timeout=2, verbose=F)) ){
    dt<-read.csv("dpc-covid19-ita-province.csv")  
    
  } else {
    dt<-read.csv(dati.prov) 
    write.csv(dt,"dpc-covid19-ita-province2.csv", row.names = F )
  }
  
  
   dati.naz<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
 
  if(!RCurl::url.exists(dati.naz,
                        .opts=list(timeout=2, verbose=F)) ){
    dt.naz<-read.csv("dpc-covid19-ita-andamento-nazionale.csv") 
  
    
  } else {
    dt.naz<-read.csv(dati.naz) 
    write.csv(dt.naz,"dpc-covid19-ita-andamento-nazionale2.csv", row.names = F )
  }
  
  
  dt$data<-as.Date(dt$data)
  dt<-dt[dt$codice_provincia<900,]
   
  dt.sorted<-dt[ order(dt$codice_provincia, dt$data), ]
   
  dt.sorted2 <- dt.sorted %>%
    group_by(codice_provincia) %>%
    mutate(  delta     =c(0, diff(totale_casi)) )
  
  aa<-mutate_each(dt.sorted2[, "totale_casi"], funs( (. - lag(.) )/(. + lag(.) )  ))
  dt.sorted2$delta.norm<-aa$totale_casi
  
  date<-unique(dt$data)
  data.ultima<<-last(sort(date))
  data.prima<<-first(sort(date))
  
  currentGiorno<<-data.ultima
  preGiorno<<-data.ultima
  
  dt.sorted2[which(is.na(dt.sorted2$delta.norm)), "delta.norm"] <-NA
  
  
  
  
  dt.sorted2$stato<-NULL
  dt.sorted2$codice_regione<-NULL
  dt.sorted2$denominazione_regione<-NULL
  dt.sorted2$note_it<-NULL
  dt.sorted2$note_en<-NULL
  dt.sorted2$denominazione_provincia<-as.character(dt.sorted2$denominazione_provincia)
  dt.sorted2<-as.data.frame(dt.sorted2)
  
  #dt.sorted3<-merge(dt.sorted2, residenti.2019)
  
  dt.sorted2$Popolazione<- residenti.2019[ dt.sorted2$denominazione_provincia, "Tot"] 
  
  dt.sorted2$totale_casi.normPop<-dt.sorted2$totale_casi/dt.sorted2$Popolazione * 10000
  dt.sorted2$delta.normPop<-dt.sorted2$delta/dt.sorted2$Popolazione * 10000
  dt.sorted2<<-dt.sorted2
  dt.filtered<<- dt.sorted2[dt.sorted2$data==data.ultima, ]
  
  dt.naz$stato<-NULL
  dt.naz$note_it<-NULL
  dt.naz$note_en<-NULL
  dt.naz$data<-as.Date(dt.naz$data)
  
   
   
  dt.naz1<-as.data.frame(dt.naz[, c( "terapia_intensiva", "ricoverati_con_sintomi")])
  dt.naz2<-as.data.frame(dt.naz[, c( "totale_positivi", "tamponi")])
  dt.naz2$tamponi<-dt.naz2$tamponi/1000
  # dati.tot.ts <<-  xts(dati.tot2, dati.tot2$data)
  # dati.tot.ts$data<<-NULL
  # dati.tot.ts$terapia_intensiva<<-as.numeric(dati.tot.ts$terapia_intensiva)
  # dati.tot.ts$ricoverati_con_sintomi<<-as.numeric(dati.tot.ts$ricoverati_con_sintomi)
  dates<-dt.naz$data
  dt.naz1$data<-NULL
  dt.naz2$data<-NULL
  # dati.tot.ts<<-ts(dati.tot2,   start = c(2020, as.numeric(format(dates[1], "%j"))),
  #                  frequency = 120 )
  dati.naz1.ts<<-zoo(dt.naz1, dates )
  dati.naz2.ts<<-zoo(dt.naz2, dates )
  
}
getData()
## PREPARO LEAFLET ############
linear<-function(x){ x } 
log10Per<-function(x){ log10(x+1) } 
logPer<-function(x){ log(x+1) }

inv.funct_ <- list(
  linear=function(x){ x } ,
  log10Per=function(x){ round(10^(x)-1,0) } ,
  logPer=function(x){ round(exp(x)-1, 0) },
  sqrt=function(x){ x^2 }
)




functionList<- list( "Linear"="linear",   
                     "Log10"="log10Per",
                     "Ln"= "logPer",
                     "Sqrt"= "sqrt"  )

functionList.lut <- names(functionList)
names(functionList.lut) <- functionList

pwd<-system("pwd", intern = T)

basic.layerlist.list<-list(    baseGroups = list( osm.bn="Map Night",osm.light ="Map Light", 
                                                  osm="None" ),
                               overlayGroups = list( Casi_COVID19="COVID-19", Casi_COVID19labels="Labels")
)
 
basic.layerlist<-list(
  baseGroups = unname(unlist(basic.layerlist.list$baseGroups )),
  overlayGroups = unname(unlist(basic.layerlist.list$overlayGroups ))
)
 
creaMapLegend<-function(giorno, fixed=T, fn=linear, calcolo="totale_casi", palette="Person"){
  opacity<-75 
  dt.filtered <<- dt.sorted2[dt.sorted2$data==giorno, ]
  
  #fn <- get(funcList)
 
  palette<-paletteList.t[[palette]]
  # palette<-c(
  #   "#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb",
  #   "#41b6c4", "#1d91c0", "#225ea8",
  #  # "#0c2c84", "#0c2c84",   "#8c6bb1", "#88419d",
  #   "#6e016b", "#990000", "#d7301f", "#FF0000" )
  # 
  if(fixed){  
    pal <<- colorNumeric( 
        palette = palette,  
        domain =  fn( unlist(dt.sorted2[,calcolo ]))
    )
    
    }
  else{ 
    pal <<- colorNumeric( 
      palette = palette,  
      domain = fn( unlist(dt.filtered[,calcolo ]) )
    )
  }
  
  class<-sprintf(" 
    CLASS
      NAME \"%s casi\"
      GROUP \"group1\"
      EXPRESSION \"%s\"
      STYLE 
         COLOR  \"%s\"
         OPACITY %d
      END
      STYLE 
         OUTLINECOLOR  255 255 255
         WIDTH 20
         MINWIDTH 0.2
         MAXWIDTH 5
      END
   END
    ", unlist(dt.filtered[, calcolo ]), dt.filtered$codice_provincia , 
                 pal(fn( unlist(dt.filtered[,calcolo ]))), opacity )
  
  mapfile <-sprintf(" 
  include \"header.map\"
  
  LAYER
   STATUS ON
   NAME \"Casi_COVID19\"
   
   include \"connection.map\"
   TYPE POLYGON
   DATA \"geom3857 from arch_geo.province_istat_2018 using unique id using srid=3857\"
   METADATA
     \"wms_title\" \"Casi_COVID-19\"
     \"wms_srs\"   \"EPSG:3857\"  
     \"wms_enable_request\" \"*\"
     \"ows_enable_request\" \"*\"
   END
   SIZEUNITS meters
   CLASSITEM \"cod_prov\"
   
   %s
   
  END
    
  include \"footer.map\" 
    ", 
 paste0(class, collapse="
         " ) )
  cat(mapfile, file="mapfile.map")
  
}  

 
creaMapLegend(data.ultima)
 
leafletMap <- leaflet(width="100%", height = 600) %>%
  
  onRender("function(el, x) { 
           mapElement=this;   
           this.on('layeradd', onLayerAddFunction);
           }") %>%  
  flyTo( 11, 43, 6)  %>% 
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
           attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
           options = tileOptions(zIndex = 1, preferCanvas=TRUE, maxZoom = 19, subdomains = "abcd" ), group=basic.layerlist.list$baseGroups$osm.bn)  %>% 
   addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
            attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
            options = tileOptions(zIndex = 2, preferCanvas=TRUE, maxZoom = 19, subdomains = "abcd" ), group=basic.layerlist.list$baseGroups$osm.light)  %>% 
 

#  hideGroup( as.character(basic.layerlist.list$overlayGroups) ) %>%
  showGroup( c(basic.layerlist.list$overlayGroups$Casi_COVID19 ) )   %>%
  # addMouseCoordinates() %>% 
  addLayersControl(baseGroups    =  basic.layerlist$baseGroups,
                   overlayGroups = basic.layerlist$overlayGroups,
                   options = layersControlOptions(collapsed = F) )  



#### testo vario
tooltips<-list(scale.fixed="<b style='font-weight:bold; color:red;'>
           Fixed Scale</b><br>If checked, this box will keep the scale constant to the range of the min and max of all values accross the time-span of analysis of whatever observation is plotted.
           If unchecked, the scale will have min and max values of the current day, and therefore change every time a different day is chosen. 
           The former allows comparing accross time, but has lower color contrast as range will be larger. 
           The latter allows more color contrast over small variations. ",
               calculus=" <b style='color:red;'>Total Cases</b>: total count of COVID-19 cases at selected day.<br><br>
    <b style='color:red;'>Daily Increase:</b> Difference of cases with previous day.<br><br>
    <b style='color:red;'>Normalized daily Increase (not implemented)</b>: difference between chosen day's number of COVID-19 cases (T1) and previous day (T0), over the sum, i.e. 
    (T0 - T1)/(T0 + T1) - ranges from -1 to 1, 0 means flat increase - this scale highlights initial cases and tends to 0 when new cases are a 
               small percentage of total cases.",
               scale.residents="Will normalize per residents in the area and scaled x1000 (i.e. number every 1000 residents).",
               scale.funct_="Scales can be linear or transformed to lognormal, log base 10 or square root to improve visualization of asymmetric (skewed) frequency distributions."   
         )
