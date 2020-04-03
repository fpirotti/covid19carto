function(req) { 
  ips<-c()
  if(!is.null(req[["HTTP_X_FORWARDED_FOR"]]) && req[["HTTP_X_FORWARDED_FOR"]]!="127.0.0.1") ips<-c(req[["HTTP_X_FORWARDED_FOR"]])
  if(!is.null(req[["REMOTE_ADDR"]]) && req[["REMOTE_ADDR"]]!="127.0.0.1") ips<-c(ips, req[["REMOTE_ADDR"]])
 
  logIPS(ips)
 
  
  fluidPage(
    useShinyjs(),
    tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "myStyle.css?v=sff"),
     tags$script(src='script.js?v=e3')
    ),
 
  
  fluidRow(  
    # tags$img(src="logocirgeo.png",  style="min-width:40px; width:25%; max-width:100px;   
    #          display:inline;z-index: 999999999; position:absolute; bottom: 3px; left: 15px;"  ) ,
    # tags$img(src="logotesaf.svg",  style="min-width:70px;  width:25%; max-width:140px;
    # display:inline;z-index: 999999999; position:absolute; background:white; bottom: 10px; left: 105px;"  ),
    # 
    div(style="font-size:11px; color:black; width:100%; text-align:center; margin: auto 1px",
        HTML("(c) Francesco Pirotti - <a href='http://www.cirgeo.unipd.it' target=_blank>CIRGEO Interdepartmental Research Center of Geomatics</a> / <a href='https://www.tesaf.unipd.it' target=_blank>TESAF Department</a>
             - <a href='https://www.unipd.it' target=_blank>Università degli Studi di Padova</a>")),
       div(id="infoPanel", style="padding:15px; display:none; overflow-y:auto;
           z-index:999999; background:white; border:solid black 1px; border-radius:5px;
           position:absolute; opacity:0.95; left:10px; top:20px; right:10px; bottom:10px;" ,
        
        tags$div( HTML("&nbsp;(X)&nbsp;&nbsp;CHIUDIMI "), onclick="$('#infoPanel').toggle()", style="cursor:pointer; display:inline; font-weight:bold;"  ) ,
   #     tags$img(src="logocirgeo.png",  style="min-width:70px; width:25%; max-width:100px; display:inline;"  ) ,
   #     tags$img(src="logotesaf.svg",  style="min-width:150px; margin-top:15px; width:25%; max-width:250px; display:inline;"  ),
        div(
          HTML(
        sprintf("<h5 class='info2'>Fixed scale</h5> 
        %s<hr> 
        <h5 class='info2'>Values to plot</h5>%s <hr>
    <h5 class='info2'>Scale</h5>"
             ,   tooltips$scale.fixed, tooltips$calculus, tooltips$scale.funct_   ))
        )) ,
   # helpText("There are 615 experiments\n",
   #          
   #          "and 230595 peptides.",
   #          
   #          "minExp = 2, minPep = 2."), 
    column(12, id="sliderContainer",   
           div( style="margin:10px; font-size:25px;width: 16px;  float: left; color:green; cursor:pointer;", 
                onclick="$('#infoPanel').toggle()", icon("info")  ) , 
           sliderInput("giorno", NULL, width = "95%",
                           min = data.prima, max = data.ultima,
                           value = data.ultima, step = 1,
                           animate = TRUE) )
  ),
######### DOMANDE - CASI TOTALI E' CUMULATA? O SE GUARITI DIMINUISCONO? 
## "Normalized daily Increase (%)"= "delta.norm" 
    fluidRow( 
    column(3, style="font-weight:bold;",
           dateInput("date1", NULL, value = data.ultima, format = 'DD dd MM yyyy') ),
    column(3, div(  selectInput("calculus", NULL,  
              choices = list( 
                "Total cases"="totale_casi",
                "Daily cases"="delta",
                "Total cases / 10 000 residents"="totale_casi.normPop",
                "Daily cases / 10 000 residents"="delta.normPop" ) )
                    )
          ),
   
 #   column(1,  `data-toggle` ="tooltip" , `data-html` ="true" ,   `data-placement`="bottom" , title=tooltips$scale.fixed, 
#           checkboxInput("scale.fixed", "F.S.", value = F )),
    column(2, 
           div(style="display: inline-block; margin-top: 12px; width:50px; ", 
                  `data-toggle` ="tooltip" , `data-html` ="true" ,   `data-placement`="bottom" , title=tooltips$scale.fixed, 
                  prettyCheckbox(
              inputId = "scale.fixed", label = "FixScale", icon = icon("check"),
              animation = "pulse", plain = TRUE, outline = TRUE ) ) #,
    # div(style="display: inline-block;vertical-align:top; width:150px; ",  `data-toggle` ="tooltip" , `data-html` ="true" ,   `data-placement`="bottom" ,
    #     title=tooltips$scale.residents, 
    #     prettyCheckbox(
    #   inputId = "scale.residents", label = "x1000 residents", icon = icon("users"),
    #   animation = "pulse", plain = TRUE, outline = TRUE
    # ) ) 
    ),
    column(2, title="Scales",  `data-toggle` ="tooltip" ,  `data-placement`="bottom" ,  
           selectInput("scale.funct_", NULL,  choices = functionList ) ),
    column(2, title="Color Palette",   `data-toggle` ="tooltip" ,  `data-placement`="bottom" , 
           selectInput("palette", NULL,  choices = paletteList, selectize = T ) )
    ),
  

    # sliderInput("opac", "Transparency",
    #             min = 1, max=100,
    #             value = 50, step = 1),
    
    leafletOutput("mymap") ,
    dygraphOutput("dygraph"),
    dygraphOutput("dygraph2")

) }
 

