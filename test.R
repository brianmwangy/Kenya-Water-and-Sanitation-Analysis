library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(tidyverse)
library(RColorBrewer)
library(raster)
library(htmltools)
library(DT)
library(readr)

#loading adm shapefiles
province<-readOGR(dsn="C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/KEN_adm",
                  layer ="KEN_adm1" ,GDAL1_integer64_policy = TRUE)
kenya<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/adm_iebc",
               layer="ken_admbnda_adm0_iebc_20191031",GDAL1_integer64_policy = TRUE)
district<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/adm_iebc",
                  layer="ken_admbnda_adm1_iebc_20191031",GDAL1_integer64_policy = TRUE)
division<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/adm_iebc",
                  layer="ken_admbnda_adm2_iebc_20191031",GDAL1_integer64_policy = TRUE)

#loading non revenue shapefiles
nrw<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/NRW",
             layer="nrw2",GDAL1_integer64_policy = TRUE)




#loading kenya var definitions
ke_var<-read.csv("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/kenya_variables.csv")

#loading hydrology shapefiles
basins<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/Hydrology/Basins",
                layer="Kenya_Basins",GDAL1_integer64_policy = TRUE)
lakes<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/Hydrology/kenlakes/KEN_Lakes",
               layer="KEN_Lakes",GDAL1_integer64_policy = TRUE)
kerivers<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/Hydrology/kenya_major_rivers",
                  layer="kenya_major_rivers",GDAL1_integer64_policy = TRUE)


#loading demographics tif files
poverty<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/Demographics/Poverty Rate/ken08povmpi.tif")

time<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/Demographics/Time to city/accessibility_to_cities.tif")

pop<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/pop",
             layer="pop",GDAL1_integer64_policy = TRUE)


#loading water & sanitation tif files
improved_water<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/Population using improved water source/KE2014DHS_WSSRCEPIMP_MS_MEAN_v01.tif")
no_toilet<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/Population with no toilet facility/KE2014DHS_WSTLETPNFC_MS_MEAN_v01.tif")
tariff<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/rates/kenya_tariff.tif")
no_water<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/Shiny_app/data/without improved water access/population without improved water access.tif")

#changing crs
crs(poverty)<- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(time)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(improved_water)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(no_toilet)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(tariff)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(no_water)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")



ui <-dashboardPage(
  dashboardHeader(
    title = "KENYA DEMO ATLAS", titleWidth = 400
  ),
  
  dashboardSidebar(
      sidebarMenu(
        menuItem(
          "About",
          tabName = "intro",
          icon = icon("info")
        ),
        
        menuItem(
          "Map",
          tabName = "map",
          icon = icon("map")
        )
      )             
                   
                   ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
  tabItems(
    
    tabItem(
      tabName = "intro",
      div(
        br(),
        h1("Background."),
        h3("The kenya Demo Atlas displays the administrative,demographic, water and sanitation spatial data for Kenya."),
        h3("The combination of these data enables the government, policymakers at all levels, WASH experts and other development partners
                                     to gain a spatial understanding of the dynamics affecting Kenya, gain insights
                                     and make informed decisions."
                                     
           ),
        br(),
        h1("Dataset."),
        h3("The table below gives a definition of the variables and the sources:")
        
                                    
      ),#end div
      
      uiOutput("table")
    ), #end tabname
    
    tabItem(
        tabName = "map",
      
    fluidRow(
      column(width = 3,
             box(title="GEOSPATIAL DATA",status = "primary",width = NULL,
                 height = 600, solidHeader = TRUE, collapsible = TRUE,
                 checkboxGroupInput(
                   inputId = "adm",
                   label = "ADMINISTRATIVE BOUNDARIES",
                   choices = c("Country"=1,
                               "Province"=2,
                               "County"=3,
                               "Sub-County"=4
                   )
                 ),
                 checkboxGroupInput(
                   inputId = "water",
                   label = "HYDROLOGY",
                   choices = c("Water Basins"=5,
                               "Lakes"=6,
                               "Rivers"=7
                   )
                 ),
                 
                 checkboxGroupInput(
                   inputId = "demo",
                   label = "DEMOGRAPHICS",
                   choices = c("Poverty Rate"=8,
                               "Time to city"=9,
                               "Population Density"=10
                   )
                 ),
                 
                 checkboxGroupInput(
                   inputId = "san",
                   label = "WATER AND SANITATION",
                   choices = c("Population using improved water source"=11,
                               "Population with no toilet facility"=12,
                               "Population without improved water access"=13,
                               "Water tariff Rates"=14
                   )
                 )
             )
      ),
      
      column(
        width = 6,
        box(
            title = "MAP",
            status = "primary",solidHeader = TRUE,
            width = NULL,height = 600,collapsible = TRUE,
            leafletOutput("maps",height = 500)
        )
      ),
      
      column(width = 3,
             box(title="WATER UTILITY PERFORMANCE.", solidHeader = TRUE,
                 width = NULL,height = 600, status = "primary", collapsible = TRUE,
                 checkboxGroupInput(
                   inputId = "utility",
                   label = "INDICATORS.",
                   choices = c("Non Revenue Water(%)"=15,
                               "Drinking Water Quality(%)"=16,
                               "Hours Of Water Supply Per Day"=17,
                               "Staff Productivity(No of Staff Per 1000 Connections)"=18,
                               "Water Coverage(%)"=19,
                               "Personnel Expenditures as % of Total Operating and   Maintenance Costs"=20,
                               "Revenue Collection Efficiency(%)"=21,
                               "Operating and Maintenance Cost Coverage(%)"=22,
                               "Metering Ratio(%)"=23
                              
                   )
                 ) 
             )
            
      )
    ) #end row
    
    ) #end tab item
  )#end tab items
    
  )
  
  
) 

server<-function(input,output,session){
  
  #poverty legend
  color3<-c("orange1","orangered","red3","red4","tomato4")
  brk<-seq(0,1,0.2)
  cm<-colorBin(palette = color3,bins =brk,na.color="#00000000",domain = brk)
  
  #time to city legend
  color4<-c("orange1","orangered","red3","red4","tomato4")
  brk2<-seq(0,10000,500)
  cm2<-colorBin(palette = color4,bins =brk2,na.color="#00000000",domain = brk2)
  
  #improved water legend
  color5<-c("orange1","orangered","red3","red4","tomato4")
  brk3<-seq(0,1,0.2)
  cm3<-colorBin(palette = color5,bins =brk3,na.color="#00000000",domain =brk3)
  
  #unimproved water legend
  color5<-c("orange1","orangered","red3","red4","tomato4")
  brk4<-seq(0,40,10)
  cm4<-colorBin(palette = color5,bins =brk4,na.color="#00000000",domain =brk4)
  
  #tariff legend
  color5<-c("orange1","orangered","red3","red4","tomato4")
  brk5<-seq(0,0.03,0.006)
  cm5<-colorBin(palette = color5,bins =brk5,na.color="#00000000",domain =brk5)
  
  
  #population legend
  #bk=seq(0,500,6500)
  palpop<-colorBin("YlOrBr", pop$PD.NpSK, na.color = "#00000000", 10)
  

  #nrw legend
  brk6<-seq(0,100,20)
  nrwpop<-colorBin(palette = "YlOrBr",
                   domain =  nrw$NRW, na.color = "transparent", bins = brk6)
  
  #DWQ legend
  nrwpop2<-colorBin(palette = "YlOrBr",
                   domain =  nrw$DWQ, na.color = "transparent", bins = brk6)
  
  #hrs of supply hours
  brk7<-seq(0,24,6)
  nrwpop3<-colorBin(palette = "YlOrBr",
                    domain =  nrw$Spp_H__, na.color = "transparent", bins = brk7)
  
  #staff productivity
  brk8<-seq(0,50,10)
  nrwpop4<-colorBin(palette = "YlOrBr",
                    domain =  nrw$Stff_Pr, na.color = "transparent", bins = brk8)
  
  #Revenue collection efficiency
  brk9<-seq(0,120,20)
  nrwpop5<-colorBin(palette = "YlOrBr",
                   domain =  nrw$Rvn_C_E, na.color = "transparent", bins = brk9)
  
  #operation & Maintance cost
  brk10<-seq(0,150,30)
  nrwpop6<-colorBin(palette = "YlOrBr",
                    domain =  nrw$O_M_C_C, na.color = "transparent", bins = brk10)
  
  
  #rendering the data table
  output$table<-renderTable(ke_var)
  
  #rendering the basemap
  output$maps<-renderLeaflet(
    leaflet(kenya) %>%
      setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      
      addPolygons(
        #fillColor = "blue",
        # smoothFactor = 0.5,
        # weight = 2, opacity = 1.0,
        # fillOpacity = 1.0
      )
    
  )
  
  #observe function for adm shapefiles
  observe({
    clear<-leafletProxy("maps") %>% clearShapes() %>% clearMarkers() %>% clearImages()
    
    if ("1" %in% input$adm){
      kepal<-colorFactor(topo.colors(1),kenya$ADM0_EN)
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = kenya,
          #fillColor = "red",
          #color = ~kepal(ADM0_EN),
          #smoothFactor = 0.5
          weight = 2.5 
          #opacity = 1.0,
          # fillOpacity = 1.0
        )
    }
    
    prv= paste("<strong>Province:</strong>",province$NAME_1
                 
    ) %>% lapply(htmltools::HTML)
    
    if ("2" %in% input$adm){
      pal<-colorFactor(topo.colors(8),province$NAME_1)
      clear %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = province,
          weight = 2.5, 
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =prv
        )
    } #end
    
    dst= paste(
               "<strong>County:</strong>",district$ADM1_EN
               
    ) %>% lapply(htmltools::HTML)
    
    if ("3" %in% input$adm){
      pal2<-colorFactor(topo.colors(47),district$ADM1_EN)
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = district,
          weight = 2.5, 
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =dst
        )
    } #end
    
    
    dvn= paste(
      "<strong>Sub-County:</strong>",division$ADM2_EN
      
    ) %>% lapply(htmltools::HTML)
    
    if ("4" %in% input$adm){
      pal3<-colorFactor(topo.colors(290),division$ADM2_EN)
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = division,
          weight = 2.5, 
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =dvn,
          popup = ~paste(
            "<strong>Sub-County:</strong>",division$ADM2_EN
          )
        )
    } #end
    
    bsn= paste(
      "<strong>Basin:</strong>",basins$Name
      
    ) %>% lapply(htmltools::HTML)
    
    if ("5" %in% input$water){
      pal4<-colorFactor(topo.colors(5),basins$Name)
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = basins,
          weight = 2.5, 
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =bsn,
          popup = ~paste("<strong>Basin:</strong>",Name
                         
          )
        )
    }
    
    if ("6" %in% input$water){
      #pal4<-colorFactor(topo.colors(5),basins$Name)
      clear %>% clearControls() %>%
        #addTiles() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = lakes,
          #color = ~pal4(Name),
          fillColor = "yellow",
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          )
          
        )
    } #end
    
    
    rvr= paste(
      "<strong>River Type:</strong>",kerivers$Descriptio
      
    ) %>% lapply(htmltools::HTML)
    
    if ("7" %in% input$water){
      #pal5<-colorFactor(topo.colors(3),kerivers$Descriptio)
      pal5<-colorFactor(rainbow(7),kerivers$Descriptio)
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolylines(
          data = kerivers,
          color = ~pal5(Descriptio),
          #color = "yellow",
          #fillColor = "brown",
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "yellow",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =rvr,
          popup = ~paste("<strong>River Type:</strong>",Descriptio
                         
          )
        )
    }
    
    
    if ("8" %in% input$demo){
      #leaflet(poverty) %>% 
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addRasterImage(
          poverty,
          colors = cm,
          opacity = 1.0,
          project = TRUE
          
        ) %>% addLegend(
          pal = cm, values = brk, title = "Poverty Rate(%)"
        )
      
    }
    
    if ("9" %in% input$demo){
      #leaflet(poverty) %>%
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addRasterImage(
          time,
          colors = cm2,
          opacity = 1.0,
          project = TRUE
          
        ) %>% addLegend(
          pal = cm2, values = brk2, title = "Time to city in Minutes"
        )
      
    } #end
    
    pp= paste(
      "<strong>County:</strong>",district$ADM1_EN,
      "<br>",
      "<strong>Population Density(Per KM2):</strong>",pop$PD.NpSK
      
    ) %>% lapply(htmltools::HTML)
    
    if ("10" %in% input$demo){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = pop,
          color =  ~palpop(PD.NpSK),
          smoothFactor = 0.5,
          weight = 2, 
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "blue",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = pp,
          popup = ~paste(
            "<strong>County:</strong>",ADM1_EN,
            "<br>",
            "<strong>Population Density(per KM2):</strong>",PD.NpSK
            
          )
          
        ) %>%
        addLegend(title = "Population Density(per KM2)",
                  pal = palpop, values = pop$PD.NpSK, opacity = 1)
    }
    
    
    if ("11" %in% input$san){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addRasterImage(
          improved_water,
          color = cm3,
          opacity = 1.0,
          project = TRUE
        ) %>%
        addLegend(
          pal = cm3, values = brk3, title = "Population using improved water source(%)"
        )
    }
    
    
    if ("12" %in% input$san){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addRasterImage(
          no_toilet,
          color = cm3,
          opacity = 1.0,
          project = TRUE
        ) %>%
        addLegend(
          pal = cm3, values = brk3, title = "Population with no toilet facility(%)"
        )
    }
    
    if ("13" %in% input$san){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addRasterImage(
          no_water,
          color = cm4,
          opacity = 1.0,
          project = TRUE
        ) %>%
        addLegend(
          pal = cm4, values = brk4, title = "Population without improved water access(%)"
        )
    }
    
    if ("14" %in% input$san){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addRasterImage(
          tariff,
          color = cm5,
          opacity = 1.0,
          project = TRUE
        ) %>%
        addLegend(
          pal = cm5, values = brk5, title = "Water tariff Rates"
        )
    }
    
    # ikons <- makeAwesomeIcon(
    #   icon = "tint",
    #   markerColor = "blue",
    #   iconColor = "white",
    #   library = "fa"
    # )
    # 
    # ikon2<-makeAwesomeIcon(icon = "home", markerColor = "green",
    #                        library = "ion")
    
    #NRW label text
    txt = paste("<strong>Utility:</strong>",nrw$utility,"<br>",
                  "<strong>Utility Type:</strong>",nrw$Utlty_T,"<br>",
                    "<strong>Non Revenue Water:</strong>",nrw$NRW,"%",sep = ""
                            
                            ) %>% lapply(htmltools::HTML)
    
    
    
    if ("15" %in% input$utility){
        clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(
          data = nrw,
          #icon=ikons,
          fillColor=~nrwpop(NRW),
          fillOpacity = 1.0,
          #color=~nrwpop(NRW)
          radius = 8,
          stroke = FALSE,
          label =txt,
          #label=~popup,
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto")
      
        ) %>%
        
        addLegend(
          pal = nrwpop,
          values = nrw$NRW,
          opacity = 1.0,
          title = "Non Revenue Water(%)"
        )
    }
    
    #end
    
    #DWQ label text
    txt2 = paste("<strong>Utility:</strong>",nrw$utility,"<br>",
                "<strong>Utility Type:</strong>",nrw$Utlty_T,"<br>",
                "<strong>Drinking Water Quality:</strong>",nrw$DWQ,"%",sep = ""
                
    ) %>% lapply(htmltools::HTML)
    
    if ("16" %in% input$utility){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(
          data = nrw,
          #icon=ikons,
          fillColor=~nrwpop2(DWQ),
          fillOpacity = 1.0,
          #color=~nrwpop(NRW)
          radius = 8,
          stroke = FALSE,
          label =txt2,
          #label=~popup,
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto")
          
        ) %>%
        
        addLegend(
          pal = nrwpop2,
          values = nrw$DWQ,
          opacity = 1.0,
          title = "Drinking Water Quality(%)"
        )
    }
    
    #end
    
    #Hrs water supply label text
    txt3 = paste("<strong>Utility:</strong>",nrw$utility,"<br>",
                 "<strong>Utility Type:</strong>",nrw$Utlty_T,"<br>",
                 "<strong>Hours of Water supply per day:</strong>",nrw$Spp_H__,"hrs",sep = ""
                 
    ) %>% lapply(htmltools::HTML)
    
    if ("17" %in% input$utility){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(
          data = nrw,
          #icon=ikons,
          fillColor=~nrwpop3(Spp_H__),
          fillOpacity = 1.0,
          #color=~nrwpop(NRW)
          radius = 8,
          stroke = FALSE,
          label =txt3,
          #label=~popup,
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto")
          
        ) %>%
        
        addLegend(
          pal = nrwpop3,
          values = nrw$Spp_H__,
          opacity = 1.0,
          title = "Hours of water supply per day."
        )
    }
    #end
    
    #Hrs water supply label text
    txt4 = paste("<strong>Utility:</strong>",nrw$utility,"<br>",
                 "<strong>Utility Type:</strong>",nrw$Utlty_T,"<br>",
                 "<strong>Staff Productivity(No of Staff Per 1000 Connections):</strong>",nrw$Stff_Pr,"staff",sep = ""
                 
    ) %>% lapply(htmltools::HTML)
    
    if ("18" %in% input$utility){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(
          data = nrw,
          #icon=ikons,
          fillColor=~nrwpop4(Stff_Pr),
          fillOpacity = 1.0,
          #color=~nrwpop(NRW)
          radius = 8,
          stroke = FALSE,
          label =txt4,
          #label=~popup,
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto")
          
        ) %>%
        
        addLegend(
          pal = nrwpop4,
          values = nrw$Stff_Pr,
          opacity = 1.0,
          title = "Staff Productivity(No of Staff Per 1000 Connections)"
        )
    }
    
    #end
    
    #water coverage label text
    txt5 = paste("<strong>Utility:</strong>",nrw$utility,"<br>",
                 "<strong>Utility Type:</strong>",nrw$Utlty_T,"<br>",
                 "<strong>Water Coverage:</strong>",nrw$Wtr_cvr,"%",sep = ""
                 
    ) %>% lapply(htmltools::HTML)
    
    if ("19" %in% input$utility){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(
          data = nrw,
          #icon=ikons,
          fillColor=~nrwpop(Wtr_cvr),
          fillOpacity = 1.0,
          #color=~nrwpop(NRW)
          radius = 8,
          stroke = FALSE,
          label =txt5,
          #label=~popup,
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto")
          
        ) %>%
        
        addLegend(
          pal = nrwpop,
          values = nrw$Wtr_cvr,
          opacity = 1.0,
          title = "Water Coverage(%)"
        )
    }
    #end
    
    #personnel expenditure label text
    txt6 = paste("<strong>Utility:</strong>",nrw$utility,"<br>",
                 "<strong>Utility Type:</strong>",nrw$Utlty_T,"<br>",
                 "<strong>Personnel Expenditure as % of Total Operating & Maintenance Costs:</strong>",nrw$Prsnnl_,"%",sep = ""
                 
    ) %>% lapply(htmltools::HTML)
    
    if ("20" %in% input$utility){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(
          data = nrw,
          #icon=ikons,
          fillColor=~nrwpop(Prsnnl_),
          fillOpacity = 1.0,
          #color=~nrwpop(NRW)
          radius = 8,
          stroke = FALSE,
          label =txt6,
          #label=~popup,
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto")
          
        ) %>%
        
        addLegend(
          pal = nrwpop,
          values = nrw$Prsnnl_,
          opacity = 1.0,
          title = "Personnel Expenditure as % of Total Operating & Maintenance Costs."
        )
    }
    
    #end
    
    #Revenue collection efficiency label text
    txt7 = paste("<strong>Utility:</strong>",nrw$utility,"<br>",
                 "<strong>Utility Type:</strong>",nrw$Utlty_T,"<br>",
                 "<strong>Revenue Collection Efficiency:</strong>",nrw$Rvn_C_E,"%",sep = ""
                 
    ) %>% lapply(htmltools::HTML)
    
    if ("21" %in% input$utility){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(
          data = nrw,
          #icon=ikons,
          fillColor=~nrwpop5(Rvn_C_E),
          fillOpacity = 1.0,
          #color=~nrwpop(NRW)
          radius = 8,
          stroke = FALSE,
          label =txt7,
          #label=~popup,
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto")
          
        ) %>%
        
        addLegend(
          pal = nrwpop5,
          values = nrw$Rvn_C_E,
          opacity = 1.0,
          title = "Revenue Collection Efficiency(%)."
        )
    }
    #end
    
    #Operation & Maintenance cost label text
    txt8 = paste("<strong>Utility:</strong>",nrw$utility,"<br>",
                 "<strong>Utility Type:</strong>",nrw$Utlty_T,"<br>",
                 "<strong>Operation & Maintenance cost coverage:</strong>",nrw$O_M_C_C,"%",sep = ""
                 
    ) %>% lapply(htmltools::HTML)
    
    if ("22" %in% input$utility){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(
          data = nrw,
          #icon=ikons,
          fillColor=~nrwpop6(O_M_C_C),
          fillOpacity = 1.0,
          #color=~nrwpop(NRW)
          radius = 8,
          stroke = FALSE,
          label =txt8,
          #label=~popup,
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto")
          
        ) %>%
        
        addLegend(
          pal = nrwpop6,
          values = nrw$O_M_C_C,
          opacity = 1.0,
          title = "Operation & Maintenance cost coverage(%)."
        )
    }
    
    #end
    
    #Metering ratio label text
    txt9 = paste("<strong>Utility:</strong>",nrw$utility,"<br>",
                 "<strong>Utility Type:</strong>",nrw$Utlty_T,"<br>",
                 "<strong>Metering Ratio:</strong>",nrw$Mtrng_R,"%",sep = ""
                 
    ) %>% lapply(htmltools::HTML)
    
    if ("23" %in% input$utility){
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addCircleMarkers(
          data = nrw,
          #icon=ikons,
          fillColor=~nrwpop(Mtrng_R),
          fillOpacity = 1.0,
          #color=~nrwpop(NRW)
          radius = 8,
          stroke = FALSE,
          label =txt9,
          #label=~popup,
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto")
          
        ) %>%
        
        addLegend(
          pal = nrwpop,
          values = nrw$Mtrng_R,
          opacity = 1.0,
          title = "Metering Ratio(%)."
        )
    }
    #end
  }
  
  )
  
}

shinyApp(ui,server)
 

