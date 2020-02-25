

library(shiny)
library(shinythemes)
library(rgdal)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(raster)
library(htmltools)


#loading adm shapefiles
province<-readOGR(dsn="C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/KEN_adm",
            layer ="KEN_adm1" ,GDAL1_integer64_policy = TRUE)
kenya<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/adm_iebc",
               layer="ken_admbnda_adm0_iebc_20191031",GDAL1_integer64_policy = TRUE)
district<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/adm_iebc",
                  layer="ken_admbnda_adm1_iebc_20191031",GDAL1_integer64_policy = TRUE)
division<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/adm_iebc",
                  layer="ken_admbnda_adm2_iebc_20191031",GDAL1_integer64_policy = TRUE)

#loading non revenue shapefiles
nrw<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/NRW",
             layer="nrw",GDAL1_integer64_policy = TRUE)




#loading kenya var definitions
ke_var<-read.csv("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/kenya_variables.csv")

#loading hydrology shapefiles
basins<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/Hydrology/Basins",
                  layer="Kenya_Basins",GDAL1_integer64_policy = TRUE)
lakes<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/Hydrology/kenlakes/KEN_Lakes",
                layer="KEN_Lakes",GDAL1_integer64_policy = TRUE)
kerivers<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/Hydrology/kenya_major_rivers",
               layer="kenya_major_rivers",GDAL1_integer64_policy = TRUE)


#loading demographics tif files
poverty<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/Demographics/Poverty Rate/ken08povmpi.tif")

time<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/Demographics/Time to city/accessibility_to_cities.tif")

pop<-readOGR(dsn = "C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/pop",
                layer="pop",GDAL1_integer64_policy = TRUE)


#loading water & sanitation tif files
improved_water<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/Population using improved water source/KE2014DHS_WSSRCEPIMP_MS_MEAN_v01.tif")
no_toilet<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/Population with no toilet facility/KE2014DHS_WSTLETPNFC_MS_MEAN_v01.tif")
tariff<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/rates/kenya_tariff.tif")
no_water<-raster("C:/Users/Brian Lenovo/Desktop/Kenya demo/app/Kenya_demo/data/without improved water access/population without improved water access.tif")

#changing crs
crs(poverty)<- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(time)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(improved_water)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(no_toilet)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(tariff)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(no_water)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


# Define UI for application
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # Application title
  titlePanel("KENYA DEMO ATLAS"),
  
  # Sidebar with shapefile options
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "adm",
        label = "ADMINISTRATIVE BOUNDARIES",
        choices = c("Country"=1,
                    "Province"=2,
                    "County"=3,
                    "Sub-County"=4
                    #"Location"=5,
                    #"Sub-location"=6
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
      ,width = 3
    ),
    
    # tab panels
    mainPanel(
      tabsetPanel(
        tabPanel("Map",leafletOutput("maps",height = 500)),
        tabPanel("About",wellPanel(p("The kenya Demo Atlas displays the administrative,
                                     demographic, water and sanitation spatial data for Kenya."),
                                   p("The combination of these data enables the government, policymakers at all
                                     levels, WASH experts and other development partners
                                     to gain a spatial understanding of the dynamics affecting Kenya, gain insights
                                     and make informed decisions."),
                                   p("The table below gives a definition of the variables and the sources:")),
                 uiOutput("table"))
        
      ),width = 9
    )
  )
)

#Server functionality
server <- function(input, output,session) {
  
  #poverty legend
  color3<-c("orange1","orangered","red3","red4","tomato4")
  brk<-seq(0,1,0.2)
  cm<-colorBin(palette = color3,bins =brk,domain = brk)
  
  #time to city legend
  color4<-c("orange1","orangered","red3","red4","tomato4")
  brk2<-seq(0,10000,500)
  cm2<-colorBin(palette = color4,bins =brk2,domain = brk2)
  
  #improved water legend
  color5<-c("orange1","orangered","red3","red4","tomato4")
  brk3<-seq(0,1,0.2)
  cm3<-colorBin(palette = color5,bins =brk3,domain =brk3)
  
  #unimproved water legend
  color5<-c("orange1","orangered","red3","red4","tomato4")
  brk4<-seq(0,40,2)
  cm4<-colorBin(palette = color5,bins =brk4,domain =brk4)
  
  #tariff legend
  color5<-c("orange1","orangered","red3","red4","tomato4")
  brk5<-seq(0,0.03,0.006)
  cm5<-colorBin(palette = color5,bins =brk5,domain =brk5)
  
  
  #population legend
  palpop<-colorBin("plasma", pop$PD.NpSK, 10, pretty = TRUE)
  
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
  # 
  #observe function for adm shapefiles
  observe({
    clear<-leafletProxy("maps") %>% clearShapes() 
    
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
    
    if ("2" %in% input$adm){
      pal<-colorFactor(topo.colors(8),province$NAME_1)
      clear %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = province,
          # color = ~pal(NAME_1),
          # #fillColor = "red",
          # smoothFactor = 0.5,
          weight = 2.5, 
          # opacity = 1.0,
          # fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =~NAME_1,
          popup = ~paste("<strong>Province:</strong>",NAME_1

          )
        )
    }
    
    if ("3" %in% input$adm){
      pal2<-colorFactor(topo.colors(47),district$ADM1_EN)
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = district,
          # color = ~pal2(ADM1_EN),
          # #fillColor = "red",
          # smoothFactor = 0.5,
          weight = 2.5, 
          # opacity = 1.0,
          # fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =~ADM1_EN,
          popup = ~paste(
                         "<strong>County:</strong>",ADM1_EN

          )
        )
    }
    
    if ("4" %in% input$adm){
      pal3<-colorFactor(topo.colors(290),division$ADM2_EN)
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = division,
          # color = ~pal3(ADM2_EN),
          # #fillColor = "red",
          # smoothFactor = 0.5,
          weight = 2.5, 
          # opacity = 1.0,
          # fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =~ADM2_EN,
          popup = ~paste(
                         "<strong>County:</strong>",ADM1_EN,
                         "<br>",
                         "<strong>Sub-County:</strong>",ADM2_EN


          )
        )
    }
    
    if ("5" %in% input$water){
      pal4<-colorFactor(topo.colors(5),basins$Name)
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = basins,
          # color = ~pal4(Name),
          # smoothFactor = 0.5,
          weight = 2.5, 
          # opacity = 1.0,
          # fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =~Name,
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
    }
    
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
          label =~Descriptio,
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
          pal = cm, values = brk, title = "Poverty Rate"
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

    }
    
    
    if ("10" %in% input$demo){
      #leaflet(poverty) %>%
      clear %>% clearControls() %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addPolygons(
          data = pop,
          color =  ~palpop(PD.NpSK),
          #color = cm3,
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label = ~ADM1_EN,
          popup = ~paste(
            "<strong>County:</strong>",ADM1_EN,
            "<br>",
            "<strong>Population Density:</strong>",PD.NpSK

          )
          
        ) %>%
         addLegend(title = "Population Density",
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
          pal = cm3, values = brk3, title = "Population using improved water source"
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
          pal = cm3, values = brk3, title = "Population with no toilet facility"
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
          pal = cm4, values = brk4, title = "Population without improved water access"
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
  }
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

