##########################################################################################################################################################################################################################
# Title         : Tracking and Analyzing the Spatial Temporal Variation of Australian Bushfire 2019-2020 Using MODIS and VIIRS Data
#
# Purpose       : This Script is written as a final project for the Graphic Course 04-GEO-SOS1 (http://eagle-science.org/project/scientific-graphics/)
#
# Author        : Walid Ghariani (linkedin: https://www.linkedin.com/in/walid-ghariani-893365138/) (E-mail: walid.ghariani@stud-mail.uni-wuerzburg.de | walid11ghariani@gmail.com) 
#
# Input         : csv data from diffrent Instrument/Satellites (MODIS C6/Terra-Aqua; VIIRS/S-NPP ; VIIRS/NOAA 20)
#
# Processing    : Arrange the data, Create Functions and Deploy the R shiny app 
#
# Output        : Australian Bushfire 2019-2019 R Shiny App
##########################################################################################################################################################################################################################
list.of.packages <- c("ggplot2", "dplyr", "plyr", "tidyquant", 
                      "shiny","shinydashboard","shinycssloaders","leaflet","leaflet.extras")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos = "http://cran.rstudio.com/", dependencies = TRUE)
}

library(shiny)
library(shinydashboard)
library(leaflet)
library(shinycssloaders)
library(leaflet.extras)

library(dplyr)
library(plyr)
library(ggplot2)
library(plotly)
library(tidyquant)
library(xts)
###############################################################################
## Read the data from the diffrent Instrument/Satellites
# MODIS C6/Terra-Aqua
df_modis <- read.csv("./www/fire_archive_M6_155743.csv")

MODIS_C6<- df_modis %>%
  select(longitude, latitude,acq_date,frp,brightness) %>%
  mutate(acq_Date = as.Date(acq_date),
         acq_month = months(as.POSIXlt(acq_Date)),
         acq_day  = day(as.POSIXlt(acq_Date)),
         brightness= brightness - 273.15) %>% 
  filter(brightness>0)

# VIIRS/S-NPP
df1_VIIRS <- read.csv("./www/fire_archive_V1_155745.csv")

VIIRS_SNPP<- df1_VIIRS %>%
  select(longitude, latitude,acq_date,frp,bright_ti4) %>%
  mutate(acq_Date = as.Date(acq_date),
         acq_month = months(as.POSIXlt(acq_Date)),
         acq_day  = day(as.POSIXlt(acq_Date)),
         brightness= bright_ti4 - 273.15) %>% 
  filter(bright_ti4>0)

# VIIRS/NOAA 20
df2_VIIRS <- read.csv("./www/fire_nrt_J1V-C2_155744.csv")

VIIRS_NOAA_20<- df2_VIIRS%>%
  select(longitude, latitude,acq_date,frp,brightness) %>%
  mutate(acq_Date = as.Date(acq_date),
         acq_month = months(as.POSIXlt(acq_Date)),
         acq_day  = day(as.POSIXlt(acq_Date)),
         brightness= brightness - 273.15) %>% 
  filter(brightness>0)

## Important note:> The data provided by VIIRS_NOAA_20 didn't recorded the data from Sept. to Dec. of 2019

### II. Functions to VIZ the Fire Radiation Power 'FRP' and The brightness Temperature 'BT' from different satellites
#  create a color Bins for FRP & BT
bins_FRP <- c(0,5, 10, 20,30,40,50,60,Inf)
bins_BT <- c(0,10, 20, 30,40,50,60,70,80,Inf)

# Create a col pal. for each variable: FRP & BT
pal_FRP <- colorBin("Reds", domain =MODIS_C6$frp , bins = bins_FRP)
pal_BT <- colorBin("YlOrRd", domain =MODIS_C6$brightness , bins = bins_BT)

# Create  a label function 
label_FRP <- function(Sat){
  sprintf(
    "<strong>%s</strong><br/>%g (MW)",
    'FRP',Sat$frp
  ) %>% 
    lapply(htmltools::HTML)
}

label_BT <- function(Sat){
  sprintf(
    "<strong>%s</strong><br/>%g (c\u00B0)",
    'BT',Sat$brightness
  ) %>% 
    lapply(htmltools::HTML)
}

# Data_VIZ function to minimize the code while using different Satellites data
Data_VIZ<-function(Sat,Var,pal,label){
  VIZ_1<-leaflet(Sat) %>%
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DM") %>%
    addCircleMarkers(lng = ~longitude,
                     lat = ~latitude,
                     col = ~pal(Var),
                     opacity = 0.9,
                     label = label,#~as.character(paste0("FRP(MW): ", sep = " ", frp)), 
                     radius = 1, 
                     fillOpacity = 0.5,
                     labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"),
                                                 textsize = "15px",direction = "auto")) %>% 
    setView( 134.22436681269832, -27.031126703266906, 3.5 ) %>% 
    addMiniMap(position = "bottomleft", width = 120, height = 120) %>% # Layers control
    addLayersControl(
      baseGroups = c("CartoDB.DM","OSM (default)"),
      options = layersControlOptions(collapsed = FALSE))
  
  if (Var == Sat$frp){
    return(VIZ_1 %>% 
             leaflet::addLegend(pal = pal, values = ~Var, position = "bottomright",title = 'FRP (MW)'))
  }
  else (
    return(VIZ_1 %>% 
             leaflet::addLegend(pal = pal, values = ~Var, position = "bottomright",title = 'BT (c\u00B0)')
    )
  )
}

### Function for VIZ Heat map according to the Instrument/satellite
Data_HM<-function(Sat){
  leaflet(Sat) %>%
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DM") %>% 
    addHeatmap(lng = ~longitude, 
               lat = ~latitude, 
               intensity = ~brightness,
               blur = 16, max = 0.05, radius = 10,
               minOpacity = 0.05)%>% 
    setView( 134.22436681269832, -27.031126703266906, 3.5 ) %>% 
    addMiniMap(position = "bottomleft", width = 120, height = 120) %>% 
    addLayersControl(
      baseGroups = c("CartoDB.DM","OSM (default)"),
      options = layersControlOptions(collapsed = FALSE))
}

### Create a Function to arrange the data and to VIZ Time-Series Calendar Heatmap: of the The daily The daily Mean FRP and the daily maen BT
# Create a Function to arrange the data 
Sat_TS<-function(Sat){
  Sat%>% 
    dplyr::group_by(acq_Date) %>% 
    dplyr::summarise(Mean_FRP = mean(frp),
                     mean_BT = mean(brightness),
                     across(c(mean_BT, Mean_FRP), ~ round(., 3)),
                     .groups = 'drop')%>% 
    mutate(
      # new$weekday = as.POSIXlt(new$date_form)$wday #finding the day no. of the week
      weekday = as.POSIXlt(acq_Date)$wday,
      # converting the day no. to factor
      Day = factor(weekday,levels=rev(0:6),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE),
      # finding the month
      Month = factor(month(acq_Date),levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE),
      # finding the year and the month from the date. 
      yearmonth = factor(as.yearmon(acq_Date)),
      # finding the week of the year for each date
      week = as.numeric(format(acq_Date,"%W"))
    ) %>% 
    plyr::ddply(.(yearmonth),transform,Week_Month=1+week-min(week))
}

# Call the different Satellites data
MODIS_C6_TS <- Sat_TS(MODIS_C6)

VIIRS_SNPP_TS<-Sat_TS(VIIRS_SNPP)

VIIRS_NOAA_20_TS <- Sat_TS(VIIRS_NOAA_20) 

# Function to VIZ Time-Series Calendar Heatmap: 
viz_TS <- function(Sat_VIZ, Var){
  
  VIZ_2 <- ggplot(Sat_VIZ, aes(Week_Month, Day, fill = Var)) + 
    geom_tile(colour = "white") + facet_grid(year(Sat_VIZ$acq_Date)~Month) +
    labs(x="Week of Month", y = "")+
    theme(plot.title = element_text(face="bold", size = 16),
          axis.title.x = element_text(size=14),
          legend.position="bottom")
  
  if (Var == Sat_VIZ$Mean_FRP){
    return(VIZ_2+
             scale_fill_gradient(low="#FFFFFF", high="#FF0000")+
             ggtitle("Time Series Calendar Heatmap: The daily Mean Fire Radiative Power (MW)")+
             labs(fill = "FPR (MW)"))
  }
  else (
    return(VIZ_2+
             scale_fill_gradient(low="#FFFF80", high="#FF0000")+
             ggtitle("Time Series Calendar Heatmap: The daily Mean Brightness Temperature (c\u00B0)")+
             labs(fill = " BT (c\u00B0)"))
  )
}

# Function to VIZ Time Series variation of the varibales with a scatter plot foramts 
viz_SP<- function(Sat_sp, Var_sp){
  
  VIZ_3 <- ggplot(Sat_sp,aes(acq_Date, Var_sp))+
    scale_x_date(date_labels = "%m-%Y",date_minor_breaks = "1 month")+
    xlab("Date")+
    theme(plot.title = element_text(face="bold", size = 16, hjust = 0.5),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12))+
    expand_limits(y = 0)
  
  if (Var_sp == Sat_sp$Mean_FRP){
    return(VIZ_3+
             geom_point(colour="#ef3b2c")+
             geom_line(colour="#ef3b2c")+
             ggtitle("Time Series of The daily Mean Fire Radiative Power (MW)")+
             ylab("Mean Fire Radiative Power (MW)"))
  }
  else (
    return(VIZ_3+
             geom_point(colour="#fd8d3c")+
             geom_line(colour="#fd8d3c")+
             ggtitle("Time Series of The daily Mean Brightness Temperature (c\u00B0)")+
             ylab("Mean Brightness Temperature (c\u00B0)"))
  )
}

############################  User Interface 
ui <-  
  dashboardPage(
    dashboardHeader(title = "Australian Bushfire (Sep. 2019 - Mar. 2020)",
                    disable = FALSE, 
                    titleWidth  = 480),
    dashboardSidebar(tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"),
                     width = 300,
                     sidebarMenu(id="tabs",
                                 menuItem("Tracking The Bushfires", tabName="Maps", icon=icon("map-marked-alt")),
                                 menuItem("Time Series Analysis", tabName = "TSA",icon = icon("line-chart")),
                                 menuItem("Call For Action", tabName = "wwf", icon = icon("fire")),
                                 menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
                                 menuItem("About", tabName = "about", icon = icon("question"))
                     )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Maps",
                fluidPage(
                  fluidRow(
                    box(title = "Settings",solidHeader = TRUE,collapsible = TRUE,status = "primary",background = "black",width =4,height = 600,
                        selectInput(inputId = "selected_Month",
                                    label = "Select Month:",
                                    choices = unique(MODIS_C6$acq_month)),
                        sliderInput(inputId = "selected_Day", "Select Day:",
                                    min = 1, max =31, value = 1),
                        radioButtons(inputId = "SAT", label = "Select Instrument/Satellite",
                                     c("MODIS C6/Terra-Aqua"= "MODIS",
                                       "VIIRS/S-NPP"= "VIIRS_SNPP",
                                       "VIIRS/NOAA 20"= "VIIRS_NOAA")),
                        radioButtons(inputId = "VAR", label = "Select Variable",
                                     c("Fire Radiative Power" = "FRP_1",
                                       "Brightness Temperature" = "BT_1",
                                       "Heat Map" = "HM"))
                    ),
                    box(title = "Interactive Map", solidHeader = TRUE, collapsible = TRUE,status = "warning",background = "black",width =8,height = 600,
                        leafletOutput(outputId = "IM",height = "540px") %>% 
                          withSpinner(color="#ffba00")
                    )
                  )
                )
        ),
        tabItem(tabName = "TSA",
                fluidPage(
                  fluidRow(
                    box( title = "Settings",solidHeader = TRUE,collapsible = TRUE,status = "primary",background = "black",width =4,height = 600,
                         radioButtons(inputId = "inst_sat", label = "Select Instrument/Satellite",
                                      c("MODIS C6/Terra-Aqua"= "MODIS",
                                        "VIIRS/S-NPP"= "VIIRS_SNPP",
                                        "VIIRS/NOAA 20"= "VIIRS_NOAA")),
                         radioButtons(inputId = "Variable", label = "Select Variable",
                                      c("Fire Radiative Power" = "FRP_2",
                                        "Brightness Temperature" = "BT_2")),
                         radioButtons(inputId = "plot", label = "Select Plot Format",
                                      c("Calendar Heat Map" = "CHM"
                                        ,"Scatter Plot" = "SP"))
                    ),
                    box(title = "Plot",status = "warning",solidHeader = TRUE,collapsible = TRUE,background = "black",width =8,height = 600,
                        plotOutput(outputId="TSA_1",height="400px") %>% 
                          withSpinner(color="#ffba00"))
                  )
                ) 
        ),
        tabItem(tabName = "readme",
                fluidPage(
                  tags$iframe(src = './readme.html', 
                              width = '100%', height = '800px',
                              frameborder = 0, scrolling = 'auto'
                  )
                )
        ),
        tabItem(tabName = "about",
                fluidPage(
                  tags$iframe(src = './about.html', 
                              width = '100%', height = '800px',
                              frameborder = 0, scrolling = 'auto')
                  )
                ),
        tabItem(tabName = "wwf",
                HTML('<iframe width="1100" height="600" src="https://www.youtube.com/embed/Uq9bcIvdYNk" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        )
        )
      )
    )
############################ Server Interface 
server <- function(input, output) {
  
  # suppress warnings  
  storeWarn<- getOption("warn")
  options(warn = -1)
  
  output$IM <- renderLeaflet({
    
    # Function to choose one of the  Inst./inst_sat. data frame since they share the same periode of time 
    inst_sat_option <- function(inst_inst_sat){
      inst_inst_sat %>%
        filter(acq_month ==input$selected_Month & acq_day == input$selected_Day)
    }
    
    Modis_c6 <- inst_sat_option(MODIS_C6)
    Viirs_SNPP <- inst_sat_option(VIIRS_SNPP)
    Viirs_NOAA <- inst_sat_option(VIIRS_NOAA_20)
    
    # Funciotn to VIZ the data according to the input
    if(input$VAR == "FRP_1" & input$SAT == "MODIS"){
      Data_VIZ(Modis_c6 , Modis_c6$frp, pal_FRP, label_FRP(Modis_c6))
    }
    else if (input$VAR == "FRP_1" & input$SAT == "VIIRS_SNPP"){
      Data_VIZ(Viirs_SNPP , Viirs_SNPP$frp, pal_FRP, label_FRP(Viirs_SNPP))
    }
    
    else if (input$VAR == "FRP_1" & input$SAT == "VIIRS_NOAA"){
      Data_VIZ(Viirs_NOAA , Viirs_NOAA$frp, pal_FRP, label_FRP(Viirs_NOAA))
    }
    #
    else if (input$VAR == "BT_1" & input$SAT == "MODIS"){
      Data_VIZ(Modis_c6 , Modis_c6$brightness, pal_BT, label_BT(Modis_c6))
    }
    else if (input$VAR == "BT_1" & input$SAT == "VIIRS_SNPP"){
      Data_VIZ(Viirs_SNPP , Viirs_SNPP$brightness, pal_BT, label_BT(Viirs_SNPP))
    }
    else if (input$VAR == "BT_1" & input$SAT == "VIIRS_NOAA"){
      Data_VIZ(Viirs_NOAA , Viirs_NOAA$brightness, pal_BT, label_BT(Viirs_NOAA))
    }
    #
    else if (input$VAR == "HM" & input$SAT == "MODIS"){
      Data_HM(Modis_c6)
    }
    else if (input$VAR == "HM" & input$SAT == "VIIRS_SNPP"){
      Data_HM(Viirs_SNPP)
    }
    else if (input$VAR == "HM" & input$SAT == "VIIRS_NOAA"){
      Data_HM(Viirs_NOAA)
    }
  })
  
  output$TSA_1 <- renderPlot({
    # Time-Series Analysis with a Calendar Heatmap format 
    if (input$Variable == "FRP_2" & input$inst_sat == "MODIS" & input$plot == "CHM"){
      viz_TS(MODIS_C6_TS,MODIS_C6_TS$Mean_FRP)
    }
    else if (input$Variable == "FRP_2" & input$inst_sat == "VIIRS_SNPP" & input$plot == "CHM"){
      viz_TS(VIIRS_SNPP_TS,VIIRS_SNPP_TS$Mean_FRP)    
    }
    else if (input$Variable == "FRP_2" & input$inst_sat == "VIIRS_NOAA" & input$plot == "CHM"){
      viz_TS(VIIRS_NOAA_20_TS, VIIRS_NOAA_20_TS$Mean_FRP)
    }
    #
    else if (input$Variable == "BT_2" & input$inst_sat == "MODIS" & input$plot == "CHM"){
      viz_TS(MODIS_C6_TS, MODIS_C6_TS$mean_BT)
    }
    else if (input$Variable == "BT_2" & input$inst_sat == "VIIRS_SNPP" & input$plot == "CHM"){
      viz_TS(VIIRS_SNPP_TS, VIIRS_SNPP_TS$mean_BT)    
    }
    else if (input$Variable == "BT_2" & input$inst_sat == "VIIRS_NOAA" & input$plot == "CHM"){
      viz_TS(VIIRS_NOAA_20_TS, VIIRS_NOAA_20_TS$mean_BT)
    }
    # Time-Series Analysis with a Scatter plot format 
    else if (input$Variable == "FRP_2" & input$inst_sat == "MODIS" & input$plot == "SP"){
      viz_SP(MODIS_C6_TS,MODIS_C6_TS$Mean_FRP)
    }
    else if (input$Variable == "FRP_2" & input$inst_sat == "VIIRS_SNPP" & input$plot == "SP"){
      viz_SP(VIIRS_SNPP_TS,VIIRS_SNPP_TS$Mean_FRP)    
    }
    else if (input$Variable == "FRP_2" & input$inst_sat == "VIIRS_NOAA" & input$plot == "SP"){
      viz_SP(VIIRS_NOAA_20_TS, VIIRS_NOAA_20_TS$Mean_FRP)
    }
    #
    else if (input$Variable == "BT_2" & input$inst_sat == "MODIS" & input$plot == "SP"){
      viz_SP(MODIS_C6_TS, MODIS_C6_TS$mean_BT)
    }
    else if (input$Variable == "BT_2" & input$inst_sat == "VIIRS_SNPP" & input$plot == "SP"){
      viz_SP(VIIRS_SNPP_TS, VIIRS_SNPP_TS$mean_BT)    
    }
    else if (input$Variable == "BT_2" & input$inst_sat == "VIIRS_NOAA" & input$plot == "SP"){
      viz_SP(VIIRS_NOAA_20_TS, VIIRS_NOAA_20_TS$mean_BT)
    }
  }, height=540)
  
  
}

shinyApp(ui = ui, server = server)
