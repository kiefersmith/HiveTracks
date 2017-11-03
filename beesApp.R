setwd('~/Documents/Data Science/honeybees')
library(plotly)
library(shiny)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(readr)
library(jsonlite)
library(XML)
library(RColorBrewer)
library(flexdashboard)
library(shinydashboard)
library(leaflet)
#api_key = '7rkoz892zrnkxgbpvm2rzmb97dw2q18sk3rg4myqa2cf1dbvde4pvdpkk8ap1mle3amfh3w433m4ghhs6jizxrkomn'
#download.file('http://uoweb1.ncl.ac.uk/api/v1/sensors/data/raw.csv?start_time=20171001000000&end_time=20171102000000&sensor_type=Bee+Hive&api_key=7rkoz892zrnkxgbpvm2rzmb97dw2q18sk3rg4myqa2cf1dbvde4pvdpkk8ap1mle3amfh3w433m4ghhs6jizxrkomn',
#              destfile = 'tmp.csv')

#tmp <- read_csv('http://uoweb1.ncl.ac.uk/api/v1/sensors/data/raw.csv?start_time=20171001000000&end_time=20171103000000&sensor_type=Bee+Hive&api_key=7rkoz892zrnkxgbpvm2rzmb97dw2q18sk3rg4myqa2cf1dbvde4pvdpkk8ap1mle3amfh3w433m4ghhs6jizxrkomn')
#tmp <- read_csv('tmp.csv')
ui <- dashboardPage(
  dashboardHeader(title = "Newcastle Bees"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = 'home', icon = icon("home")),
      menuItem("Map", tabName = 'map', icon = icon('map')))),
  dashboardBody(
    tabItems( 
      tabItem(tabName = 'home',fluidPage(
        selectInput("hive_id", "Beehive:",
                    c("Beehive 54440" = "beehive_54440",
                      "Beehive 54460" = "beehive_54460",
                      "Beehive 54490" = "beehive_54490",
                      "Beehive 54510" = "beehive_54510",
                      "Beehive 9803" = "beehive_9803",
                      "Beehive 9837" = "beehive_9837",
                      "Beehive 9841" = "beehive_9841",
                      "Beehive 9848" = "beehive_9848",
                      "Beehive 54620" = "beehive_54620",
                      "Beehive 9847" = "beehive_9847",
                      "Beehive 54610" = "beehive_54610",
                      "Beehive 54630" = "beehive_54630")),
        tabBox(
          title = 'Hive Monitoring',
          id = 'tabset',
          width = 12,
          tabPanel('Hive Activity',plotlyOutput('plot1')),
          tabPanel('Hive Brood',plotlyOutput('plot2')),
          tabPanel('Weight',plotlyOutput('plot3')),
          tabPanel('Daily Activity',plotlyOutput('plot4'))))),
      tabItem(tabName = 'map', fluidPage(
        leafletOutput('hiveMap'))))))
      
server <- shinyServer(function(input, output, session){
  downloadData <- function() {
    data <- read_csv('tmp.csv')
    return(data)
  }
  
  cleanData <- function(df) {
    activity <- df %>%
      filter(Variable == 'Hive Activity') %>%
      group_by(Timestamp, name) %>%
      summarize(activity = mean(Value, rm.na=T), lat = mean(lat, rm.na = T), lon = mean(lon, rm.na = T))
    
    fanning <- df %>%
      filter(Variable == 'Mean Fanning') %>%
      group_by(Timestamp, name) %>%
      summarize(mean_fanning = mean(Value, rm.na=T))
    
    humidity <- df %>%
      filter(Variable == 'Humidity') %>%
      group_by(Timestamp, name) %>%
      summarize(humidity = mean(Value, rm.na=T))
    
    temp <- df %>%
      filter(Variable == 'Temperature') %>%
      group_by(Timestamp, name) %>%
      summarize(temperature = mean(Value, rm.na=T))
    
    brood <- df %>%
      filter(Variable == 'Brood') %>%
      group_by(Timestamp, name) %>%
      summarize(brood = mean(Value, rm.na=T))
    
    noise <- df %>%
      filter(Variable == 'Mean Flight Noise') %>%
      group_by(Timestamp, name) %>%
      summarize(flight_noise = mean(Value, rm.na=T))
    
    weight <- df %>%
      filter(Variable == 'Weight') %>%
      group_by(Timestamp, name) %>%
      summarize(weight = mean(Value, rm.na = T))
    
    tmp <- activity %>%
      left_join(weight)
    tmp <- tmp %>%
      left_join(brood, by = c("Timestamp", "name"))
    tmp <- tmp %>%
      left_join(fanning, by = c("Timestamp", "name"))
    tmp <- tmp %>%
      left_join(humidity, by = c("Timestamp", "name"))
    tmp <- tmp %>%
      left_join(noise, by = c("Timestamp", "name"))
    tmp <- tmp %>%
      left_join(temp, by = c("Timestamp", "name"))
    tmp$hour <- hour(tmp$Timestamp)
    tmp$date <- as.Date(tmp$Timestamp)
    
    df <- tmp
    return(df)
  }
  master <- cleanData(downloadData())
  
  byDay <- function(df) {
    day <- df %>% 
      group_by(date, name) %>%
      summarize(day_total = sum(activity))
    return(day)
  }
  cumulativeHour <- function(df) {
    temp <- df$activity
    for(i in 2:length(temp)) {
      temp[i] = temp[i] + temp[i-1]
    }
    df$cumulative <- temp
    return(df)
  }
  
  scaleToDay <- function(index) {
    df <- master
    df$date <- as.Date(df$Timestamp)
    tmp <- df %>%
      group_by(date, name) %>%
      summarize(max_value = max(activity, na.rm = T), min_value = min(activity, na.rm = T), abs_max = max(abs(activity)))
    joined <- df %>%
      left_join(tmp)
    scaled <- joined[[index]]
    scaled <- (joined[[index]] - joined$min_value)/(joined$max_value - joined$min_value)
    df[[ncol(df) + 1]] <- scaled
    names(df[[ncol(df)]]) <- 'scaledActivity'
    return (df)
  }
  scaleCumulative <- function(df) {
    df$date <- as.Date(df$Timestamp)
    tmp <- df %>%
      group_by(date, name) %>%
      summarize(max_value = max(cumulative, na.rm = T), min_value = min(cumulative, na.rm = T), abs_max = max(abs(cumulative)))
    joined <- df %>%
      left_join(tmp)
    scaled <- joined$cumulative
    scaled <- (joined$cumulative - joined$min_value)/(joined$max_value - joined$min_value)
    df[[ncol(df) + 1]] <- scaled
    names(df)[ncol(df)] <- 'scaledCumulative'
    return (df)
  }
  output$plot1 <- renderPlotly({
    sub <- master[master$name == input$hive_id,]
    dfs <- split(sub, interaction(sub$date), drop = T)
    p <- plot_ly() %>%
      layout(xaxis = list(title = 'Hour of Day'),
             yaxis = list(title = 'Hive Activity'), paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)') %>%
      layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) %>%
      config(displayModeBar = F, showLink = F)
    
    for (i in 1:length(dfs)) {
      sub = dfs[[i]]
      if(max(sub$hour) > 16 & min(sub$hour) < 8) {
        p <- p %>% add_trace(x = sub$hour, y = sub$activity, type = 'scatter',mode = 'lines',text = paste("Date: ", sub$date), name = sub$date)
      }
    }
    p
  })
  output$plot2 <- renderPlotly({
    sub <- master[master$name == input$hive_id,]
    p <- plot_ly(x = sub$Timestamp, y = as.numeric(scale(sub$brood)), type = 'scatter', mode = 'none', fill = 'tozeroy',
                 fillcolor = 'rgba(0, 235, 0, 0.5)', plot_bgcolor = 'rgba(0,0,0,0)') %>%
      layout(hovermode = 'compare', xaxis = list(title = 'Day'),
             yaxis = list(title = 'Brood'), paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)') %>%
      layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) %>%
      config(displayModeBar = F, showLink = F)
    p
  })
  output$plot3 <- renderPlotly({
    sub <- master[master$name == input$hive_id,]
    p <- plot_ly(x = sub$Timestamp, y = as.numeric(scale(sub$weight)), type = 'scatter', mode = 'none', fill = 'tozeroy',
                 fillcolor = 'rgba(0, 235, 0, 0.5)', plot_bgcolor = 'rgba(0,0,0,0)') %>%
      layout(hovermode = 'compare', xaxis = list(title = 'Day'),
             yaxis = list(title = 'Hive Weight'), paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)') %>%
      layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) %>%
      config(displayModeBar = F, showLink = F)
    p
  })
  output$plot4 <- renderPlotly({
    sub <- master[master$name == input$hive_id,]
    dfs <- split(sub, interaction(sub$date), drop = T)
    sub <- dfs[[1]]
    sub <- cumulativeHour(sub)
    sub <- scaleCumulative(sub)
    sub$age <- sub$date - min(sub$date)
    p <- plot_ly() %>%
      layout(xaxis = list(title = 'Hour of Day'),
             yaxis = list(title = 'Hive Activity'), paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)') %>%
      layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) %>%
      config(displayModeBar = F, showLink = F)
    
    for (i in 1:length(dfs)) {
      sub <- dfs[[i]]
      sub <- cumulativeHour(sub)
      sub <- scaleCumulative(sub)
      if(max(sub$hour) > 16 & min(sub$hour) < 8){
        p <- p %>% add_trace(x = sub$hour, y = sub$cumulative, type = 'scatter',mode = 'lines', text = paste("Date: ", sub$date), name = sub$date)
      }
    }
    p
  })
  output$hiveMap <- renderLeaflet({
    m <- leaflet(master) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(stroke=FALSE,fillOpacity = 1,label = ~name) %>% 
      
      addMiniMap()
    m
  })
})
shinyApp(server = server, ui = ui)

