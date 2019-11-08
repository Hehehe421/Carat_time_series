#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(plotly)
library(data.table)
library(forecast)
library(zoo)
library(tidyverse)
library(tseries)
library(lubridate)
library(anytime)
library(gridExtra)
library(prophet)

#source('C:/Users/hes0/Documents/Sixuan/Projects/2019Q3/Carat_time_series/R/plot_functions.R')
source('R/plot_functions.R')
#source('C:/Users/hes0/Documents/Sixuan/Projects/2019Q3/Carat_time_series/R/plot_functions.R')
#df_day = readRDS('C:/Users/hes0/Documents/Sixuan/Projects/2019Q3/Carat_time_series/df_day.rds')
df_day = readRDS('data/df_day.rds')
#total_activity = readRDS('C:/Users/hes0/Documents/Sixuan/Projects/2019Q3/Carat_time_series/total_activity.rds')
#df_category = readRDS('df_category.rds')
#df_category = readRDS('C:/Users/hes0/Documents/Sixuan/Projects/2019Q3/Carat_time_series/df_category.rds')
df_category = readRDS('data/df_category.rds')
category_choices = unique(df_day$CATEGORY)
#category_choices <- append(category, "Select All", after = 0)
category_selected <- c('OVERALL')

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))

KPI_choices = c('Total_activity','Total_users', 'Average_activity', 
                'Average_activity_ratio', 'Percentage_activity', 'Percentage_users')

KPI_choices2 = c('Total_activity','Total_users', 'Average_activity')

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Time Series Dashboard",
                  titleWidth = 450),
  
  dashboardSidebar(sidebarMenu(
    menuItem("KPI Overview", tabName = "dashboard2", icon = icon("dashboard")),
    menuItem("Stationary", tabName = "dashboard3", icon = icon("th")),
    menuItem("ARIMA Model", tabName = "dashboard4", icon = icon("th")),
    menuItem("ARIMA-GARCH Model", tabName = "dashboard6", icon = icon("th")),
    menuItem("Prophet Model", tabName = 'dashboard5', icon = icon("th"))#,
    #menuItem('Test', tabName = 'test', icon = icon("th"))
    ),
    dateRangeInput("daterange1", "Date range:",
                   start = "2016-05-01",
                   end   = "2018-06-30")
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dashboard2',
              fluidRow(tweaks,
                column(width = 7,
                       box(width = 12,height = 225,
                           title = "KPI Selection", status = "primary", solidHeader = TRUE,
                           radioButtons(inputId="KPI_input", label="What KPI would you like to see?", choices=KPI_choices)
                           
                       ),
                       #box(width = 12,height = 225,
                       #    title = "Key Performance Indicator Definition", status = "primary", solidHeader = TRUE,
                       #    uiOutput("image")
                       #),
                       box(width = 12,
                           title = "Key Performance Indicator Trend by Date and Category", status = "primary", solidHeader = TRUE,
                           #collapsible = TRUE,
                           plotlyOutput("KPI_plot1", height = 600)
                       )
                ),
                column(width = 5,
                       #box(width = 12,height = 225,
                       #    title = "KPI Selection", status = "warning", solidHeader = TRUE,
                       #    radioButtons(inputId="KPI_input", label="What KPI would you like to see?", choices=KPI_choices)
                           
                       #),
                       box(width = 12,height = 225,
                           title = "App Usage Category", status = "warning", solidHeader = TRUE,
                           tags$div(align = 'left', 
                                    class = 'multicol', 
                                    checkboxGroupInput("category_input","Check below:",category_choices, selected = category_selected))
                       ),
                       box(width = 12,height = 660,
                           title = "Category Chart by Volume", status = "warning", solidHeader = TRUE,
                           #collapsible = TRUE,
                           plotOutput("Category_plot1", height = 550)
                       )
                       
                )
              )
      ),
      tabItem(tabName = 'dashboard3',
              fluidRow(tweaks,
                       column(width = 8,
                              box(width = 12,height = 600,
                                  title = "Total activity trend and first order difference trend", status = "primary", solidHeader = TRUE,
                                  plotOutput('ts_activity_plot1', height = 525)
                              ),
                              box(width = 12,
                                  title = "ACF and PACF Plot", status = "primary", solidHeader = TRUE,
                                  #collapsible = TRUE,
                                  #plotlyOutput("KPI_plot1", height = 600)
                                  plotOutput('ts_acf_pacf_plot1', height = 225)
                              )
                       ),
                       column(width = 4,
                              box(width = 12,height = 150,
                                  title = "KPI Selection", status = "warning", solidHeader = TRUE,
                                  radioButtons(inputId="KPI_input2", label="What KPI would you like to see?", choices=KPI_choices2)
                                  
                              ),
                              box(width = 12,height = 220,
                                  title = "App Usage Category", status = "warning", solidHeader = TRUE,
                                  tags$div(align = 'left', 
                                           class = 'multicol', 
                                           radioButtons("category_input2","Check below:",category_choices, selected = category_selected))
                              ),
                              box(width = 12,height = 500,
                                  title = "Stationary Test", status = "warning", solidHeader = TRUE,
                                  verbatimTextOutput('test_print1'),
                                  verbatimTextOutput('test_print2')
                                  #plotOutput('decompose_plot1')
                                  )
                              
                       )
              )
        
      ),

      tabItem(tabName = 'dashboard5',
              fluidRow(tweaks,
                       column(width = 6,
                              box(width = 12,height = 650,
                                  title = "Prophet Forecast 10 forward", status = "primary", solidHeader = TRUE,
                                  plotOutput('ts_forecast_plot2', height = 550)
                              )
                              
                       ),
                       column(width = 6,
                              
                              box(width = 12,height = 650,
                                  title = "Prophet Model Component Plot", status = "primary", solidHeader = TRUE,
                                  #collapsible = TRUE,
                                  #plotlyOutput("KPI_plot1", height = 600)
                                  plotOutput('prophet_com_plot1', height = 550)
                              )
                       )
               ),
              fluidRow(
                box(width = 12,height = 250,
                    title = "Data Scource", status = "success", solidHeader = TRUE,
                    HTML(
                      paste(h3('Carat Top 1000 Users Long-Term App Usage Dataset'),
                    h4('Available online: https://www.cs.helsinki.fi/group/carat/data-sharing/'),
                    h4(
                  'A. J. Oliner, A. P. Iyer, I. Stoica, E. Lagerspetz, and S. Tarkoma.
                Carat: Collaborative Energy Diagnosis for Mobile Devices.
                In Proceedings of the 11th ACM Conference on Embedded Networked Sensor Systems, (10 pages), 2013, ACM.'),
                  h4('http://www.cs.helsinki.fi/group/carat/pubs/caratSensys13.pdf')
                    )
              )))
              
      ),
      tabItem(tabName = 'dashboard4',
              fluidRow(tweaks,
                       column(width = 8,
                              box(width = 12,height = 350,
                                  title = "ARIMA Forecast 10 forward", status = "primary", solidHeader = TRUE,
                                  plotOutput('ts_forecast_plot1', height = 280)
                              ),
                              box(width = 12,
                                  title = "ARIMA Model Residual Diagnostics", status = "primary", solidHeader = TRUE,
                                  #collapsible = TRUE,
                                  #plotlyOutput("KPI_plot1", height = 600)
                                  plotOutput('ts_diag_plot1', height = 500)
                              )
                       ),
                       column(width = 4,
                              box(width = 12,height = 130,
                                  title = "ARIMA Methods", status = "warning", solidHeader = TRUE,
                                  selectInput("forecast", "Choose Forecast Method:",
                                              c("AUTO ARIMA" = "auto_arima",
                                                "ARIMA(0,1,0)" = "fit_arima010",
                                                "ARIMA(0,1,1)" = "fit_arima011",
                                                "ARIMA(1,1,1)" = "fit_arima111",
                                                "ARIMA(1,1,0)" = "fit_arima110",
                                                "ARIMA(0,1,2)" = "fit_arima012",
                                                "ARIMA(1,1,2)" = "fit_arima112",
                                                "ARIMA(2,1,0)" = "fit_arima210",
                                                "ARIMA(2,1,2)" = "fit_arima212",
                                                "ARIMA(2,1,1)" = "fit_arima211"
                                              ))
                              ),
                              box(width = 12,height = 280,
                                  title = "Summary Models", status = "warning", solidHeader = TRUE,
                                  verbatimTextOutput('summary_print1')#,
                                  #DT::dataTableOutput("accuracy_table1")
                                  ),
                              box(width = 12, height = 480,
                                  title = 'Accuracy Table (Test 15 forward step)', status = "warning", solidHeader = TRUE,
                                  
                                  DT::dataTableOutput("accuracy_table1")
                                  )
                              
                       )
              )
              
      ),
      tabItem(tabName = 'dashboard6',
              fluidRow(tweaks,
                       column(width = 8,
                              box(width = 12,height = 180,
                                  title = "Box-Ljung Test on ARIMA Residuals (Lag = 20)", status = "primary", solidHeader = TRUE,
                                  verbatimTextOutput('summary_print3')
                              ),
                              box(width = 12,height = 700,
                                  title = "Squared Residuals Analysis for ARIMA", status = "primary", solidHeader = TRUE,
                                  #collapsible = TRUE,
                                  #plotlyOutput("KPI_plot1", height = 600)
                                  plotOutput('squared_residuals_ts', height = 200),
                                  plotOutput('squared_residuals_acfpacf', height = 400)
                          
                              )
                       ),
                       column(width = 4,
                              
                              box(width = 12,height = 180,
                                  title = "GARCH Methods", status = "warning", solidHeader = TRUE,
                                  selectInput("forecast_garch", "Choose GARCH Method:",
                                              c("GARCH(0,1)" = "fit_garch01",
                                                "GARCH(0,2)" = "fit_garch02",
                                                "GARCH(0,3)" = "fit_garch03",
                                                "GARCH(0,4)" = "fit_garch04",
                                                "GARCH(0,5)" = "fit_garch05",
                                                "GARCH(1,1)" = "fit_garch06"
                                              ))
                              ),
                              box(
                                width = 12,height = 700,
                                title = "Summary Models", status = "warning", solidHeader = TRUE,
                                verbatimTextOutput('summary_print4')
                              )
                              
                       )
              )
              
      )
    )
    )
    
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    my_data <- df_day
    my_data <- my_data[(my_data$Date>=input$daterange1[1]) & (my_data$Date<=input$daterange1[2]), ]
    #my_data <- my_data[(my_data$CAT_NEW %in% category_input), ]
    #if (input$category_input != "Select All") { 
    #  my_data <- my_data[(my_data$CAT_NEW %in% c(input$category_input)), ]
    #} 
    my_data <- my_data[(my_data$CATEGORY%in% c(input$category_input)), ]
    my_data
  })
  
  filtered_data2 <- reactive({
    my_data <- df_day
    if (input$KPI_input2 == "Total_activity") { 
      my_data <- my_data[(my_data$Date>=input$daterange1[1]) & (my_data$Date<=input$daterange1[2]), ]
      my_data <- my_data[(my_data$CATEGORY%in% c(input$category_input2)), ]
      my_data <- my_data[, .(Date, Total_activity)][order(Date)]
      days = data.table(Date = seq(as.Date(min(my_data$Date)),as.Date(max(my_data$Date)),by = 1))
      
      #Missing date checking and forward fill
      
      my_data = my_data[days, on = 'Date']
      
      my_data = na.locf(na.locf(my_data), fromLast = TRUE)
      ts_selected <- ts(my_data[,.(Total_activity)], start = decimal_date(as.Date(min(my_data$Date))), frequency = 365)
    } 
    else if (input$KPI_input2 == "Total_users") { 
      my_data <- df_day
      my_data <- my_data[(my_data$Date>=input$daterange1[1]) & (my_data$Date<=input$daterange1[2]), ]
      my_data <- my_data[(my_data$CATEGORY%in% c(input$category_input2)), ]
      my_data <- my_data[, .(Date, Total_users)][order(Date)]
      days = data.table(Date = seq(as.Date(min(my_data$Date)),as.Date(max(my_data$Date)),by = 1))
      
      #Missing date checking and forward fill
      
      my_data = my_data[days, on = 'Date']
      #[, Mflag := ifelse(is.na(input$KPI_input), 1,0)]
      
      my_data = na.locf(na.locf(my_data), fromLast = TRUE)
      ts_selected <- ts(my_data[,.(Total_users)], start = decimal_date(as.Date(min(my_data$Date))), frequency = 365)
      ts_selected
    } 
    else {
    my_data <- my_data[(my_data$Date>=input$daterange1[1]) & (my_data$Date<=input$daterange1[2]), ]
    my_data <- my_data[(my_data$CATEGORY%in% c(input$category_input2)), ]
    my_data <- my_data[, .(Date, Average_activity)][order(Date)]
    days = data.table(Date = seq(as.Date(min(my_data$Date)),as.Date(max(my_data$Date)),by = 1))
    
    #Missing date checking and forward fill
    
    my_data = my_data[days, on = 'Date']
    #[, Mflag := ifelse(is.na(input$KPI_input), 1,0)]
    
    my_data = na.locf(na.locf(my_data), fromLast = TRUE)
    ts_selected <- ts(my_data[,.(Average_activity)], start = decimal_date(as.Date(min(my_data$Date))), frequency = 365)
    ts_selected}
    ts_selected
  })
  
  filtered_data3 <- reactive({
    my_data <- df_day
    if (input$KPI_input2 == "Total_activity") { 
      my_data <- my_data[(my_data$Date>input$daterange1[2]), ]
      my_data <- my_data[(my_data$CATEGORY%in% c(input$category_input2)), ]
      my_data <- my_data[, .(Date, Total_activity)][order(Date)]
      days = data.table(Date = seq(as.Date(min(my_data$Date)),length = 15,by = 1))
      
      #Missing date checking and forward fill
      
      my_data = my_data[days, on = 'Date']
      
      my_data = na.locf(na.locf(my_data), fromLast = TRUE)
      ts_test <- ts(my_data[,.(Total_activity)], start = decimal_date(as.Date(min(my_data$Date))), frequency = 365)
      ts_test
    } 
    else if (input$KPI_input2 == "Total_users") { 
      my_data <- my_data[(my_data$Date>input$daterange1[2]), ]
      my_data <- my_data[(my_data$CATEGORY%in% c(input$category_input2)), ]
      my_data <- my_data[, .(Date, Total_users)][order(Date)]
      days = data.table(Date = seq(as.Date(min(my_data$Date)),length = 15,by = 1))
      
      #Missing date checking and forward fill
      
      my_data = my_data[days, on = 'Date']
      
      my_data = na.locf(na.locf(my_data), fromLast = TRUE)
      ts_test <- ts(my_data[,.(Total_users)], start = decimal_date(as.Date(min(my_data$Date))), frequency = 365)
      ts_test
    } 
    else {
      my_data <- my_data[(my_data$Date>input$daterange1[2]), ]
      my_data <- my_data[(my_data$CATEGORY%in% c(input$category_input2)), ]
      my_data <- my_data[, .(Date, Average_activity)][order(Date)]
      days = data.table(Date = seq(as.Date(min(my_data$Date)),length = 15,by = 1))
      #Missing date checking and forward fill
      
      my_data = my_data[days, on = 'Date']
      #[, Mflag := ifelse(is.na(input$KPI_input), 1,0)]
      
      my_data = na.locf(na.locf(my_data), fromLast = TRUE)
      ts_test <- ts(my_data[,.(Average_activity)], start = decimal_date(as.Date(min(my_data$Date))), frequency = 365)
      ts_test}
    ts_test
  })
  
  fit_arima <- reactive({
    log_ts = log(filtered_data2())
    if (input$forecast == "auto_arima") {
      fit = auto.arima(log(filtered_data2()))
    } else if (input$forecast == "fit_arima010") {
      fit = arima(log(filtered_data2()), order = c(0,1,0))
    } else if (input$forecast == "fit_arima011") {
      fit = arima(log(filtered_data2()), order = c(0,1,1))
    } else if (input$forecast == "fit_arima111") {
      fit = arima(log(filtered_data2()), order = c(1,1,1))
    } else if (input$forecast == "fit_arima110") {
      fit = arima(log(filtered_data2()), order = c(1,1,0))
    } else if (input$forecast == "fit_arima012") {
      fit = arima(log(filtered_data2()), order = c(0,1,2))
    } else if (input$forecast == "fit_arima112") {
      fit = arima(log(filtered_data2()), order = c(1,1,2))
    } else if (input$forecast == "fit_arima210") {
      fit = arima(log(filtered_data2()), order = c(2,1,0))
    } else if (input$forecast == "fit_arima212") {
      fit = arima(log(filtered_data2()), order = c(2,1,2))
    } else if (input$forecast == "fit_arima211") {
      fit = arima(log(filtered_data2()), order = c(2,1,1))
    }
    fit
  })
  
  fit_garch <- reactive({
    res.fit_arima = fit_arima()$res
    if (input$forecast_garch == "fit_garch01") {
      fit =garch(res.fit_arima,order=c(0,1),trace=F)
    } else if (input$forecast_garch == "fit_garch02") {
      fit =garch(res.fit_arima,order=c(0,2),trace=F)
    } else if (input$forecast_garch == "fit_garch03") {
      fit =garch(res.fit_arima,order=c(0,3),trace=F)
    } else if (input$forecast_garch == "fit_garch04") {
      fit =garch(res.fit_arima,order=c(0,4),trace=F)
    } else if (input$forecast_garch == "fit_garch05") {
      fit =garch(res.fit_arima,order=c(0,5),trace=F)
    } else if (input$forecast_garch == "fit_garch06") {
      fit =garch(res.fit_arima,order=c(1,1),trace=F)
    } 
    fit
  })
  
  ts_prophet <- reactive({
    my_data <- df_day
    if (input$KPI_input2 == "Total_activity") { 
      my_data <- my_data[(my_data$Date>=input$daterange1[1]) & (my_data$Date<=input$daterange1[2]), ]
      my_data <- my_data[(my_data$CATEGORY%in% c(input$category_input2)), ]
      my_data <- my_data[, .(Date, Total_activity)][order(Date)]

      setnames(my_data, c('Date', 'Total_activity'), c('ds', 'y'))
    } 
    else if (input$KPI_input2 == "Total_users") { 
      my_data <- df_day
      my_data <- my_data[(my_data$Date>=input$daterange1[1]) & (my_data$Date<=input$daterange1[2]), ]
      my_data <- my_data[(my_data$CATEGORY%in% c(input$category_input2)), ]
      my_data <- my_data[, .(Date, Total_users)][order(Date)]

      setnames(my_data, c('Date', 'Total_users'), c('ds', 'y'))
      my_data
    } 
    else {
      my_data <- my_data[(my_data$Date>=input$daterange1[1]) & (my_data$Date<=input$daterange1[2]), ]
      my_data <- my_data[(my_data$CATEGORY%in% c(input$category_input2)), ]
      my_data <- my_data[, .(Date, Average_activity)][order(Date)]
      setnames(my_data, c('Date', 'Average_activity'), c('ds', 'y'))
      my_data}
    my_data

  })
  
  
  current_player = reactive({
    req(input$KPI_input)
  })
  c_url <- reactive({
    paste0("image_", current_player(), ".png")
  })
  output$image <- renderUI({
    tags$img(src = c_url(), height = 150, width = 800,align="center")
  })
  

  output$KPI_plot1 <- renderPlotly({
    p = plot_line(filtered_data(), "Date", input$KPI_input, "CATEGORY")
    ggplotly(p)%>%layout(legend = list(orientation = 'h',x = -0.0, y =-0.1))
  })
  
  output$Category_plot1 <- renderPlot({
    p = ggplot(data = df_category, aes(x = reorder(CATEGORY, -Total_activity), y = Total_activity, fill = CATEGORY)) 
    p + geom_bar(stat="identity") +theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      xlab("") +  ylab("") +
      theme(legend.position = "none") +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
  })
  
  output$ts_activity_plot1 <- renderPlot({
    
    ts_plot_day(filtered_data2())
  })
  
  output$ts_acf_pacf_plot1 <- renderPlot({
    
    plot_acf_pacf(filtered_data2(), 'selected series', 2)
  })
  
  output$test_print1 <- renderPrint({
    selected_series = filtered_data2()
    Box.test(selected_series, lag = 20)
    
  })
  
  output$test_print2 <- renderPrint({
    selected_series = filtered_data2()
    
    adf.test(selected_series)
  })
  
  output$ts_diag_plot1 <- renderPlot({
    #fit = auto.arima(filtered_data2())
    ggtsdiag_custom(fit_arima(), 'selected series')
  })
  
  output$summary_print1 <- renderPrint({
    fit = fit_arima()
    fit
  })

  output$ts_forecast_plot1 <- renderPlot({
    forecast_fit = forecast(fit_arima(), h = 15)
    ts_forecast = fortify(forecast_fit, ts.connect = TRUE)
    setDT(ts_forecast)
    setnames(ts_forecast, c("Point Forecast", "Lo 95", "Hi 95"), c("Point", "Lower", "Higher"))
    plot_ts_forecast(ts_forecast)
  })
  
  output$accuracy_table1 <- renderDataTable({
    forecast_fit = forecast(fit_arima(), h = 15)
    test_ts = log(filtered_data3())
    acc_arima <- round(accuracy(forecast_fit, test_ts), 4)
    t(acc_arima)
  })
  
  output$ts_forecast_plot2 <- renderPlot({
    m = prophet(ts_prophet())
    future = make_future_dataframe(m, periods = 15)
    forecast = predict(m, future)
    p = plot(m, forecast)
    #p + theme_bw()
    p
  })
  
  output$prophet_com_plot1 <- renderPlot({
    m = prophet(ts_prophet())
    future = make_future_dataframe(m, periods = 15)
    forecast = predict(m, future)
    prophet_plot_components(m, forecast)
  })
  
  output$summary_print3 <- renderPrint({
    res.fit_arima = fit_arima()$res
    Box.test(res.fit_arima, lag = 20, fitdf = 2)
  })
  
  output$squared_residuals_ts <- renderPlot({
    squared.res.fit_arima = (fit_arima()$res)^2
    plot_time_series(squared.res.fit_arima, 'Squared Residuals')
  })
  
  output$squared_residuals_acfpacf <- renderPlot({
    squared.res.fit_arima = (fit_arima()$res)^2
    plot_acf_pacf(squared.res.fit_arima, 'Squared Residuals', 1)
  })
  
  output$summary_print4 <- renderPrint({
    summary(fit_garch())
  })
  
  output$ts_forecast_plot_test <- renderPrint({
    fit = filtered_data2()
    fitarima = auto.arima(fit)
    fitarima
  })
#  output$decompose_plot1 <- renderPlot({
#    if (input$KPI_input2 == "Total_activity") { 
#      my_tsdata <- ts_total_activity()
#    } 
#    else if (input$KPI_input2 == "Total_users") { 
#      my_tsdata <- ts_total_users()
#    } 
#    else {my_tsdata <- ts_average_activity()}
#    de = decompose(my_tsdata)
#    plot(de)
#  })


}

shinyApp(ui, server)

