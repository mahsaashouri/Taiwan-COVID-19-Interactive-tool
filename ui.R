

library(shiny)
library(shinyscreenshot)
library(shinydashboard)
library(shinythemes)


shinyUI(fluidPage( theme = shinytheme("cerulean"),
                   titlePanel("Taiwan COVID19 confirmed cases analysis"),
                   sidebarLayout(
                     sidebarPanel( 
                       fileInput('target_upload', 'Choose file to upload',
                                             accept = c(
                                               'text/csv',
                                               'text/comma-separated-values',
                                               '.csv'
                                             )),
                                   selectInput("Depth", label="MOB depth", choices=list("1-no"= "1","2" = "2", "3" = "3", "4" = "4", "5" = "5"), selected=list("3")),
                                   selectInput("Prune", label = "Prune options",
                                               choices = list("AIC" = "AIC", "BIC" = "BIC"), selected = list("AIC")),
                                   checkboxGroupInput("SplitVariables", label = "Split variables",
                                                      choices = list("population" = "population", "region" = "region",  "imported" = "imported",
                                                                     "administrative" = "administrative", "airport" = "airport"),
                                                      selected = list("population", "region",  "imported","administrative", "airport")),
                                   actionButton("go", "Take a screenshot"),
                                   width = 2
                     ),
                     mainPanel(
                       tabPanel("MSE", div(tableOutput("MSE"), value = "title"), style = "font-size:150%"),
                       tabPanel(h3("MOB - Heatmaps"), title = uiOutput("titleHeatmap1")),
                       tabPanel("MOB - Heatmaps", plotOutput("MOBTree1", width = 1300, height = 500)),
                       tabPanel(h3("MOB - Heatmaps (Day.of.week)"), title = uiOutput("titleHeatmap2")),
                       tabPanel("Heatmaps - Day.of.week", plotOutput("MOBTree2", width = 1300, height = 500)),
                       tabPanel(h3("Series with average line"), title = uiOutput("titleline")),
                       tabPanel("Series with average line", plotOutput("ClusterSeries")),
                       tabPanel(h3("Coefficient plot"), title = uiOutput("titlecoefficient")),
                       tabPanel("Coefficient plot", plotOutput("Coefficientplot",  click = "plot_click")),
                       verbatimTextOutput("info"),
                       tabPanel(h3("Forecast error boxplot and density plot"), title = uiOutput("forecasts")),
                       tabPanel("Forecast error boxplot and density plot", plotOutput("forecasts")),
                       tabPanel(h3("Forecast accuracy - Correlation coefficients - OLS"), title = uiOutput("ForecastAccuracyTitleOLScorr")),
                       tabPanel("Forecast accuracy - Correlation coefficients - OLS", div(tableOutput("ForecastAccuracyOLScorr"), value = "title"), style = "font-size:150%"),
                       tabPanel(h3("Forecast accuracy - OLS"), title = uiOutput("ForecastAccuracyTitleOLS")),
                       tabPanel("Forecast accuracy - OLS", div(tableOutput("ForecastAccuracyOLS"), value = "title"), style = "font-size:150%"),
                       tabPanel(h3("Forecast accuracy - ETS"), title = uiOutput("ForecastAccuracyTitleETS")),
                       tabPanel("Forecast accuracy - ETS", div(tableOutput("ForecastAccuracyETS"), value = "title"), style = "font-size:150%"),
                       tabPanel(h3("Forecast future - One week - OLS"), title = uiOutput("titleforefutureOLS")),
                       tabPanel("Forecast future - One week - OLS", div(DT::dataTableOutput("forecastfutureOLS"), value = "title"), style = "font-size:150%"),
                       tabPanel(h3("Forecast future - One week - ETS"), title = uiOutput("titleforefutureETS")),
                       tabPanel("Forecast future - One week - ETS", div(DT::dataTableOutput("forecastfutureETS"), value = "title"), style = "font-size:150%")
                     ))
))
