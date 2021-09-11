library(shiny)
library(shinydashboard)
library(readxl)
library(mice)
library(ggplot2)
library(cowplot)
library(dplyr)
library(prophet)
library(dygraphs)



ui <- 
  dashboardPage(
    
    dashboardHeader(title = "Cagece BDML 0.1"),
    
    dashboardSidebar( collapsed = TRUE,
      sidebarMenu(
        tags$style(".fa-circle {color:rgb(0,100,0)}"),
        tags$style(".fa-biohazard {color:rgb(100,0,0)}"),
        menuItem("toxinas", tabName = "AbaToxinas", icon = icon("biohazard"),
                 menuSubItem("Dataset", tabName = "submenudataset", icon = icon("table")),
                 menuSubItem("Previsão", tabName = "submenuprevisao", icon = icon("forward"))),
        menuItem("Clorofila-a", tabName = "AbaClorofila", icon = icon("circle"))
    )
    
  ),
    dashboardBody(
        
        tabItems(
      
          tabItem(tabName = "submenudataset",
      
            conditionalPanel(condition = "input.toxinasnograf.includes('mc') || input.toxinasnograf.includes('cyn') || input.toxinasnograf.includes('stx') ",
                
                             fluidRow( box(plotOutput("plottoxin", hover = hoverOpts("plot_hover",delay = 100)), width = "100%", collapsible = TRUE))),
            conditionalPanel(condition = "input.plot_hover != null & output.hover_info != null", box(verbatimTextOutput("hover_info"), width =12)),
    
      fluidRow(box(dateRangeInput("datas","Período", start = "2010-10-01", end = "2019-04-15", format = "dd-mm-yyyy", separator = "até"
                    , language = "pt-BR", autoclose = TRUE)), box(checkboxGroupInput("toxinasnograf", "toxinas", choices = list("Microcistina" = "mc",
                                                                                                                           "Cilindrospermopsina" = "cyn",
                                                                                                                           "Saxitoxina" = "stx"), selected = FALSE))),
      conditionalPanel(condition = "input.toxinasnograf.includes ('mc')", fluidRow(box(verbatimTextOutput("summarymc"), title = "Microcistina", width = "auto"))),
      conditionalPanel(condition = "input.toxinasnograf.includes ('cyn')", fluidRow(box(verbatimTextOutput("summarycyn"), title = "Cilindrospermopsina", width = "auto"))),
      conditionalPanel(condition = "input.toxinasnograf.includes ('stx')", fluidRow(box(verbatimTextOutput("summarystx"), title = "Saxitoxina", width = "auto")))
    ),

          tabItem(tabName = "submenuprevisao",
                  conditionalPanel(condition = "input.previsaonograf.includes('mc') || input.previsaonograf.includes('cyn') || input.previsaonograf.includes('stx') ",
                                   fluidRow( box(dygraphOutput("plotprevisao"), width = "100%", collapsible = TRUE))),
                                   
                  fluidRow(box(sliderInput("param1previsao", "Pontos de mudança", min = 0, max = 1, value = 0.05, step = 0.005, animate = TRUE)),
                  box(numericInput("diasdeprevisao", "Dias de previsão",value = 60,step = 1, min =1, max = 60))),
                  fluidRow(box(checkboxGroupInput("previsaonograf", "toxinas", choices = list("Microcistina" = "mc",
                                                                                                 "Cilindrospermopsina" = "cyn",
                                                                                                 "Saxitoxina" = "stx"), selected = FALSE)))
                  
                  
                  
                  ),
 
  
      tabItem(tabName = "AbaClorofila",
              h1("Em breve"))
  ) 
 )
 )




server <- function(input, output, session) {
  #mudar para receber arquivos
  
  diasdeprevisao = reactive({input$diasdeprevisao})
  
  toxinas = read.csv("alltoxins.csv",
                     encoding = "UTF-8" )

  toxinas$Date=as.Date(toxinas$Date) 
  
  Toxinasmc = toxinas[toxinas$type == "mc",] 
  
  Toxinascyn = toxinas[toxinas$type == "cyn",] 
  
  Toxinasstx = toxinas[toxinas$type == "stx",] 
  
  colnames(Toxinasmc) = c("ds","y","tipo")
  
  colnames(Toxinascyn) = c("ds","y","tipo")
  
  colnames(Toxinasstx) = c("ds","y","tipo")
  
  modelomc = prophet(Toxinasmc, changepoint.prior.scale = 0.06, interval.width = 0.8)
  
  modelocyn = prophet(Toxinasmc)
  
  modelostx = prophet(Toxinasmc)
  
  futuromc = make_future_dataframe(modelomc, periods = 60)
  
  futurocyn = make_future_dataframe(modelocyn, periods = 365)
  
  futurostx = make_future_dataframe(modelostx, periods = 365)
  
  forecastmc = predict(modelomc,futuromc)
  
  forecastcyn = predict(modelocyn,futurocyn)
  
  forecaststx = predict(modelostx,futurostx)
  

  
  output$plottoxin = 
    renderPlot(ggplot(toxinas[toxinas$type == c(input$toxinasnograf[1],input$toxinasnograf[2],input$toxinasnograf[3]), ],aes(x = Date, y = Tox, group = type))+
                                  geom_line(aes(colour = type), size = 1)+
                                  geom_point(shape = 16 , size =1. )+
                                  theme_cowplot()+
                                  scale_x_date(limits = as.Date(input$datas))+
                                  theme(axis.text.x = element_text(angle = 0),
                                        legend.title=element_blank(),
                                        legend.position = "top",
                                        #legend.position = c(.05,.95),
                                        legend.direction = "horizontal")+
                                  ylab(expression(paste(mu,g,L)^-1))+
                                  xlab(""))
  
  output$hover_info = renderPrint({nearPoints(toxinas, input$plot_hover, maxpoints = 1, threshold = 3)})

  output$summarymc =
    
      renderPrint(
      summary(toxinas[toxinas$Date >= input$datas[1] & toxinas$Date <= input$datas[2] & toxinas$type == "mc",]) 
      )
    
  
  output$summarycyn =
    
    renderPrint(
      summary(toxinas[toxinas$Date >= input$datas[1] & toxinas$Date <= input$datas[2] & toxinas$type == "cyn",])
    )
  
  output$summarystx =
    renderPrint(
      summary(toxinas[toxinas$Date >= input$datas[1] & toxinas$Date <= input$datas[2] & toxinas$type == "stx",])
    )
  output$plotprevisao = renderDygraph(prophet_plot_components(modelomc,forecastmc))
    
    
    
    
    #if_else(input$previsaonografr=="mc",renderPlot(prophet_plot_components(modelomc, forecastmc)),) + 
                        #if_else(input$previsaonografr == "cyn", renderPlot(prophet_plot_components(modelocyn, forecastcyn)),)+
                       #if_else(input$previsaonografr == "stx", renderPlot(prophet_plot_components(modelostx,forecaststx)),)
  
}

shinyApp(ui, server)


