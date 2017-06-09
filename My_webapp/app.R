library(shiny)
library(shinythemes)
library(shinydashboard)

source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")
source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/actoplot_v4.R")



ui <- navbarPage(theme = shinytheme("flatly"),
    title = "Analysis of Circadian Rhythm",
    id = "inTabset",
    HTML("<a name='top'></a>"),
    tabPanel("Home",
             fluidRow(
             column(6, offset = 3, img(src="11175406.jpg", align = "center", width = "898px"))
             ),
             fluidRow(
               column(6, offset = 3,
                      wellPanel(h2("Abstract"),
                                tags$head(
                                  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                                ),hr(),
                                "Circadian Rhythm is "))
             ),
             fluidRow(column(1, offset = 8, 
                    actionButton('nextpage1', 'Next Page')#, style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             ))
             ),
    tabPanel("Introduction",
             fluidRow(
               column(6, offset = 3, 
                      p("this is one paragraph"),"test")
             ),
             fluidRow(
               column(2, offset = 7,
                      actionButton('prevpage1', 'Previous Page'),
                      actionButton('nextpage2', 'Next Page')
                      )
             )),
    tabPanel("Actoplot",
             
             fluidRow(
               column(6, offset = 3,
                      h5("Below is a GUI for the function", code("actoplot()"), "with limited optional arguements available purely for demonstration. The full function is more flexible.", br(), "Please be patient as it may take time to load data."))
             ),
             selectInput("dataset", label = h3("Select example data to load"), 
                         choices = c("dam1ex1", "dam1ex2"),
                         width = "20%"), 
             actionButton('loaddata', 'Load data'),
             br(), br(), br(),
             h5(textOutput("ex1text")),
             uiOutput("hline"),
             dataTableOutput('ex1'),
             hr(),
             sidebarLayout(uiOutput("sidePanel"),
                           mainPanel(plotOutput("actogram", width = "100%", height = "700"))
             ),
             # fluidRow(
             #   column(8, offset = 2,
             #          plotOutput("actogram", width = "100%", height = "700")
             #   )),
             fluidRow(column(2, offset = 7, HTML("<a href='#top'>top of page</a>")))
             ),
    tabPanel("Methods and Results",
             withMathJax(),
             fluidRow(
               column(6, offset = 3, 
                      helpText('An irrational number \\(\\sqrt{2}\\)
           and a fraction $$1-\\frac{1}{2}$$'))
             )
             )
)


server <- function(input, output, session) {
  observeEvent(input$nextpage2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Methods and Results")
  })
  observeEvent(input$prevpage1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Home")
  })
  observeEvent(input$nextpage1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Introduction")
  })
  
  observeEvent(input$loaddata, {
    withProgress(message = "loading", value = 0,{
    dam1ex1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C03.txt")
    dam1ex2 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Estaban_new_data/Circadian_data_for_Nicholas/220914es5/220914es5CtM011C27.txt")
    output$sidePanel <- renderUI(sidebarPanel(
      sliderInput("dup_num", label = h3("Number of duplicated days:"),
                  min = 1, max = 10, value = 2),
      selectInput("operation", label = h3("Operation to perform:"),
                  choices = list("mean" = "mean", "median" = "median", "sum" = "sum"), selected = "mean"),
      selectInput("plot_type", label = h3("Type of plot:"),
                  choices = list("bar" = "bar", "line" = "line",
                                 "ribbon" = "ribbon", "tile" = "tile"), selected = "bar"),
      
      conditionalPanel("input.plot_type == 'bar'",
                       h4("DD day range:"), p("(e.g. From 4 to 8)"),
                       #br(),
                       column(6, selectInput("DD_start", label = "From", selected = NULL, choices = list(NULL = "none",0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))),
                       column(6, selectInput("DD_end", label = "to", selected = NULL, choices = list(NULL = "none",0,1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))),
                       h4("LD day range:"), p("(e.g. From 0 to 3)"),
                       #br(),
                       column(6, selectInput("LD_start", label = "From", selected = NULL, choices = list(NULL = "none",0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))),
                       column(6, selectInput("LD_end", label = "to", selected = NULL, choices = list(NULL = "none",0,1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))),
                       h4("Time when lights turn on and off:"), p("(e.g. Darkness starts 0; Darkness ends, Light starts 12; Light ends 24)"),
                       #br(),
                       fluidRow(
                       column(4, br(), numericInput("D_start", label = "Darkness starts", value = 0, min = 0, max = 100)),
                       column(4, numericInput("D_end_L_start", label = "Darkness ends, Light starts", value = 0, min = 0, max = 100)),
                       column(4, br(), numericInput("L_ends", label = "Light ends", value = 0, min = 0, max = 100))),
                       fluidRow(column(4, numericInput("LD_offset", label = "Offset LD", value = 0, min = -100, max = 100)))
                       ),
      conditionalPanel("input.plot_type == 'line'||input.plot_type == 'ribbon'||input.plot_type == 'tile'", "DD and LD options only available for bar plots currently"),
      hr(),
      actionButton(inputId = "go",
                   label = "Update Plot")
    ))
    
    data <- eventReactive(input$go, {actoplot_dam1(isolate(datasetInput()),
                                                   num_of_plot = input$dup_num,
                                                   type_of_plot = input$plot_type, #currently only "bar" has LD and DD annotations available
                                                   DD_days_start = input$DD_start,
                                                   DD_days_end = input$DD_end,
                                                   LD_days_start = input$LD_start,
                                                   LD_days_end = input$LD_end,
                                                   LD_offset = input$LD_offset,
                                                   D_start = input$D_start,
                                                   D_end_L_start = input$D_end_L_start,
                                                   L_end = input$L_ends,
                                                   operation = input$operation,
                                                   pop_overview = mean,
                                                   time_to_round = hours(1))})
    output$actogram <- renderPlot({
      data()
    })
    datasetInput <- reactive({
      switch(input$dataset,
           "dam1ex1" = dam1ex1,
           "dam1ex2" = dam1ex2)
      })
    datasetText <- reactive({
      switch(input$dataset,
             "dam1ex1" = "Displayed below is raw example data from DAM1 machines.",
             "dam1ex2" = "Displayed below is raw example data from DAM2 machines.")
      })
    output$hline <- renderUI(hr())
    output$ex1text <- renderText(
      isolate(datasetText())
      )
    output$ex1 <- renderDataTable(
        isolate(datasetInput()), options = list(
          lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All')),
          pageLength = 10,
          orderClasses = TRUE
          ))
    })
  })
}


shinyApp(ui = ui, server = server)