library(shiny)
library(shinythemes)
library(shinydashboard)

source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")
dam1ex1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C03.txt")
dam1ex2 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Estaban_new_data/Circadian_data_for_Nicholas/220914es5/220914es5CtM011C27.txt")
# acto = actoplot_dam1(dam1ex1,
#                      num_of_plot = 2,
#                      type_of_plot = "bar", #currently only "bar" has LD and DD annotations available
#                      LD_days = 0:2,
#                      DD_days = 3:15,
#                      LD_offset = 0,
#                      D_start = 0,
#                      D_end_L_start = 12,
#                      L_end = 24,
#                      operation = mean,
#                      pop_overview = mean,
#                      time_to_round = hours(1))


ui <- navbarPage(theme = shinytheme("flatly"),
    title = "Analysis of Circadian Rhythm",
    id = "inTabset",
    tabPanel("Home",
             fluidRow(
             column(6, offset = 3, img(src="11175406.jpg", align = "center", width = "898px"))
             ),
             fluidRow(
               column(6, offset = 3,
                      wellPanel(h2("Abstract"),hr(),
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
                      "Below is a GUI for the function", code("actoplot()"), "with a few optional arguements available purely for demonstration.")
             ),
             selectInput("dataset", label = h3("Select example data to load"), 
                         choices = c("dam1ex1", "dam1ex2"),
                         width = "20%"), 
             textOutput("ex1text"),
             hr(),
             dataTableOutput('ex1')
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
  output$ex1text <- renderText(
    datasetText()
  )
  output$ex1 <- renderDataTable(
      datasetInput(), options = list(
        lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All')),
        pageLength = 10,
        orderClasses = TRUE
    ))
  # output$ex1plot <- renderText()
}


shinyApp(ui = ui, server = server)