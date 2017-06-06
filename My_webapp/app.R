library(shiny)
library(shinythemes)
library(shinydashboard)

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
}


shinyApp(ui = ui, server = server)