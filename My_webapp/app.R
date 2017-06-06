library(shiny)
library(shinythemes)
library(shinydashboard)

ui <- navbarPage(theme = shinytheme("flatly"),
    title = "Circadian Rhythm",
    tabPanel("Introduction",
             tags$p("this is one paragraph"),
             fluidRow(
               column(3),
               column(5, "test")
             )
             ),
    tabPanel("Navbar 1",
             sidebarPanel(
               fileInput("file", "File input:"),
               textInput("txt", "Text input:", "general"),
               sliderInput("slider", "Slider input:", 1, 100, 30),
               tags$h5("Deafult actionButton:"),
               actionButton("action", "Search"),
               
               tags$h5("actionButton with CSS class:"),
               actionButton("action2", "Action button", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Tab 1",
                          h4("Table"),
                          tableOutput("table"),
                          h4("Verbatim text output"),
                          verbatimTextOutput("txtout"),
                          h1("Header 1"),
                          h2("Header 2"),
                          h3("Header 3"),
                          h4("Header 4"),
                          h5("Header 5")
                 ),
                 tabPanel("Tab 2"),
                 tabPanel("Tab 3")
               )
             )
    ),
    tabPanel("Navbar 2",
             mainPanel("test"))
)


server <- function(input, output) {}

shinyApp(ui = ui, server = server)