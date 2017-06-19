library(shiny)
library(shinythemes)
library(shinydashboard)

source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")
source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/actoplot_v6.R")

PATH1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M"
dammulti1 = DAM1_multi_reader(PATH1, time_format = "min")
data("sleep_sexual_dimorphism")
sleep_sexual_dimorphism = sleep_sexual_dimorphism[region_id <= 2]

ui <- navbarPage(theme = shinytheme("flatly"),
    title = "Analysis of Circadian Rhythm",
    id = "inTabset",
    footer = column(6, offset = 3, br(), hr(), "Nicholas Li, Imperial College London, 2017"),
    HTML("<a name='top'></a>"),
    tabPanel("Home",
             tags$head(tags$style(
               type="text/css",
               "#image img {max-width: 100%; width: 100%; height: auto}"
             )),
             fluidRow(
             column(6, offset = 3, img(src="11175406.jpg", align = "center", width = "898px")
                    ,
                    
                    imageOutput("image")
                    )
             ),
             fluidRow(
               column(6, offset = 3,
                      wellPanel(h2("Abstract"),
                                tags$head(
                                  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                                ),hr(),
                                "Circadian Rhythms are partly endogenous oscillations in biological process that exhibit "))
             ),
             fluidRow(column(1, offset = 8, 
                    actionButton('nextpage1', 'Next Page')#, style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             ))
             ),
    tabPanel("Introduction",
             fluidRow(
               column(6, offset = 3, 
                      "Circadian rhythms are partly endogenous oscillations in biological processes that exhibit an approximately 24 hour cycle. Hence the name “Circadian”, which stems from the Latin “circa diem” or “about a day”.", actionLink("ref1", tags$sup("[1]")),
"Circadian rhythms are important in many biological aspects as many physiological and molecular activities follow such oscillations. This ranges from the more obvious examples like sleep and feeding patterns throughout the dayand night, to rates of transcription and hormone production.", actionLink("ref2", tags$sup("[2]")),
p(),
"To maintain rhymicity, circadian rhythms are regulated by circadian clocks. In Drosophila melanogaster, physiological circadian rhythms are regulated by a network of about 150 neurons with the small ventral lateral neurons as the central pacemaker.", actionLink("ref3", tags$sup("[3]")),
"In humans, a group of 20,000 neurons called the suprachiasmatic nucleus (SCN) is located in the hypothalamus of the brain and this is considered as the master circadian clock. As well as controlling its own set of rhythmic processes, it also synchronises all other body clocks in the organism.", actionLink("ref4", tags$sup("[4]")),
"Without it, all physiological and molecular rhythmicity is lost.",
p(),
"Though organisms as whole experience circadian rhythms, even individual cells regulate their own rhythms. Almost every single cell has its own circadian clock. This is particularly evident in the regulation of the cell cycle, as cellular proliferation has been proven to be rhythmic with circadian  disruption being linked to cell cycle deregulation and possible tumour growth.",actionLink("ref5", tags$sup("[5]")),
" At the molecular level, almost all cells express so-called clock genes that construct feedback loops that regulate aforementioned molecular osciallitions. ")
             ),
             fluidRow(
               column(2, offset = 7,
                      actionButton('prevpage1', 'Previous Page'),
                      actionButton('nextpage2', 'Next Page')
                      )
             )),
    tabPanel("Actograms",
             fluidRow(
               column(6, offset = 3,
                      "One useful way of visualising activity data is via Actograms.")
             )),
    tabPanel("Actoplot",
             
             fluidRow(
               column(6, offset = 3,
                      h5("Below is a GUI for the function", code("actoplot()"), "with limited optional arguements available purely for demonstration. The full function is more flexible.", br(), "Please be patient as it may take time to load data. Only DAM1 data is available."))
             ),
             selectInput("dataset", label = h3("Select example data to load"), 
                         choices = c("dam1ex1", "dam1ex2", "dammulti1", "dam2", "ethoscope"),
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
    navbarMenu(title = "Other Methods",
    tabPanel("Autocorrelation",
             withMathJax(),
             fluidRow(
               column(6, offset = 3, 
                      h3("Autocorrelation"), "An actogram is useful for qualitative analysis, but often more quantitative methods are needed for decisive conclusions. One way is autocorrelation. ",
                      "Mathematically it is defined as:",
                      helpText("$$\\large{r=\\frac{\\sum_\\limits{t=1}^{N-1} (x_t-\\bar x_{(1)})(x_{t+1}-\\bar x_{(2)})}{\\sqrt{\\sum_\\limits{t=1}^{N-1} (x_t-\\bar x_{(1)})^2 \\sum_\\limits{t=1}^{N-1} (x_{t+1}-\\bar x_{(2)})^2}}}$$"),
                      "where", 
                      helpText("$$\\large{\\bar x_{(1)} = \\sum_\\limits{t=1}^{N-1} x_t/(N-1)}$$"),
                      "is the mean of of the first set of observation in each of the (N-1) pairs and", HTML("<span style='font-size:130%'>x&#772<sub>(2)</sub></span>"), "is the mean of the second set.", "Without showing all the steps, Chatifield (2003) demonstrates that the equation above can be conveniently made less complicated by approximating to",
                      helpText("$$\\large{r=\\frac{\\sum_\\limits{t=1}^{N-1} (x_t-\\bar x)(x_{t+1}-\\bar x)}{\\sum_\\limits{t=1}^{N} (x_t - \\bar x)^2}}$$"),
                      "as", HTML("<span style='font-size:130%'>x&#772<sub>(1)</sub> &#8776 x&#772<sub>(2)</sub></span>"), ", and by dropping the factor N/(N-1) since it is close to 1 for large N."
                      )
             )
             )),
    tabPanel("References",
             fluidRow(
               column(6, offset = 3, HTML("<a name='ref1'></a>"),
                      "[1] Vitaterna, M. H., Takahashi, J. S. & Turek, F. W. (2001) Overview of circadian rhythms. Alcohol Research and Health. 25 (2), 85-93.",
                      p(),
                      "[2] Panda, S., Hogenesch, J. B. & Kay, S. A. (2002) Circadian rhythms from flies to human. Nature. 417 (6886), 329-335.",
                      p(),
                      "[3] Beckwith, E. J. & Ceriani, M. F. (2015) Experimental assessment of the network properties of the Drosophila circadian clock. Journal of Comparative Neurology. 523 (6), 982-996.",
                      p(),
                      "[4] Bernard, S., Gonze, D., Čajavec, B., Herzel, H. & Kramer, A. (2007) Synchronization-induced rhythmicity of circadian oscillators in the suprachiasmatic nucleus. PLoS Comput Biol. 3 (4), e68.",
                      p(),
                      "[5] Feillet, C., Van Der Horst, Gijsbertus TJ, Levi, F., Rand, D. A. & Delaunay, F. (2015) Coupling between the circadian clock and cell cycle oscillators: implication for healthy cells and malignant growth. Frontiers in Neurology. 6 96.0000"
                      )
             ))
)


server <- function(input, output, session) {
  output$image <- renderImage({
    return(list(
      src = "www/11175406.jpg",
      contentType = "image/jpeg",
      width = "898px"
    ))
  }, deleteFile = FALSE)
  observeEvent(input$nextpage2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Autocorrelation")
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
      
      conditionalPanel("input.dataset == 'dammulti1'||input.dataset == 'ethoscope'",
                       selectInput("pop_overview", label = h3("Additional summary opperation to perform on population data:"),
                                   choices = list("mean" = "mean", "median" = "median", "sum" = "sum", "NULL" = "NULL"), selected = "mean")),
      
      conditionalPanel("input.dataset != 'ethoscope'",
                       selectInput("plot_type", label = h3("Type of plot:"),
                                   choices = list("bar" = "bar", "line" = "line", "ribbon" = "ribbon", "tile" = "tile"), selected = "bar")),
      
      conditionalPanel("input.dataset == 'ethoscope'",
                       selectInput("condition", label = h3("Y axis values to plot:"),
                                   choices = list(moving = "moving", asleep = "asleep", max_velocity = "max_velocity", is_interpolated = "is_interpolated"), selected = "moving")),
      
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
                   label = "Plot graph")
    ))
    
    data <- eventReactive(input$go, {actoplot(isolate(datasetInput()),
                                                   num_of_plot = input$dup_num,
                                              file_format = datasetFile_format(),
                                              condition = input$condition,
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
                                                   pop_overview = input$pop_overview,
                                                   time_to_round = hours(1))})
    output$actogram <- renderPlot({
      data()
    })
    datasetInput <- reactive({
      switch(input$dataset,
           "dam1ex1" = dam1ex1,
           "dam1ex2" = dam1ex2,
           "dammulti1" = dammulti1,
           "dam2" = dam2,
           "ethoscope" = sleep_sexual_dimorphism)
      })
    datasetFile_format <- reactive({
      switch(input$dataset,
             "dam1ex1" = "dam1",
             "dam1ex2" = "dam1",
             "dammulti1" = "dam1",
             "dam2" = "dam2",
             "ethoscope" = "ethoscope")
    })
    datasetText <- reactive({
      switch(input$dataset,
             "dam1ex1" = "Displayed below is an example  raw data from DAM1 machines.",
             "dam1ex2" = "Displayed below is raw example data from DAM1 machines.",
             "dammulti1" = "Displayed below is DAM1 population data",
             "dam2" = "Displayed below is DAM2 data",
             "ethoscope" = "Displayed below is ethoscope data")
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
  
  observeEvent(input$ref1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref5, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
}


shinyApp(ui = ui, server = server)