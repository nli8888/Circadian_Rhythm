library(shiny)
library(shinythemes)
library(shinydashboard)

source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")
source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/actoplot_v6.R")

PATH1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M"
dammulti1 = DAM1_multi_reader(PATH1, time_format = "min")
data("sleep_sexual_dimorphism")
sleep_sexual_dimorphism = sleep_sexual_dimorphism[region_id <= 2]

ui <- navbarPage(theme = shinytheme("readable"),
    title = "Analysis of Circadian Rhythm",
    # position = "fixed-top",
    inverse = TRUE,
    id = "inTabset",
    collapsible = TRUE,
    
    footer = column(6, offset = 3, br(), hr(), column(6, "Nicholas Li, Imperial College London, 2017", br(), br(), br()),
                    column(2, offset = 4, 
                           actionButton("top", "Top of Page", onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');")
                           # tags$b(HTML("<a href='#top'>top of page</a>"))
                           ), 
                    br(),br()),
    HTML("<a name='top'></a>"),
    tabPanel("Home",
             # tags$style(type="text/css", "body {padding-top: 70px;}"),
             onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');",
             fluidRow(
               column(6, offset = 3, 
                      wellPanel(tags$b("Note:"), "Due to the nature of R Shiny apps, your browser's 'previous-page' button will not work. This is true for all browsers. Apologies"))
             ),
             tags$head(tags$style(
               type="text/css",
               "#image img {max-width: 100%; width: 100%; height: auto}"
             )),
             fluidRow(
             column(6, offset = 3, #img(src="11175406.jpg", align = "center", width = "898px"),
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
                    actionButton('homenextpage', 'Next Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');")#, style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             ))
             ),
    
    tabPanel("Introduction",
             fluidRow(
               column(6, offset = 3, h2("Introduction"), hr(),
                      "Circadian rhythms are partly endogenous oscillations in biological processes that exhibit an approximately 24 hour cycle. Hence the name “Circadian” - derived from the Latin “circa diem” or “about a day”.", 
                      actionLink("ref1", tags$sup("[1]")),
"Circadian rhythms are important in many biological aspects as many physiological and molecular activities follow such oscillations. This ranges from sleep and feeding patterns throughout the day and night, to rates of transcription and hormone production.", 
actionLink("ref2", tags$sup("[2]")),
p(),
"To maintain rhythmicity, circadian rhythms are regulated by circadian clocks. In Drosophila melanogaster, physiological circadian rhythms are regulated by a network of about 150 neurons with the small ventral lateral neurons (sLNvs) as the central pacemaker.", 
actionLink("ref3", tags$sup("[3]")),
"In humans, a group of 20,000 neurons called the suprachiasmatic nucleus (SCN), located in the hypothalamus of the brain, is considered as the master circadian clock. As well as controlling its own set of rhythmic processes, it also synchronises all other body clocks in the organism.", 
actionLink("ref4", tags$sup("[4]")),
"Without it, all physiological and molecular oscillations become arhythmic.",
p(),
"Though organisms as a whole experience circadian rhythms, individual cells regulate their own rhythms as well. Almost every single cell has its own circadian clock. This is particularly evident in the regulation of the cell cycle, as cellular proliferation has been proven to be rhythmic with circadian  disruption being linked to cell cycle deregulation and possible tumour growth.",
actionLink("ref5", tags$sup("[5]")),
" At the molecular level, almost all cells express so-called clock genes that construct feedback loops that regulate aforementioned molecular osciallitions.",
p(),
"Circadian rhythms are only partly controlled by endogenoues means, as environmental cues can also entrain and synchronize such biological rhythms to envirmental oscillations.", 
"These cues, known as", tags$i("zeitgebers"), "(German for “time giver”), achieves entrainment when the period and phase of the biological rhythms match those of the environmental oscillations.", 
actionLink("ref3.1", tags$sup("[3]")),
"Examples of", tags$i("zeitgebbers"), "are light and temperature, and these are the most common and prominent ones among the animal kingdom. As the sun rises during mornings and falls during evenings, daylight changes to night and temperatures oscillate, which forces entrainment on most organisms. Hence the daily rountine activities of feeding and sleep can be shown to follow such enivironmental cues.", 
"When completely isolated from", tags$i("zeitgebbers"), "an organisms rhythmicity will then be solely regulated by its endogenous circadian clocks. These might not necessarily be around 24 hours and can be anywhere between 18 to 32 hours, hence why entrainment by", tags$i("zeitgebers"), 
"is required. The period during which an organisms rhythmicity is solely regulated endogenously is called the free-running period", actionLink("ref6", tags$sup("[6]")),
tags$b("(Fig. 1)"),"."
)
             ),
              tags$head(tags$style(
                type="text/css",
                "#image2 img {max-width: 100%; width: 100%; height: auto}"
              )),
             fluidRow(br(),
               column(6, offset = 3, 
                        column(6, imageOutput("image2")), 
                      column(6, wellPanel(tags$b("Figure 1."), "A diagram to demostrate rhythmic entrainment to a", tags$i("zeitgeber"), 
                                          "in this case light (where the sun icon represents light and moon represents darkness). Black bars represent variable being examined, such as “sleep” in this example. Top half panel: typically, sleep beings when darkness occurs and stops when light returns. The sleep pattern follows that of the oscillation of light with the same phase and period.", 
                                          "Bottom half panel: however, when the", tags$i("zeitgeber"), "is taken away, the organisms sleep pattern deviates to its own endogenous rhythm and is describe as “free-running”.",
                                          "Credit: Golombek, D. A. & Rosenstein, R. E. (2010)", actionLink("ref7", tags$sup("[7]"))))
                        )),
             fluidRow(br(),
               column(6, offset = 3, 
                      "Experimentalists have methods of studying the aspects of circadian rhythms mentioned above", "(see", actionLink("intronextpagelink", "next page", onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');"),") but have few software that allows practical analysis of acquired data.", 
                      "Recently, the Gilestro Laboratory have developed and published their own custom equipment, named “Ethoscopes”, for observing animal activity in the lab and accompanying R package “rethomics” for analytics (publicly available at", tags$a(href="http://gilestrolab.github.io/ethoscope/", "http://gilestrolab.github.io/ethoscope/", target="_blank"), ")", 
                      actionLink("ref8", tags$sup("[8]")),
                      ". But the package lacks any specific functions related to circadian rhythm analysis.", br(), br(),
                      wellPanel(tags$b("Aim"),br(),'To develop an R package that is compatible with an already existing package developed by Gilestro Laboratory ("rethomics") and helps experimentalist analyse circadian rhythm.'))
             ),
             fluidRow(
               column(2, offset = 7,
                      actionButton('introprevpagebutton', 'Previous Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');"),
                      actionButton('intronextpagebutton', 'Next Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');")
                      )
             )),
    tabPanel("Equipment Acquisition",
             onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');",
      #title=actionLink("EquimentAcquisitionTab", "Equipment Acquisition", onclick="$('html,body').scrollTop(0);"),
             fluidRow(
               column(6, offset = 3,
                      h2("Equipment for studying", tags$i("Drosophila"), "activity"), hr(),
                      "Before analyzing data, the package must be able to import and read data from equipment commonly used by experimentalists to study circadian rhythms.", 
                      "As", tags$i("Drosophila"), "are well characterised model organisms they tend to be chosen for most studies. Traditionally,", 
                      tags$b("Drosophila Activity Monitors"), "(DAMs), purchasable from TriKinetics (", tags$a(href="http://www.trikinetics.com", "http://www.trikinetics.com", target="_blank"), ") have been used in most labs", 
                      tags$b("(Fig. 2)"),".")
             ),
             tags$head(tags$style(
               type="text/css",
               "#image3 img {max-width: 100%; width: 100%; height: auto}"
             )),
             fluidRow(br(),
                      column(6, offset = 3, 
                             column(6, imageOutput("image3")),
                             column(6, wellPanel(tags$b("Figure 2."), 
                                                 "Drosophila Activity Monitor (DAM) pictured. DAMs consist of 32 holding docks that can each be equipped with approximately 5mm diameter wide transparent tubes big enough to accommodate", tags$i("Drosophila."), 
                                                 "Typically, the tubes are centered in the middle, and where the DAM holds the tube is an infrared beam that spans across the tube. Sensors at the opposite side of the tube facing the source of the beam detect it. The number of times the beam is broken by", 
                                                 tags$i("Drosophila"), "crossing it over a certain period is recorded.",
                                                 "Credit: Rosato, E. & Kyriacou, C. P. (2006)", actionLink("ref9", tags$sup("[9]"))
                                                 ))
                      )),
             fluidRow(br(),
                      column(6, offset = 3,
                             "Older DAM models record data in DAM1 format, where individual (.txt) files are created for each individual animal. Whereas newer DAM models record data in DAM2 format, where data for all 32 animals are stored in a single  (.txt) file.", 
                             "“Rethomics” includes the function", code("loadDAM2Data()"), "to read DAM2 data format but does not have one for DAM1. Therefore, the initial step was to develop functions for reading DAM1 data.",
                             p(),
                             "2 were developed:",
                             tags$ul(
                               tags$li("One for reading a single file (individual data)  -", code("DAM1_single_reader()"), "where the primiary argument is the directory path of the DAM1 file;"),
                               tags$li("One for reading multiple files (population data) -", code("DAM1_multi_reader()"), "where the primary argument is the directory path of the folder contatining all the DAM1 files of interest")
                             ),
                             p(),
                             "For more documentation and source code please refer to", tags$a(href="https://github.com/nli8888/Circadian_Rhythm", "https://github.com/nli8888/Circadian_Rhythm", target="_blank"),
                             p(),
                             "Additionally, as aforementioned Geissmann", tags$i("et al."), "(2017) from the Gilestro Laboratory have developed their own equipment for studying Drosophila called “Ethoscopes” and as such, “rethomics” can already import raw ethoscope data directly.", 
                             actionLink("ref8.1", tags$sup("[8]")),
                             "Ethoscope machines are designed for higher throughput analysis of animal activity compared to DAM", tags$b("(Fig 3)"), "."
                             )),
             tags$head(tags$style(
               type="text/css",
               "#image4 img {max-width: 100%; width: 100%; height: auto}"
             )),
             fluidRow(br(),
                      column(6, offset = 3,
                             column(8, offset = 2, imageOutput("image4"))
                             )
                      ),
             fluidRow(
                      column(6, offset = 3,
                             wellPanel(tags$b("Figure 3. a)"), 
                                       "Blown-up view of a prototypical ethoscope. The upper case contains a Raspberry Pi and its HD camera which tracks and records animal behaviours in the arena. The lower case contains infrared lights and support for the experimental arena. Custom arena designs can be made as long as they fit the required dimensions, which allow for flexible experimental design. For", 
                                       tags$i("Drosophila,"), "the arena is typically designed to equip the same transparent tubes used in DAM, with all tubes laid out horizontally to allow full video tracking.",
                                       tags$b("b)"), "Rendered model of fully assembled product with actual size dimensions shown. Guide slits allow the arena to slide and lock into position.",
                                       "Credit: Geissmann", tags$i("et al."), "(2017)", actionLink("ref8.2", tags$sup("[8]"))
                                       )
                             )),
      fluidRow(column(6, offset = 3,
                      "Ethoscopes offer more dynamical experimentation and does not have the same limitations as DAM. While DAM can only measure the number of times",
                      tags$i("Drosophila,"), "has crossed the infrared beam, ethoscopes can record all activity using its HD camera and video tracking software. Therefore, behaviours that would otherwise not be captured by DAM, such as micro-movements (grooming, eating etc.), can be observed and distinguished from sleeping or walking.",
                      p(), "Below is a short video demonstrating ethoscopes; for more details and tutorials on ethoscopes, please visit:", tags$a(href="http://gilestrolab.github.io/ethoscope/", "http://gilestrolab.github.io/ethoscope/", target="_blank"),
                      p()
                      )),
             fluidRow(
               column(6, offset = 3, 
                      HTML('<iframe width="768" height="432" class="embed-responsive-item  m-x-auto d-block" src="https://www.youtube.com/embed/5oWGBUMJON8" frameborder="0" allowfullscreen></iframe>')
               )
             ),
      fluidRow(
        column(2, offset = 7,
               actionButton('equipprevpagebutton', 'Previous Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');"),
               actionButton('equipnextpagebutton', 'Next Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');")
        )
      )
             ),
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
             selectInput("dataset", label = h3("1) Select example data to load"), 
                         choices = c("dam1ex1", "dam1ex2", "dammulti1", "dam2", "ethoscope"),
                         width = "20%"), 
             actionButton('loaddata', 'Load data'),
             hr(),
             dataTableOutput('ex1'),
             # br(), br(), br(),
             h5(textOutput("ex1text")),
             uiOutput("hline"),
             sidebarLayout(uiOutput("sidePanel"),
                           mainPanel(plotOutput("actogram", width = "100%", height = "700"))
             )
             # fluidRow(
             #   column(8, offset = 2,
             #          plotOutput("actogram", width = "100%", height = "700")
             #   )),
             # fluidRow(column(2, offset = 7, HTML("<a href='#top'>top of page</a>")))
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
                      "[5] Feillet, C., Van Der Horst, Gijsbertus TJ, Levi, F., Rand, D. A. & Delaunay, F. (2015) Coupling between the circadian clock and cell cycle oscillators: implication for healthy cells and malignant growth. Frontiers in Neurology. 6 96.0000",
                      p(),
                      "[6] Aschoff, J. (1981) Freerunning and entrained circadian rhythms. In: Anonymous Biological rhythms. , Springer. pp. 81-93.",
                      p(),
                      "[7] Golombek, D. A. & Rosenstein, R. E. (2010) Physiology of circadian entrainment. Physiological Reviews. 90 (3), 1063-1102.",
                      p(),
                      "[8] Geissmann, Q., Rodriguez, L. G., Beckwith, E. J., French, A. S., Jamasb, A. R. & Gilestro, G. F. (2017) Ethoscopes: an open platform for high-throughput ethomics. Biorxiv. 113647.",
                      p(),
                      "[9] Rosato, E. & Kyriacou, C. P. (2006) Analysis of locomotor activity rhythms in Drosophila. Nature Protocols. 1 (2), 559-568."
                      )
             ))
)


server <- function(input, output, session) {
  output$image <- renderImage({
    return(list(
      src = "www/11175406.jpg",
      contentType = "image/jpeg"
      # ,
      # width = "898px"
    ))
  }, deleteFile = FALSE)
  
  output$image2 <- renderImage({
    return(list(
      src = "www/F2.large.jpg",
      contentType = "image/jpeg"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$image3 <- renderImage({
    return(list(
      src = "www/nprot.2006.79-F4.jpg",
      contentType = "image/jpeg"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$image4 <- renderImage({
    return(list(
      src = "www/ethoscope1.png",
      contentType = "image/png"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$video <- renderUI({
    tags$video(src="https://www.youtube.com/embed/5oWGBUMJON8", type = "video/mp4", autoplay = NA, controls = NA)
  })
  
  observeEvent(input$EquimentAcquisitionTab, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Equipment Acquistion")
  })
  
  observeEvent(input$intronextpagelink, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Equipment Acquisition")
    #HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$intronextpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Equipment Acquisition")
  })
  
  observeEvent(input$introprevpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Home")
  })
  
  observeEvent(input$equipprevpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Introduction")
  })
  
  observeEvent(input$equipnextpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Actograms")
  })
  
  observeEvent(input$homenextpage, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Introduction")
  })
  
  observeEvent(input$loaddata, {
    withProgress(message = "loading", value = 0,{
    dam1ex1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C03.txt")
    dam1ex2 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Estaban_new_data/Circadian_data_for_Nicholas/220914es5/220914es5CtM011C27.txt")
    
    output$sidePanel <- renderUI(sidebarPanel(h3("2) Plot graph"),
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
    
    data <- eventReactive(input$go, {withProgress(message = "loading", value = 0,{
      actoplot(isolate(datasetInput()),
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
                                                   time_to_round = hours(1))
     })})
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
  
  observeEvent(input$ref3.1, {
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
  
  observeEvent(input$ref6, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref7, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref8, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref8.1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref9, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref8.2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
}


shinyApp(ui = ui, server = server)