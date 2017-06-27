library(shiny)
library(shinythemes)
library(shinydashboard)

source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")
source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/actoplot_v6.R")

# source("./DAM1_reader.R")
# source("./actoplot_v6.R")

PATH1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M"
dammulti1 = DAM1_multi_reader(PATH1, time_format = "min")
data("sleep_sexual_dimorphism")
sleep_sexual_dimorphism = sleep_sexual_dimorphism[region_id <= 2]

ui <- navbarPage(theme = shinytheme("readable"),
    title = "Analysis of Circadian Rhythm",
    position = "fixed-top",
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
             tags$style(type="text/css", "body {padding-top: 70px;}"),
             # onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');",
             fluidRow(
               column(6, offset = 3, 
                      wellPanel(tags$b("Note:"), "Due to the nature of R Shiny apps, your browser's 'previous-page' button will not work. This is true for all browsers. Apologies"))
             ),
             tags$head(tags$style(
               type="text/css",
               "#image img {max-width: 100%; width: 100%; height: auto}"
             )),
             fluidRow(
               # column(3, div(style = "height:313px;")),
             column(6, offset = 3, #img(src="11175406.jpg", align = "center", width = "898px"),
                    # HTML('<a href="f1_actogram.png" target="_blank">'),
                    imageOutput("image", height = "auto")
                    # ,HTML("</a>")
                    )
             ),
             fluidRow(br(),
               column(6, offset = 3,
                      wellPanel(h2("Abstract"),
                                tags$head(
                                  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                                ),hr(),
                                HTML('<p style="font-size:18px">'),
                                "Circadian rhythms are important in many biological aspects from the molecular level, such as regulation of the cell cycle, to physiological activities, like sleeping patterns. Deregulation of biological processes that experience circadian rhythms have been shown to be detrimental to the functionality and health of the organism in question.",
"Therefore, studying circadian rhythms are of great interest and can offer insight into how such processes are regulated.",br(), "Demonstrated in this website are techniques that experimentalists commonly use to study circadian rhythms via Drosophila Activity Monitors (DAMs) and recently developed custom machinery “Ethoscopes” from the Gilestro Laboratory (Imperial College London).", 
"Also shown are the analysis of acquired data using common methods such as visual analysis via actograms, and quantitative analysis via autocorrelation and periodograms. Furthermore demonstrated, is a custom R package developed with the aim of helping experimentalists import and process raw data which can then be visualised and analysed via actograms.",
HTML('</p>')
),
h2("Lay Summary"), hr(),
HTML('<p style="font-size:18px">'),
"Circadian rhythms are any semi-endogenous rhythms that follow a period of about 24 hours. Hence the name “circadian”, which comes from the Latin phrase of “about a day”. The periodicity can be entrained (or enforced) by environmental factors, known as", tags$i("zeitgebers,"), 
"German for “time-giver”.  The most common example being light/darkness. Circadian rhythms occur in many different biological aspects from the molecular level to physiological states, such as sleeping patterns for example, and therefore is quite important in biology and needs to be studied.", 
"Shown in this website are some of the few common ways of analysing circadian rhythm data from visual methods to more quantitative calculations. In addition, a custom R package has been developed to help experimentalists visualise and inspect raw data gathered from typical experimental procedures.",
HTML('</p>')
)
             ),
             fluidRow(column(1, offset = 8, 
                    actionButton('homenextpage', 'Next Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');")#, style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             ))
             ),
    
    tabPanel("Introduction",
             fluidRow(
               column(6, offset = 3, h2("Introduction"), hr(),
                      HTML('<p style="font-size:18px">'),
                      "Circadian rhythms are partly endogenous oscillations in biological processes that exhibit an approximately 24 hour cycle. Hence the name “Circadian” - derived from the Latin “circa diem” or “about a day”.", 
                      actionLink("ref1", tags$sup("[1]")),
"Circadian rhythms are important in many biological aspects as many physiological and molecular activities follow such oscillations. This ranges from sleep and feeding patterns throughout the day and night, to rates of transcription and hormone production.", 
actionLink("ref2", tags$sup("[2]")),
br(),br(),
"To maintain rhythmicity, circadian rhythms are regulated by circadian clocks. In Drosophila melanogaster, physiological circadian rhythms are regulated by a network of about 150 neurons with the small ventral lateral neurons (sLNvs) as the central pacemaker.", 
actionLink("ref3", tags$sup("[3]")),
"In humans, a group of 20,000 neurons called the suprachiasmatic nucleus (SCN), located in the hypothalamus of the brain, is considered as the master circadian clock. As well as controlling its own set of rhythmic processes, it also synchronises all other body clocks in the organism.", 
actionLink("ref4", tags$sup("[4]")),
"Without it, all physiological and molecular oscillations become arhythmic.",
br(),br(),
"Though organisms as a whole experience circadian rhythms, individual cells regulate their own rhythms as well. Almost every single cell has its own circadian clock. This is particularly evident in the regulation of the cell cycle, as cellular proliferation has been proven to be rhythmic with circadian  disruption being linked to cell cycle deregulation and possible tumour growth.",
actionLink("ref5", tags$sup("[5]")),
" At the molecular level, almost all cells express so-called clock genes that construct feedback loops that regulate aforementioned molecular osciallitions.",
br(),br(),
"Circadian rhythms are only partly controlled by endogenoues means, as environmental cues can also entrain and synchronize such biological rhythms to envirmental oscillations.", 
"These cues, known as", tags$i("zeitgebers"), "(German for “time giver”), achieves entrainment when the period and phase of the biological rhythms match those of the environmental oscillations.", 
actionLink("ref3.1", tags$sup("[3]")),
"Examples of", tags$i("zeitgebbers"), "are light and temperature, and these are the most common and prominent ones among the animal kingdom. As the sun rises during mornings and falls during evenings, daylight changes to night and temperatures oscillate, which forces entrainment on most organisms. Hence the daily rountine activities of feeding and sleep can be shown to follow such enivironmental cues.", 
"When completely isolated from", tags$i("zeitgebbers"), "an organisms rhythmicity will then be solely regulated by its endogenous circadian clocks. These might not necessarily be around 24 hours and can even deviate by a few hours, hence why entrainment by", tags$i("zeitgebers"), 
"is required. The period during which an organisms rhythmicity is solely regulated endogenously is called the free-running period", actionLink("ref6", tags$sup("[6]")),
tags$b("(Fig. 1)"),".", HTML('</p>')
)
             ),
              tags$head(tags$style(
                type="text/css",
                "#image2 img {max-width: 100%; width: 100%; height: auto}"
              )),
             fluidRow(br(),
               column(6, offset = 3, 
                        column(6,
                               HTML('<a href="F2.large.jpg" target="_blank">'),
                               imageOutput("image2", height = "100%"),
                               HTML('</a>')
                               ), 
                      column(6, wellPanel(HTML('<p style="font-size:18px">'),
                                          tags$b("Figure 1."), "A diagram to demostrate rhythmic entrainment to a", tags$i("zeitgeber"), 
                                          ", in this case light (where the sun icon represents light and moon represents darkness). Black bars represent the variable being examined, such as “sleep” in this example. Top half panel: typically, sleep beings when darkness occurs and stops when light returns. The sleep pattern follows that of the oscillation of light with the same phase and period.", 
                                          "Bottom half panel: however, when the", tags$i("zeitgeber"), "is removed, the organism's sleep pattern deviates to its own endogenous rhythm and is describe as “free-running”.",
                                          "Credit: Golombek, D. A. & Rosenstein, R. E. (2010)", actionLink("ref7", tags$sup("[7]")), HTML('</p>')
                                          ))
                        )),
             fluidRow(br(),
               column(6, offset = 3, HTML('<p style="font-size:18px">'),
                      "Experimentalists have methods of studying the aspects of circadian rhythms mentioned above", "(see next page)", 
                      # actionLink("intronextpagelink", "next page", onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');"),
                      "but have few software that allows practical analysis of acquired data.", 
                      "Recently, the Gilestro Laboratory have developed and published their own custom equipment, named “Ethoscopes”, for observing animal activity in the lab and accompanying R package “rethomics” for analytics (publicly available at", tags$a(href="http://gilestrolab.github.io/ethoscope/", "http://gilestrolab.github.io/ethoscope/", target="_blank"), ")", 
                      actionLink("ref8", tags$sup("[8]")),
                      ". But the package lacks any specific functions related to circadian rhythm analysis.", br(), br(), HTML('</p>'),
                      wellPanel(HTML('<p style="font-size:18px">'),tags$b("Aim"),br(),'To develop an R package that is compatible with an already existing package developed by Gilestro Laboratory ("rethomics") that helps experimentalist analyse circadian rhythm and to demostrate other methods commonly used.',HTML('</p>'))
                      )
             ),
             fluidRow(
               column(2, offset = 7,
                      actionButton('introprevpagebutton', 'Previous Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');"),
                      actionButton('intronextpagebutton', 'Next Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');")
                      )
             )),
    tabPanel("Data Acquisition",
             # onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');",
      #title=actionLink("EquimentAcquisitionTab", "Equipment Acquisition", onclick="$('html,body').scrollTop(0);"),
             fluidRow(
               column(6, offset = 3,
                      h2("Equipment for studying", tags$i("Drosophila"), "activity"), hr(),
                      HTML('<p style="font-size:18px">'),
                      "Before analyzing data, the package must be able to import and read data from equipment commonly used by experimentalists to study circadian rhythms.", 
                      "As", tags$i("Drosophila"), "are well characterised model organisms they tend to be chosen for most studies. Traditionally,", 
                      tags$b("Drosophila Activity Monitors"), "(DAMs), purchasable from TriKinetics (", tags$a(href="http://www.trikinetics.com", "http://www.trikinetics.com", target="_blank"), ") have been used in most labs", 
                      tags$b("(Fig. 2)"),".",HTML('</p>'))
             ),
             tags$head(tags$style(
               type="text/css",
               "#image3 img {max-width: 100%; width: 100%; height: auto}"
             )),
             fluidRow(br(),
                      column(6, offset = 3, 
                             column(6, 
                                    HTML('<a href="nprot.2006.79-F4.jpg" target="_blank">'),
                                    imageOutput("image3", height = "auto"),
                                    HTML('</a>')
                                    ),
                             column(6, wellPanel(HTML('<p style="font-size:18px">'),tags$b("Figure 2."), 
                                                 "Drosophila Activity Monitor (DAM) pictured. DAMs consist of 32 holding docks that can each be equipped with approximately 5mm diameter wide transparent tubes big enough to accommodate", tags$i("Drosophila."), 
                                                 "Typically, the tubes are centered in the middle, and where the DAM holds the tube is an infrared beam that spans across the tube. Sensors at the opposite side of the tube facing the source of the beam detect it. The number of times the beam is broken by", 
                                                 tags$i("Drosophila"), "crossing it over a certain period is recorded.",
                                                 "Credit: Rosato, E. & Kyriacou, C. P. (2006).", actionLink("ref9", tags$sup("[9]")), HTML('</p>')
                                                 ))
                      )),
             fluidRow(br(),
                      column(6, offset = 3,
                             HTML('<p style="font-size:18px">'),
                             "Older DAM models record data in DAM1 format, where individual (.txt) files are created for each individual animal. Whereas newer DAM models record data in DAM2 format, where data for all 32 animals are stored in a single  (.txt) file.", 
                             "“Rethomics” includes the function", code("loadDAM2Data()"), "to read DAM2 data format but does not have one for DAM1. Therefore, the initial step was to develop functions for reading DAM1 data.",
                             br(),br(),
                             "2 were developed:",  HTML('</p>'),
                             tags$ul(
                               tags$li(HTML('<p style="font-size:18px">'),"One for reading a single file (individual data) -", code("DAM1_single_reader(file)"), ", where the primiary argument is the directory path of the DAM1 file;",HTML('</p>')),
                               tags$li(HTML('<p style="font-size:18px">'),"One for reading multiple files (population data) -", code("DAM1_multi_reader(dir)"), ", where the primary argument is the directory path of the folder contatining all the DAM1 files of interest",HTML('</p>'))
                             ),
                             br(),br(),HTML('<p style="font-size:18px">'),
                             "To be compatible to “rethomics”, data is read into", code("data.table()"), "format using the R package", tags$a(href="https://cran.r-project.org/web/packages/data.table/", "data.table", target="_blank"), ".",
                             "For more documentation and source code visit", tags$a(href="https://github.com/nli8888/Circadian_Rhythm", "https://github.com/nli8888/Circadian_Rhythm", target="_blank"),
                             br(),br(),
                             "Additionally, as aforementioned Geissmann", tags$i("et al."), "(2017) from the Gilestro Laboratory have developed their own equipment for studying Drosophila called “Ethoscopes” and as such, “rethomics” can already import raw ethoscope data directly.", 
                             actionLink("ref8.1", tags$sup("[8]")),
                             "Ethoscope machines are designed for higher throughput analysis of animal activity compared to DAM", tags$b("(Fig 3)"), ".",  HTML('</p>')
                             )),
             tags$head(tags$style(
               type="text/css",
               "#image4 img {max-width: 100%; width: 100%; height: auto}"
             )),
             fluidRow(br(),
                      column(6, offset = 3,
                             column(8, offset = 2, 
                                    HTML('<a href="ethoscope1.png" target="_blank">'),
                                    imageOutput("image4", height = "100%"),
                                    HTML('</a>'))
                             )
                      ),
             fluidRow(br(),
                      column(6, offset = 3,
                             wellPanel(HTML('<p style="font-size:18px">'),
                               tags$b("Figure 3. a)"), 
                                       "Blown-up view of a prototypical ethoscope. The upper case contains a Raspberry Pi and its HD camera which tracks and records animal behaviours in the arena. The lower case contains infrared lights and support for the experimental arena. Custom arena designs can be made as long as they fit the required dimensions, which allow for flexible experimental design. For", 
                                       tags$i("Drosophila,"), "the arena is typically designed to equip the same transparent tubes used in DAM, with all tubes laid out horizontally to allow full video tracking.",
                                       tags$b("b)"), "Rendered model of fully assembled product with actual size dimensions shown. Guide slits allow the arena to slide and lock into position.",
                                       "Credit: Geissmann", tags$i("et al."), "(2017).", actionLink("ref8.2", tags$sup("[8]")), "Click image to enlarge.", HTML('</p>')
                                       )
                             )),
      fluidRow(column(6, offset = 3, HTML('<p style="font-size:18px">'),
                      "Ethoscopes offer more dynamical experimentation and does not have the same limitations as DAM. While DAM can only measure the number of times",
                      tags$i("Drosophila,"), "has crossed the infrared beam, ethoscopes can record all activity using its HD camera and video tracking software. Therefore, behaviours that would otherwise not be captured by DAM, such as micro-movements (grooming, eating etc.), can be observed and distinguished from sleeping or walking.",
                      br(),br(), "Below is a short video demonstrating ethoscopes; for more details and tutorials on ethoscopes, please visit:", tags$a(href="http://gilestrolab.github.io/ethoscope/", "http://gilestrolab.github.io/ethoscope/", target="_blank"),
                      br(),br(), HTML('</p>')
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
navbarMenu(title="Visual Analysis",
    tabPanel("Actograms",
             fluidRow(
               column(6, offset = 3,
                      h2("Analysing circadian rhythms using Actograms"), hr(),
                      HTML('<p style="font-size:18px">'),
                      "Now that raw data can be read and imported, it can be processed and analyzed. Some questions that are asked are:", HTML('</p>'),
                      tags$ul(
                        tags$li(HTML('<p style="font-size:18px">'),"if the data appears rhythmic, is it circadian?",HTML('</p>')),
                        tags$li(HTML('<p style="font-size:18px">'),"what is the period of the rhythmicity?",HTML('</p>')),
                        tags$li(HTML('<p style="font-size:18px">'),"what is the power of the period?",HTML('</p>'))
                      ), HTML('<p style="font-size:18px">'),
                      "A procress has a circadian rhythm if it has an entrainable endogenous rhythmicity with a period of approximately 24 hours. There does not seem to be an absolute range for how close to 24 hours the period needs to be to be deemed circadian in the field.", 
                      "As with most interpretations of biological data there does appear to be an aspect of arbitary and subjectiveness, and therefore analyzing the data is non-trivial.",
                      br(),br(),
                      "One of the most common ways of analyzing circadian rhythms is through graphical representation and visual inspection of the data via an actogram",
                      "(", tags$b("Fig. 4"), ").", actionLink("ref10", tags$sup("[10]")),
                      "Actograms plot time on the x-axis and the variable of interest, such as activity (e.g. the number of times", 
                      tags$i("Drosophila"), "crossed the beam), on the y-axis. The time component of the data is also vertically split into separate days, with each sucessive day following the preceding one a line below. Therefore the range of the x-axis is typically limited to [0, 24] hours.",
                      br(),br(),
                      "As light is a common", tags$i("zeitgeber"), "it is usually experimented with and therefore annotated on actograms as well. Experiments are run under different conditions of light refer to as:", actionLink("ref11", tags$sup("[11]")), HTML('</p>'), 
                      tags$ul(
                        tags$li(HTML('<p style="font-size:18px">'), "LL – constant continuous illumination;", HTML('</p>')),
                        tags$li(HTML('<p style="font-size:18px">'),"DD – constant continuous darkness;", HTML('</p>')),
                        tags$li(HTML('<p style="font-size:18px">'),"LD – a regular alternation of light and darkness at a constant interval throughout each day, typically 12 hours of light then 12 hours of darkness;", HTML('</p>'))
                      ), 
               HTML('<p style="font-size:18px">'),
               "On actograms, durations of DD and LD are commonly annotated via shading in of the defined region to indicate that period of data being in (alternating) darkness.", 
               br(), br(),
               "Furthermore, actograms may be multi-plotted where the x-axis is extended by a factor and the data duplicated accordingly. For example, a double-plotted actogram will have a x-axis of 48 hours instead of 24, and plot 2 days worth of data on each horizontal line instead of one.", 
               "There will be a duplicated redundancy of data where the data of the “second day” will be plotted on the second half of each line, as well as the first half of each following line, and so on.",
               actionLink("ref12", tags$sup("[12]")),
               HTML('</p>')
               )),
             tags$head(tags$style(
               type="text/css",
               "#image5 img {max-width: 100%; width: 100%; height: auto}"
             )),
             fluidRow(
               column(6, offset = 3, 
                             column(6, HTML('<a href="f1_actogram.png" target="_blank">'),
                                    imageOutput("image5", height = "auto"), HTML('</a>')),
                      column(6, wellPanel(HTML('<p style="font-size:18px">'), 
                                          tags$b("Figure 4."),
                                          "Double-plotted actogram of wild-type mouse locomotor activity entrained under a 12:12 LD (12 hour light, 12 hour dark) cycle for 7 days followed by 20 days of DD (constant darkness). The bar on top represents the LD lighting schedule in Zeitgeber time (ZT). White boxes indicate light, dark boxes indicate darkness. Credit: Yang",
                                          tags$i("et al."), "(2012).",
                                          actionLink("ref13", tags$sup("[13]")),  "Click image to enlarge.",
                                          HTML('</p>')))
                      )),
             fluidRow(
               column(6, offset = 3, br(),
                      HTML('<p style="font-size:18px">'),
                      
                      "This is helpful in visualising non-24 hour rhythms; manual analysis of the vertical alignment of data provides information about the periodicity.",
                      "Vertically straight alignments suggests a 24 hour period, while drifts to the left indicate a cycle shorter than 24 hours and drifts to the right indicate a cycle longer than 24 hours.",
                      actionLink("ref10.1", tags$sup("[10]")), 
                      br(),br(),
                      "This is shown in", tags$b("Fig. 5a"), ", where once in DD, the", tags$i("shaggy (sgg)"), "gene knock-down mutant", tags$i("Drosophila"), "experiences an endogenous period of approximately 25 hours instead of 24.", tags$b("Fig. 5b"),
                      "The rhythm of the wild-type ", tags$i("Drosophila"), "in DD can be seen to be less regular and in fact drifting away from 24 hours but not to the same extent as the mutant.",
                      tags$i("Sgg"), "is a protein kinase and together with other kinases regulates the localisation and stability of core clock proteins PERIOD (PER) and TIMELESS (TIM). Knock-downs causes deregulation of the entire circadian network.",
                      actionLink("ref3.2", tags$sup("[3]")),
                      HTML('</p>')
                      )
             ),
             
             tags$head(tags$style(
               type="text/css",
               "#image6 img {max-width: 100%; width: 100%; height: auto}"
             )),
             tags$head(tags$style(
               type="text/css",
               "#image7 img {max-width: 100%; width: 100%; height: auto}"
             )),
             fluidRow(br(),
                      column(8, offset = 2,
                      column(6, 
                             HTML('<p style="font-size:18px">'), tags$b("a)"), HTML('</p>'),
                             # br(),
                             HTML('<a href="tim_sggRNAi_analyzed.png" target="_blank">'), 
                             imageOutput("image6", height = "auto"), HTML('</a>')),
                      column(6,
                             HTML('<p style="font-size:18px">'), tags$b("b)"), HTML('</p>'),
                             # br(),
                             HTML('<a href="tim_WT_analyzed.png" target="_blank">'), 
                             imageOutput("image7", height = "auto"), HTML('</a>')
                             
                             )
                      )),
             fluidRow(br(),
                      column(6, offset = 3,
                             wellPanel(
                               HTML('<p style="font-size:18px">'),
                               tags$b("Figure 5. a)"), "Double-plotted actogram of ", tags$i("shaggy (sgg)"), "gene knock-down mutant", tags$i("Drosophila"),
                               "DAM1 activity data plotted using", code("actoplot()"), "(see next page).",
                               "Knock-down was achieved using RNAi of", tags$i("sgg"), "with a UAS-", tags$i("sgg"), tags$sup("RNAi"), "and", tags$i("timeless (tim)"), "gene-GAL4 system.",
                               "Entrained under 12:12 LD cycle for 4 days followed by 14 days of DD.", 
                               tags$b("b)"), "Double-plotted actogram of wild-type", tags$i("Drosophila"), "DAM1 activity data. Entrained under 12:12 LD cycle for 3 days followed by 16 days of DD.", 
                               "All data acquired from Beckwith and Ceriani (2015)", actionLink("ref3.3", tags$sup("[3]")),
                               "Additional annotations of manual analysis of periodicity shown for LD and DD phases.", 
                               "Click image to enlarge.",
                               HTML('</p>')
                               ))
               
             ),
             fluidRow(
               column(6, offset = 3,
                      HTML('<p style="font-size:18px">'),
                      "Actograms have been traditionally used to plot animal activity data, but can be theoretically used on any other time-series data.",
                      HTML('</p>')
                      )
             ),
             fluidRow(
               column(2, offset = 7,
                      actionButton('actogramprevpagebutton', 'Previous Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');"),
                      actionButton('actogramnextpagebutton', 'Next Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');")
               )
             )
             ),
    tabPanel("Actoplot",
             fluidRow(column(6, offset = 3,
                             h2("Actoplot"), hr(),
                             HTML('<p style="font-size:18px">'),
                             "Standalone programs that plot actograms, such as", tags$a(href="http://actogramj.neurofly.de/", "Actogram J", target="_blank"), "and", tags$a(href="http://actimetrics.com/products/clocklab/", "ClockLab", target="_blank"),
                             " already exist but they are not easily accessible nor intuitive to import and process raw data. Nor are they compatible with any programming language. As part of the package, a custom function", code("actoplot()"), "was developed for DAM1, DAM2, and ethoscope data.",
                             "Actoplot is built using",  tags$a(href="https://cran.r-project.org/web/packages/ggplot2/index.html", "ggplot2", target="_blank"), "and is therefore completely modular and compatible with any ggplot2 arguments and functions.",
                             br(),br(),
                             code("actoplot()"), "can plot DAM1, DAM2 and ethoscope data that are stored as", code("data.tables()"), 
                             "and has 4 different plot types (bar, line, ribbon, and tile) for the user to choose from", tags$b("(Fig. 6)"), ". For details on these plots, refer to the", tags$a(href="http://ggplot2.tidyverse.org/reference/", "ggplot2 documentation.", target="_blank"),
                             "The user can define any arbitary number of duplicated days for multiplots as long as there is sufficient data.",
                             HTML('</p>')
                             # actionLink("actoexample", "Back to Actograms", onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');")
             )),
             fluidRow(br(),
                      column(6, offset = 3,
                             wellPanel(
                               HTML('<p style="font-size:18px">'),
                               tags$b("Figure 6."), "Example", tags$b("a)"), "bar, and", tags$b("b)"), "line double-plotted actograms, and", tags$b("c)"), "ribbon, and", tags$b("d)"), "tile quadruple-plotted actograms created from",
                               code("actoplot()"), "Data from Beckwith and Ceriani (2015)", actionLink("ref3.4", tags$sup("[3]")), "was used. Also shown in", tags$b("e)"), "is  population ethoscope data with all individuals shown separately on the same plot, generated from pre-packaged data in “rethomics”.",
                               HTML('</p>')
                             ))
                      
             ),
             tags$head(tags$style(
               type="text/css",
               "#image8 img {max-width: 100%; width: 100%; height: auto}"
             )),
             tags$head(tags$style(
               type="text/css",
               "#image9 img {max-width: 100%; width: 100%; height: auto}"
             )),
             tags$head(tags$style(
               type="text/css",
               "#image10 img {max-width: 100%; width: 100%; height: auto}"
             )),
             tags$head(tags$style(
               type="text/css",
               "#image11 img {max-width: 100%; width: 100%; height: auto}"
             )),
             tags$head(tags$style(
               type="text/css",
               "#image12 img {max-width: 100%; width: 100%; height: auto}"
             )),
             fluidRow(br(),
                      column(6, offset = 3,
                             column(6, 
                                    HTML('<p style="font-size:18px">'), tags$b("a)"), HTML('</p>'),
                                    # br(),
                                    HTML('<a href="Bar_plot_example.jpeg" target="_blank">'), 
                                    imageOutput("image8", height = "auto"), HTML('</a>')),
                             column(6,
                                    HTML('<p style="font-size:18px">'), tags$b("b)"), HTML('</p>'),
                                    # br(),
                                    HTML('<a href="Line_plot_example.jpeg" target="_blank">'), 
                                    imageOutput("image9", height = "auto"), HTML('</a>')
                                    
                             )
                      )),
             fluidRow(br(),
                      column(6, offset = 3,
                             column(6, 
                                    HTML('<p style="font-size:18px">'), tags$b("c)"), HTML('</p>'),
                                    # br(),
                                    HTML('<a href="Ribbon_plot_example.jpeg" target="_blank">'), 
                                    imageOutput("image10", height = "auto"), HTML('</a>')),
                             column(6,
                                    HTML('<p style="font-size:18px">'), tags$b("d)"), HTML('</p>'),
                                    # br(),
                                    HTML('<a href="Tile_plot_example.jpeg" target="_blank">'), 
                                    imageOutput("image11", height = "auto"), HTML('</a>')
                                    
                             )
                      )),
             fluidRow(br(),
                      column(6, offset = 3,
                             column(6, 
                                    HTML('<p style="font-size:18px">'), tags$b("e)"), HTML('</p>'),
                                    # br(),
                                    HTML('<a href="Ethoscope_plot_example.jpeg" target="_blank">'), 
                                    imageOutput("image12", height = "auto"), HTML('</a>'))
                      )),
             fluidRow(br(),
                      column(6, offset = 3,
                             HTML('<p style="font-size:18px">'),
                             code("actoplot()"), "can handle individual and population data and can utilize existing functions, such as", code("mean()"), 
                             "as arguments to summarise data points between desired intervals before plotting. Alternatively, for population data the user can choose to visualise all individual data from the population without summarising on one plot", 
                             tags$b("(Fig. 6e)"), "It also has built-in arguments for LD and DD annotation as seen previously in", tags$b("Fig. 5"),
                             br(),br(),
                             "See next page for demonstration.",
                             br(),br(),
                             "For more documentation and source code visit", tags$a(href="https://github.com/nli8888/Circadian_Rhythm", "https://github.com/nli8888/Circadian_Rhythm", target="_blank"),
                             HTML('</p>')
                             )),
             fluidRow(
               column(2, offset = 7,
                      actionButton('actoplotprevpagebutton', 'Previous Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');"),
                      actionButton('actoplotnextpagebutton', 'Next Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');")
               )
             )
             ),
    tabPanel("Demo",
             
             fluidRow(
               column(6, offset = 3,
                      h5("Below is a GUI for the function", code("actoplot()"), "with limited optional arguements available purely for demonstration. The full function is designed to be used in code and is more flexible.", br(), "Please be patient as it may take time to load data."))
             ),
             hr(),
             sidebarLayout(sidebarPanel(
             selectInput("dataset", label = h3("1) Select example data to load"), 
                         choices = c("dam1 individual", "dam1 population", "dam2", "ethoscope"
                                     # , "dam1ex2"
                                     )
                         #width = "20%"
                         ),
             
             actionButton('loaddata', 'Load data'),
             h5(p("After selecting please click load data.")),
             h5(textOutput("ex1text")),
             h2(uiOutput("ex2text"))
             
             ),
             mainPanel(dataTableOutput('ex1'))
             ),
             # hr(),
             # fluidRow(
             #   column(6, offset = 3,
             #        
             #          dataTableOutput('ex1')
             #          
             #          )
             # ),
             # dataTableOutput('ex1'),
             # br(), br(), br(),
             
             uiOutput("hline"),
             sidebarLayout(uiOutput("sidePanel"),
                           mainPanel(plotOutput("actogram", width = "100%", height = "800px"))
             ),
             # fluidRow(
             #   column(8, offset = 2,
             #          plotOutput("actogram", width = "100%", height = "700")
             #   )),
             # fluidRow(column(2, offset = 7, HTML("<a href='#top'>top of page</a>")))
             fluidRow(
               column(2, offset = 7,
                      actionButton('demoprevpagebutton', 'Previous Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');"),
                      actionButton('demonextpagebutton', 'Next Page', onclick="$('html, body').animate({ scrollTop: 0 }, 'fast');")
               )
             )
             )
    ),
    navbarMenu(title = "Quantitative Analysis",
    tabPanel("Autocorrelation",
             withMathJax(),
             fluidRow(
               column(6, offset = 3, 
                      h2("Detection of Rhythmicity and Periodicity using Autocorrelation"), hr(),
                      # "Mathematically it is defined as:",
                      # helpText("$$\\large{r=\\frac{\\sum_\\limits{t=1}^{N-1} (x_t-\\bar x_{(1)})(x_{t+1}-\\bar x_{(2)})}{\\sqrt{\\sum_\\limits{t=1}^{N-1} (x_t-\\bar x_{(1)})^2 \\sum_\\limits{t=1}^{N-1} (x_{t+1}-\\bar x_{(2)})^2}}}$$"),
                      # "where", 
                      # helpText("$$\\large{\\bar x_{(1)} = \\sum_\\limits{t=1}^{N-1} x_t/(N-1)}$$"),
                      # "is the mean of of the first set of observation in each of the (N-1) pairs and", HTML("<span style='font-size:130%'>x&#772<sub>(2)</sub></span>"), "is the mean of the second set.", "Without showing all the steps, Chatifield (2003) demonstrates that the equation above can be conveniently made less complicated by approximating to",
                      # helpText("$$\\large{r=\\frac{\\sum_\\limits{t=1}^{N-1} (x_t-\\bar x)(x_{t+1}-\\bar x)}{\\sum_\\limits{t=1}^{N} (x_t - \\bar x)^2}}$$"),
                      # "as", HTML("<span style='font-size:130%'>x&#772<sub>(1)</sub> &#8776 x&#772<sub>(2)</sub></span>"), ", and by dropping the factor N/(N-1) since it is close to 1 for large N.",
                      HTML('<p style="font-size:18px">'),
                      "An actogram is useful for visual analysis, but is often subjective and therefore more quantitative methods are needed for decisive conclusions. As the data is a time-series, one way is though autocorrelation, where the correlation coefficient",
                      tags$i("(r)"), "of the data set compared to itself is calculated via standard correlation analysis, point by point, from beginning to end. The data set is then lagged by one time interval and compared to the original data set again.",
                      "Given", tags$i("N"), "data points", HTML('(<em>x</em><sub>1</sub>,...,<em>x</em><sub><em>N</em></sub>)'), "there will be", HTML("<em>N</em>-1"), "pairs of observations,", 
                      HTML('(<em>x</em><sub>1</sub>,<em>x</em><sub>2</sub>),'), HTML('(<em>x</em><sub>2</sub>,<em>x</em><sub>3</sub>),'), "...,", HTML('(<em>x</em><sub><em>N</em>-1</sub>,<em>x</em><sub><em>N</em></sub>).'),
                      br(),br(),
                      "The process is repeated with each repeat using the original data and data that is further lagged by one interval. However, as with each successive lag a pair of observations is removed, meaning that the power of the test gradually decreases, autocorrelation is usually performed up to a limit of", 
                      HTML("<em>N</em>/3."), actionLink("ref14", tags$sup("[14]")),
                      br(),br(),
                      "The autocorrelation coefficient at lag", tags$i("k"), "is defined as:",
                      HTML('</p>'),
                      helpText("$$\\large{r_k = \\frac{\\sum_\\limits{t=1}^{N-k} (x_t-\\bar x)(x_{t+k}-\\bar x)}{\\sum_\\limits{t=1}^{N} (x_t-\\bar x)^2}}$$"),
                      HTML('<p style="font-size:18px">'),
                      "where,", HTML('</p>'),
                      helpText("$$\\large{\\bar x = \\sum_\\limits{t=1}^{N} x_t/N}$$"),
                      HTML('<p style="font-size:18px">'),
                      "is the overall mean.", br(),br(),
                      tags$i("r"),"will be 1 at the start due to perfect correlation of data against self but will then decrease as the data becomes out of register with itself as lag increases.", 
                      "However, if there is a regular rhythmicity then the peaks and troughs in the amplitude of the data will slip back into register and r will increase again when the lag approximates the period of the data.", actionLink("ref15", tags$sup("[15]")),
                      br(),br(),
                      "Plotting the correlation coefficients against lag in a correlogram can identify rhythmicity and the period. Rhythmic data will create sinusoidal oscillations on the correlogram with decreasing amplitude as lag increases",
                      tags$b("(Fig 7a and 7b)"), 
                      ", and any local maxima peaks above the chosen confidence interval can be considered as statistically significant multiples of the period.", 
                      actionLink("ref14.1", tags$sup("[14;")), actionLink("ref15.1", tags$sup("15]")),
                      "Chatfield (2016) describes the 95% confidence interval as", HTML('&plusmn;2/&radic;<em>N</em>'),
                      br(),br(),
                      "In", tags$b("Fig. 7c"), "statistically significant peaks appear at 12, 24, and 36, suggesting a circadian period of 24 hours, while a local maxima peak at 25 hours is seen in", tags$b("Fig. 7d"), 
                      "agreeing with the actogram analysis in", tags$b("Fig. 5")
                      HTML('</p>')
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
                      "[9] Rosato, E. & Kyriacou, C. P. (2006) Analysis of locomotor activity rhythms in Drosophila. Nature Protocols. 1 (2), 559-568.",
                      p(),
                      "[10] Refinetti, R., Cornélissen, G. & Halberg, F. (2007) Procedures for numerical analysis of circadian rhythms. Biological Rhythm Research. 38 (4), 275-325.",
                      p(),
                      "[11] Refinetti, R. (2016) Circadian physiology. , CRC press.",
                      p(),
                      "[12] Verwey, M., Robinson, B. & Amir, S. (2013) Recording and analysis of circadian rhythms in running-wheel activity in rodents. JoVE (Journal of Visualized Experiments). (71), e50186-e50186.",
                      p(),
                      "[13] Yang, Y., Duguay, D., Bedard, N., Rachalski, A., Baquiran, G., Na, C. H., Fahrenkrug, J., Storch, K. F., Peng, J., Wing, S. S. & Cermakian, N. (2012) Regulation of behavioral circadian rhythms and clock protein PER1 by the deubiquitinating enzyme USP2. Biology Open. 1 (8), 789-801.",
                      p(),
                      "[14] Chatfield, C. (2016) The analysis of time series: an introduction. , CRC press.",
                      p(),
                      "[15] Levine, J. D., Funes, P., Dowse, H. B. & Hall, J. C. (2002) Signal analysis of behavioral and molecular cycles. BMC Neuroscience. 3 (1), 1."
                      )
             )),
tags$script(" $(document).ready(function () {
         $('#inTabset a[data-toggle=\"tab\"]').bind('click', function (e) {
            $(document).load().scrollTop(0);
            });
            
            });"),
tags$head(tags$style(HTML('.modal-lg {width: 1000px;}')))
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
  
  output$image5 <- renderImage({
    return(list(
      src = "www/f1_actogram.png",
      contentType = "image/png"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$image6 <- renderImage({
    return(list(
      src = "www/tim_sggRNAi_analyzed.png",
      contentType = "image/png"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$image7 <- renderImage({
    return(list(
      src = "www/tim_WT_analyzed.png",
      contentType = "image/png"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$image8 <- renderImage({
    return(list(
      src = "www/Bar_plot_example.jpeg",
      contentType = "image/jpeg"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$image9 <- renderImage({
    return(list(
      src = "www/Line_plot_example.jpeg",
      contentType = "image/jpeg"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$image10 <- renderImage({
    return(list(
      src = "www/Ribbon_plot_example.jpeg",
      contentType = "image/jpeg"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$image11 <- renderImage({
    return(list(
      src = "www/Tile_plot_example.jpeg",
      contentType = "image/jpeg"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$image12 <- renderImage({
    return(list(
      src = "www/Ethoscope_plot_example.jpeg",
      contentType = "image/jpeg"
      # ,
      # width = "500px"
    ))
  }, deleteFile = FALSE)
  
  output$video <- renderUI({
    tags$video(src="https://www.youtube.com/embed/5oWGBUMJON8", type = "video/mp4", autoplay = NA, controls = NA)
  })
  
  observeEvent(input$EquimentAcquisitionTab, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Data Acquistion")
  })
  
  observeEvent(input$intronextpagelink, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Data Acquisition")
    #HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$intronextpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Data Acquisition")
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
  
  observeEvent(input$actogramprevpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Data Acquisition")
  })
  
  observeEvent(input$actogramnextpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Actoplot")
  })
  
  observeEvent(input$actoplotprevpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Actograms")
  })
  
  observeEvent(input$actoplotnextpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Demo")
  })
  
  observeEvent(input$demoprevpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Actoplot")
  })
  
  observeEvent(input$demonextpagebutton, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Autocorrelation")
  })
  
  observeEvent(input$homenextpage, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Introduction")
  })
  
  observeEvent(input$actoexample, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Actograms")
  })
  
  observeEvent(input$loaddata, {
    withProgress(message = "loading", value = 0,{
    dam1ex1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C03.txt")
    # dam1ex2 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Estaban_new_data/Circadian_data_for_Nicholas/220914es5/220914es5CtM011C27.txt")
      # dam1ex1 = DAM1_single_reader("./www/DAM1_data/220714esM035C01.txt")
      # dam1ex2 = DAM1_single_reader("./www/DAM1_data/220714esM035C02.txt")
    
      
    output$sidePanel <- renderUI(sidebarPanel(h3("2) Plot graph"), h5("Select parameters and then click 'Plot graph' at the bottom. The plot will appear on the right."),
                                              textInput("plot_title", label = tags$b("Title of graph"), value = "Actogram plotted using actoplot()"),
                                              textInput("plot_xlab", label = tags$b("x-axis label"), value = "time (hours)"),
                                              textInput("plot_ylab", label = tags$b("y-axis label"), value = "activity"),
                                              
      fluidRow(
        column(11, 
               sliderInput("dup_num", label = h4("Number of duplicated days:"),
                  min = 1, max = 10, value = 2)), column(1, actionButton("help_dup_num", "?"))
        ), 
      
      fluidRow(
        column(11,
      selectInput("operation", label = h4("Operation to perform:"),
                  choices = list("mean" = "mean", "median" = "median", "sum" = "sum"), selected = "mean")),column(1, actionButton("help_operation", "?"))
      ),
      
      conditionalPanel("input.dataset == 'dam1 population'||input.dataset == 'ethoscope'",
                       fluidRow(
                         column(11,
                       selectInput("pop_overview", label = h4("Additional summary opperation to perform on population data:"),
                                   choices = list("mean" = "mean", "median" = "median", "sum" = "sum", "NULL" = "NULL"), selected = "mean")),
                       column(1, actionButton("help_pop_overview", "?"))
                       )
                       ),
      
      conditionalPanel("input.pop_overview == 'NULL'", 
                       HTML('<p style="font-size:18px">'),
                       "When 'summary operation' is 'NULL' only line plot formats are allowed currently.",
                       HTML('</p>')
      ),
      
      conditionalPanel("input.dataset != 'ethoscope'&&input.pop_overview != 'NULL'",
                       fluidRow(
                         column(11,
                       selectInput("plot_type", label = h4("Type of plot:"),
                                   choices = list("bar" = "bar", "line" = "line", "ribbon" = "ribbon", "tile" = "tile"), selected = "bar")),
                       column(1, actionButton("help_plot_type", "?"))
                       )
                       ),
      
      conditionalPanel("input.dataset == 'ethoscope'",
                       fluidRow(
                         column(11,
                       selectInput("condition", label = h4("Y axis values to plot:"),
                                   choices = list(moving = "moving", asleep = "asleep", max_velocity = "max_velocity", is_interpolated = "is_interpolated"), selected = "moving")),
                       column(1, actionButton("help_condition", "?"))
                       )
                       ),
      
      conditionalPanel("input.plot_type == 'bar'&&input.pop_overview != 'NULL'",
                       fluidRow(column(11, h4("DD day range:")), column(1, actionButton("help_DD", "?"))),
                       p("(e.g. From 4 to 8)"),
                       #br(),
                       column(6, selectInput("DD_start", label = "From", selected = NULL, choices = list(NULL = "none",0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))),
                       column(6, selectInput("DD_end", label = "to", selected = NULL, choices = list(NULL = "none",0,1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))),
                       fluidRow(column(11, h4("LD day range:")), column(1, actionButton("help_LD", "?"))),
                       p("(e.g. From 0 to 3)"),
                       #br(),
                       column(6, selectInput("LD_start", label = "From", selected = NULL, choices = list(NULL = "none",0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))),
                       column(6, selectInput("LD_end", label = "to", selected = NULL, choices = list(NULL = "none",0,1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))),
                       fluidRow(column(11, h4("Time when lights turn on and off:")),  column(1, actionButton("help_LD_time", "?"))),
                       p("(e.g. Darkness starts 0; Darkness ends, Light starts 12; Light ends 24)"),
                       #br(),
                       fluidRow(
                       column(4, br(), numericInput("D_start", label = "Darkness starts", value = 0, min = 0, max = 100)),
                       column(4, numericInput("D_end_L_start", label = "Darkness ends, Light starts", value = 12, min = 0, max = 100)),
                       column(4, br(), numericInput("L_ends", label = "Light ends", value = 24, min = 0, max = 100))),
                       fluidRow(column(4, numericInput("LD_offset", label = "Offset LD", value = 0, min = -100, max = 100)))
                       ),
      conditionalPanel("input.plot_type == 'line'||input.plot_type == 'ribbon'||input.plot_type == 'tile'", 
                       HTML('<p style="font-size:18px">'),
                       "DD and LD options only available for bar plots currently",
                       HTML('</p>')
                       ),
      hr(),
      actionButton(inputId = "go",
                   label = "Plot graph")
    ))
    
    data <- eventReactive(input$go, {withProgress(message = "loading", value = 0,{
      actoplot(isolate(datasetInput()),
                                                   num_of_plot = input$dup_num,
                                                   plot_title = input$plot_title,
                                                   plot_xlab = input$plot_xlab,
                                                   plot_ylab = input$plot_ylab,
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
           "dam1 individual" = dam1ex1,
           "dam1ex2" = dam1ex2,
           "dam1 population" = dammulti1,
           "dam2" = dam2,
           "ethoscope" = sleep_sexual_dimorphism)
      })
    datasetFile_format <- reactive({
      switch(input$dataset,
             "dam1 individual" = "dam1",
             "dam1ex2" = "dam1",
             "dam1 population" = "dam1",
             "dam2" = "dam2",
             "ethoscope" = "ethoscope")
    })
    datasetText <- reactive({
      switch(input$dataset,
             "dam1 individual" = "Displayed on the right is raw DAM1 individual data. Increase the number of entries to expand and explore the data if desired.",
             "dam1ex2" = "Displayed below is raw example data from DAM1 machines. Increase the number of entries to expand and explore the data if desired.",
             "dam1 population" = "Displayed on the right is raw DAM1 population data. Increase the number of entries to expand and explore the data if desired.",
             "dam2" = "Displayed on the right is raw DAM2 data. Increase the number of entries to expand and explore the data if desired.",
             "ethoscope" = "Displayed on the right is raw ethoscope data. Increase the number of entries to expand and explore the data if desired.")
      })
    output$hline <- renderUI(hr())
    output$ex1text <- renderText(
      isolate(datasetText())
      )
    output$ex2text <- renderUI(
      HTML('<h5>Plot an actogram of the data using the side panel below with either already selected default options or user chosen ones.</h5>
           
           <h2>&darr;</h2>')
    )
    output$ex1 <- renderDataTable(
        isolate(datasetInput()), options = list(
          lengthMenu = list(c(5, 10, 15, 20), c('5', '10', '15', '20')),
          pageLength = 5,
          orderClasses = TRUE,
          scrollX = TRUE
          ))
    })
  })
  
  observeEvent(input$help_dup_num, {
    showModal(modalDialog(size = "l", easyClose = TRUE,
      title = "Number of duplicated days", 
      HTML('<p style="font-size:18px">'),
      "The number of days the data is to be duplicated by. E.g. for single-plotted actograms (1 day), there is no duplication and only 24 hours of data is shown along the x-axis.", 
      "For double-plotted actograms (2 days), 48 hours worth of data is shown along the x-axis. In", code("actoplot"), "any integer can be used as an argument but here it is limited to 10 days.",
      HTML('</p>')
    ))
  })
  
  observeEvent(input$help_operation, {
    showModal(modalDialog(size = "l", easyClose = TRUE,
                          title = "Operation to perform", 
                          HTML('<p style="font-size:18px">'),
                          "What operation to perform on the raw data.", "Plotting every single value for every single second would not be visually informative. Therefore the data is usually summarised per every hour or user chosen time interval (Not available in thi GUI)",
                          "In", code("actoplot"), "the user can use any existing function such as", code("mean()"), "as the arguement. Here options are limited.",
                          HTML('</p>')
    ))
  })
  
  observeEvent(input$help_pop_overview, {
    showModal(modalDialog(size = "l", easyClose = TRUE,
                          title = "Additional summary opperation to perform on population data", 
                          HTML('<p style="font-size:18px">'),
                          "For population data, further summary statistical operations can be performed on the multiple individuals to summarise the data even further",
                          "Like 'Operation to perform', existing functions can be used as an argument in", code("actoplot()"), ". Here options are also limited.", "NOTE: if 'NULL' is chosen then population data will not be summarised and will all appear separately on the plot. Currently this forces the plot to be in line plot format for practical visibility.",
                          HTML('</p>')
    ))
  })
    
  observeEvent(input$help_plot_type, {
    showModal(modalDialog(size = "l", easyClose = TRUE,
                          title = "Type of plot", 
                          HTML('<p style="font-size:18px">'),
                          "For DAM1 and DAM2 data currently, users can choose the aesthetic format of the plot from: bar; line; ribbon; and tile.",
                          HTML('</p>')
    ))
  })
  
  observeEvent(input$help_condition, {
    showModal(modalDialog(size = "l", easyClose = TRUE,
                          title = "Y axis values to plot", 
                          HTML('<p style="font-size:18px">'),
                          'For ethoscope data, multiple variables are recorded, such as if the animal is "moving" or "asleep" and therefore which variable to be plotted can be chosen.', 
                          "For more information on ethoscopes and its data format please refer to the documentation.",
                          HTML('</p>')
    ))
  })
  
  observeEvent(input$help_DD, {
    showModal(modalDialog(size = "l", easyClose = TRUE,
                          title = "DD day range", 
                          HTML('<p style="font-size:18px">'),
                          "The days during which the experiment was conducted in DD (constant darkness) specified from and to. To select only 1 day then both 'from' and 'to' need to be set to the desired day.",
                          HTML('</p>')
    ))
  })
  
  observeEvent(input$help_LD, {
    showModal(modalDialog(size = "l", easyClose = TRUE,
                          title = "LD day range", 
                          HTML('<p style="font-size:18px">'),
                          "The days during which the experiment was conducted in LD (alternating light and darkness) specified from and to. To select only 1 day then both 'from' and 'to' need to be set to the desired day. NOTE: overlapping DD annotations will overwrite any existing LD annotations underneath.",
                          HTML('</p>')
    ))
  })
  
  observeEvent(input$help_LD_time, {
    showModal(modalDialog(size = "l", easyClose = TRUE,
                          title = "Time when lights turn on and off", 
                          HTML('<p style="font-size:18px">'),
                          "For LD, the time (in hours) of when the darkness alternates to light can be set.", 
                          "It is advise for values to be kept in the order of (Darkness starts) < (Darkness ends, Light starts) < (Light ends) for full functionality.", "Standard 12:12 hour LD is given in the example and as default.",
                          "Offset LD is to shift the annotations left or right and is the easiest method of moving them to the desired time points. E.g. under standard 12:12 LD an 'offset LD' of 12 will fully alternate the light to darkness and vice versa.",
                          HTML('</p>')
    ))
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
  
  observeEvent(input$ref3.2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref3.3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref3.4, {
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
  
  observeEvent(input$ref10, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref10.1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref11, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref12, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref13, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref14, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref14.1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref15, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
  
  observeEvent(input$ref15.1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "References")
    HTML("<a href='#ref1'></a>")
  })
}


shinyApp(ui = ui, server = server)