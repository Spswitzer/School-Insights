# (2) School Culture  Tab ----
uiSchoolculture <- tabPanel(
  value = 'main-Culture',
  title = 'School Culture',
  br(),
  tabBox(
    id = 'climate',
    width = 12,
# Overview ----
    tabPanel(value = 'sub-CultureOverview',
             title = 'Overview',
             fluidRow(
               box(
                 title = " ",
                 background = 'light-blue',
                 width = 12,
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 closable = FALSE,
                 enable_dropdown = FALSE,
                 span(HTML(paste0("<b>Positive school cultures help students learn. Districtwide surveys inform how well students, families, and educators are connected to the school, how safe and welcome they feel at school, and if there are good supports for learning in school.</b><br><br><table><colgroup>
       <col span='1' style = 'width: 33%;'>
       <col span='1' style='width: 66%;'></colgroup>
<td valign='top';>What do the colors mean? </td><td valign='top';><ul> <li>", paste0('<span style=\"color:', '#e2a331', '\ ">',"<b> "), "Yellow", "</b>", '</span>'," = Lowest 25% of all schools </li><li>", paste0('<span style=\"color:', '#515151', '\ ">',"<b> "), "Gray", "</b>", '</span>'," = Middle 50% of all schools </li> <li>", paste0('<span style=\"color:', '#22a783', '\ ">',"<b> "), "Green", "</b>", '</span>',"  = Highest 25% of all schools</li></ul></td></tr><tr><td valign='top'>Why are the same numbers different colors across categories?</td><td valign='top'><ul><li>The low, middle, and high colors are based on the range of answers in each category, so they vary for each item.</ul></li></td></tr></table>")), style = "color:#315683; font-size:14px")
               )  #close box
             ), #close FluidRow
             fluidRow(
               column(
                 width = 4,
## Students ----
                 span("Student Survey",
                      style = cultureHeading),
                 bsPopover(
                   id = 'myvhHelper',
                   title = '',
                   content = 'The Make Your Voice Heard student survey was designed and administered within Jeffco beginning in 2012. Feedback is gathered from students in grades 2 through 12.',
                   placement = "bottom",
                   trigger = "hover",
                   options = NULL
                 ),
                 actionButton(
                   inputId = "myvhHelper",
                   label = "info"
                 ), 
                 br(),
                 span('Make Your Voice Heard',
                      style = cultureSubHeading),
# br(),
# span("",
#      style = "color:#E57A3C; font-size:14px"),

                 br(),
                 fluidRow(
                   column(width = 1),
                   column(width = 8,
                          style = "background-color:#DCDCDC; border: 1px solid #315683;",
                          radioGroupButtons(inputId = 'yearMyvh',
                                            label = '',
                                            choices = c('2022','2021', '2020'),
                                            selected = '2022',
                                            size = 'xs',
                                            status = 'primary',
                                            individual = TRUE,
                                            checkIcon = list(
                                              yes = icon("check-square"),
                                              no = icon("square-o", verify_fa = FALSE)))
                   ), #close column
                   column(width = 1)
                 ), #close column
                 fluidRow(
                   uiOutput("MYVHOverallFavor")
                 ), #close fluidRow
                 fluidRow(
                   gt_output("MYVHTableServer"), 
                   uiOutput('schoolCultureLinkMyvh')
                 ) #close fluidRow
               ), #close column
               column(width = 4,
## Families ----
                      span('Family Survey',
                           style = cultureHeading),
                      bsPopover(
                        id = 'FSPhelper',
                        title = '',
                        content = 'Jeffco Public Schools has been using a family survey to measure the national PTA standards since 2014.',
                        placement = "bottom",
                        trigger = "hover",
                        options = NULL),
                      actionButton(
                        inputId = "FSPhelper",
                        label = "info"),
                      br(),
                      span("Family-School Partnership",
                           style = cultureSubHeading),
# br(),
# span(" ",
#      style = "color:#E57A3C; font-size:14px"),
                      br(),
                      fluidRow(
                        column(width = 1),
                        column(width = 8,
                               style = "background-color:#DCDCDC; border: 1px solid #315683;",
                               radioGroupButtons(inputId = 'fspYear',
                                                 label = '',
                                                 choices = c('2022', '2021', '2020'),
                                                 selected = '2022',
                                                 size = 'xs',
                                                 status = 'primary',
                                                 individual = TRUE,
                                                 checkIcon = list(
                                                   yes = icon("check-square"),
                                                   no = icon("square-o", verify_fa = FALSE)
                                                 ))),
                        column(width = 1)
                      ), #close fluidRow
                      fluidRow(
                        uiOutput("FSPOverallFavor")
                      ), #close fluidRow
                      fluidRow(
                        gt_output("OverallConstructsTableServer"), 
                        uiOutput('schoolCultureLinkFsp')
                      ) #close fluidRow
               ), #close column
               column(
                 width = 4,
## Educators ----
                 span("Educator Survey",
                      style = cultureHeading),
                 bsPopover(
                   id = 'TLCChelper',
                   title = '',
                   content = 'The educator survey was designed and administered statewide beginning in 2018.',
                   placement = "bottom",
                   trigger = "hover",
                   options = NULL),
                 actionButton(
                   inputId = "TLCChelper",
                   label = "info"),
                 br(),
                 span("Teaching & Learning in Colorado",
                      style = cultureSubHeading),
# br(),
# span("2022 results available in late May",
#      style = "color:#E57A3C; font-size:14px"),
                 br(),
                 fluidRow(
                   column(width = 1),
                   column(width = 8,
                          style = "background-color:#DCDCDC; border: 1px solid #315683;",
                          radioGroupButtons(inputId = 'yearTlcc',
                                            label = ' ',
                                            choices = c('2022', '2021', '2020'),
                                            selected = '2022',
                                            size = 'xs',
                                            status = 'primary',
                                            individual = TRUE,
                                            checkIcon = list(
                                              yes = icon("check-square"),
                                              no = icon("square-o", verify_fa = FALSE)
                                            ))),
                   column(width = 1)
                 ), #close fluidRow
                 fluidRow(
                   uiOutput("TLCCOverallFavor")
                 ), #close fluidRow
                 fluidRow(
                   gt_output("TLCCTableServer"),
                   uiOutput('schoolCultureLinkTlcc')
                 ) #close fluidRow
               ) # close column
             ),  #close fluidRow
br(),
box(
  title = "Visit our School Culture Website",
  background = 'light-blue',
  width = 12,
  status = "primary",
  solidHeader = TRUE,
  collapsible = FALSE,
  closable = FALSE,
  enable_dropdown = FALSE,
  
  footer = fluidRow(
    
    column(width = 10, 
           span(HTML("<b>Click here to explore additional survey details, including the results of each survey question</b>"), style = "color:#315683; font-size:14px"),
           uiOutput('schoolCultureHelper')
    ),   
    # column(width = 4, 
    #             span(HTML(paste0("<b>Access the specific questions for each survey on the</b><br><a href = https://www.jeffcopublicschools.org/community_portal/research_surveys/annual_surveys target = blank>  District Survey Website</a>")), style = "color:#315683; font-size:14px")
    # )
  )
) #close box
    ), #close tabPanel Overview
# Trends ----
    tabPanel(value = 'sub-CultureTrends',
             title = 'Trends',
## Student Trends ----
             fluidRow(
               column(
                 width = 4,
                 span("Student Survey", style = cultureHeading),
                 br(),
                 span("Make Your Voice Heard", style = cultureSubHeading),
                 br(),
                 span('% responding favorably', style = "color:#315683; font-size:14px"),
                 plotOutput('myvhTrendPlot', height = 200)
               ), #close column
## Family Trends ----
               column(
                 width = 4,
                 span("Family Survey", style = cultureHeading),
                 br(),
                 span("Family-School Partnership", style = cultureSubHeading),
                 br(),
                 span("% responding favorably", style = "color:#315683; font-size:14px"),
                 plotOutput('fspTrendPlot', height = 200)
               ), #close column
## Educator Trends ----
               column(
                 width = 4,
                 span("Educator Survey", style = cultureHeading),
                 br(),
                 span("Teaching & Learning in CO", style = cultureSubHeading),
                 br(),
                 span("% responding favorably", style = "color:#315683; font-size:14px"),
                 plotOutput('tlccTrendPlot', height = 200)
               ) #close column
             ), #close fluidRow
             fluidRow(
               column(width = 8),
               column(width = 4,
                      span(HTML('In 2021, data were collected by and provided by the University of Colorado Denver. Results include feedback solely from Educators.<br> In 2020 and 2022, data were collected by and provided by the Colorado Department of Education. Results include feedback from employees in multiple positions: Educators, Education Professionals or Service Providers, and School Leaders.'), style = "color:#lightgrey;font-size:75%")
               ) #close column
             ), #close column
             fluidRow(
               column(width = 3),
               column(width = 6,
                      span(HTML('Sites with no data indicates low response rates'))
               ) #close column
             ) # close fluidRow
# fluidRow(
#   uiOutput("cultureFrame")
# ) #close fluidRow
    ) #close tabPanel
  ) #close tabBox
) #close tabPanel
