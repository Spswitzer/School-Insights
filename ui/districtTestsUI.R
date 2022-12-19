# (4) Academic Growth & Achievement Jeffco Tab ----
uiDistrictTests <-      tabPanel(
  value = 'main-DistrictTests',
  title = 'District Tests',
  br(),
# MAP Subtab ----
  tabBox(
    id = 'districtTestsTabBox',
    width = 12,
    tabPanel(value = 'sub-DistrictTestsMap',
             title = 'Measures of Academic Progress (MAP)',
             fluidRow(
               box(
                 title = "Introduction",
                 closable = FALSE,
                 background = 'light-blue',
                 width = 12,
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 enable_dropdown = TRUE,
                 dropdown_icon = " ",
                 span(HTML("Jeffco Public Schools uses a districtwide test called MAP (<b>M</b>easures of <b>A</b>cademic <b>P</b>rogress) to  understand how students are learning at multiple points throughout the school year. MAP gives students feedback on what they have learned and what they still need to learn in Reading and Math. Unlike state tests like CMAS or SAT, MAP results are meant to be a continual guide for teaching and learning instead of an end-of-year evaluation of what has been learned. MAP scores help teachers check student performance by measuring achievement and growth. Teachers use MAP results to adjust instruction and learning goals for students. <br> MAP results will be provided following district administrations in October, January, and June annually."), style = "color:#315683; font-size:14px")
               )#close box
             ), #close fluidRow
             fluidRow(
               column(
                 width = 12,
                 span("MAP", style = "color:#315683; font-weight:bold; font-size:16px"),
                 span(HTML("<b>M</b>easures of <b>A</b>cademic <b>P</b>rogress: 2022-2023"), 
                      style = "color:#315683;font-size:100%")
               ) #close column
             ) ,#close fluidRow
## Achievement ----
### MAP Reading Ach ----

## Student Demographic Groups ----
box(width = 12,
    title = html('Student Performance'),
    solidHeader = T, 
fluidRow(
  column(width = 4),
  column(width = 8, 
         radioGroupButtons(inputId = 'mapInput',
                           label = ' ',
                           size = 'lg',
                           choices = c('View by Grade Level', 'View by Student Group'))
         ), #close Column
  column(width = 6, 
         style = 'background-color:white; border-right: 1px solid lightgrey;',
         span(HTML("<b>Reading Achievement: </b>"),
              style = headingFont),
         br(),
         span(HTML("% in High or High Average Performance Level"),
              style = subHeadingFont),
         
         withSpinner(ggiraphOutput('mapElaDemos'), 
                     image = 'jeffcoSpin.gif', 
                     image.width = '75',
                     image.height = '75')

  ), 
  column(width = 6, 
         span(HTML("<b>Mathematics Achievement: </b>"),
              style = headingFont),
         br(),
         span(HTML("% in High or High Average Performance Level"),
              style = subHeadingFont),
         withSpinner(ggiraphOutput('mapMathDemos'), 
                     image = 'jeffcoSpin.gif', 
                     image.width = '75',
                     image.height = '75')
)
)
),
## Growth ----
### MAP Reading Growth ----
box(width = 12,
    title = html('Student Growth'),
    solidHeader = T, 
             fluidRow(
               column(width = 6,
                      style = 'background-color:white; border-right: 1px solid lightgrey;',
                      span('Reading', style = headingFont),
                      # span(HTML("<b>Growth:</b>"), style = headingFont),
                      br(),
                      span(HTML("Median Growth Percentile"), style = subHeadingFont),
                      bsPopover(
                        id = 'MAPReadingPercentile',
                        title = '',
                        content = 'The median growth percentiles are used to summarize student progress for the school. Median growth can range from 1-99, with 50 representing average growth. A median growth percentile of 55 means the school experienced growth equal to or higher than 55% of other students who took MAP (based on a national sample in the U.S.)',
                        placement = 'bottom',
                        trigger = 'hover',
                        options = NULL ),
                      actionButton(
                        inputId = 'MAPReadingPercentile',
                        label = 'info'),
                      br(),
                      ggiraphOutput('MapReadingGrowthPlot')
               ),#close column
### MAP Math Growth ----
               column(
                 width = 6,
                 span('Mathematics',
                      style = headingFont),
                 # span(HTML("<b>Growth:</b>"), style = headingFont),
                 br(),
                 span(HTML("Median Growth Percentile"), style = subHeadingFont),
                 bsPopover(
                   id = 'MAPMathPercentile',
                   title = '',
                   content = 'The median growth percentiles are used to summarize student progress for the school. Median growth can range from 1-99, with 50 representing average growth. A median growth percentile of 55 means the school experienced growth equal to or higher than 55% of other students who took MAP (based on a national sample in the U.S.)',
                   placement = 'bottom',
                   trigger = 'hover',
                   options = NULL ),
                 actionButton(
                   inputId = 'MAPMathPercentile',
                   label = 'info'),
                 ggiraphOutput('MapMathGrowthPlot')
               ) #close column
) #Box
             ), #close fluidRow
## MAP benchmark period ----
             fluidRow(
               column(width = 2,
                      span(
                        htmlOutput("MAPreadingBenchmark"),
                        style = "color:#315683; font-size:12px; text-align:right;")
               ) # close column
             ) #close fluidRow
    ), #close tabPanel
# MAP Trend Subtab ----
    tabPanel(value = 'sub-DistrictTestsMapTrend', 
             title = 'MAP Performance Level Details',
             fluidRow(
               box(
                 title = "Introduction",
                 closable = FALSE,
                 background = 'light-blue',
                 width = 12,
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 enable_dropdown = TRUE,
                 dropdown_icon = icon("info"),
                 dropdown_menu = boxDropdown(
                   boxDropdownItem(url = "https://www.jeffcopublicschools.org/academics/tests",
                                   name = "Jeffco Public Schools")),
                 span(HTML('Trend data can be helpful in understanding school performance over time. The charts below display the proportion of students in the school with <i>High</i> or <i>High Average</i> scores on the MAP test during a specific benchmark period over time. District data are also displayed for comparison purposes. Note: The ways MAP compares students to typical grade level performance (test norms) changed for Beginning of Year 2020 so comparisons with prior years should be made with caution.'), style = "color:#315683;font-size:14px")
               ) #close box
             ), #close fluidRow
## MAP Reading Trend ----
             fluidRow(
               column(
                 width = 5,
                 span('Reading',
                      style = headingFont),
                  span(HTML("<i><BR>Percentage of students in Performance Levels</i>"),
                      style = "color:#315683;font-size:100%"),
                 bsPopover(
                   id = 'MAPReadingTrendInfo',
                   title = '',
                   content = HTML('MAP has five achievement levels: High, High Average, Average, Low Average, and Low. Students who score in the <i>High</i> or <i>High Average</i> levels have a higher likelihood of scoring at or above grade level on the annual state assessment. The proportion of students in the school with <i>High</i> or <i>High Average</i> scores is displayed here, along with a district comparison.'),
                   placement = 'bottom',
                   trigger = 'hover',
                   options = NULL ),
                 actionButton(
                   inputId = 'MAPReadingTrendInfo',
                   label = 'info'),
                 withSpinner(ggiraphOutput('MAPTrendElaDiverging'), 
                             image = 'jeffcoSpin.gif', 
                             image.width = '75',
                             image.height = '75'),
                 hr(), 
                 gt_output('MAPTrendElaTable')
               ), #close column
## MAP Math Trend ----
               column(
                 width = 5,
                 span('Math',
                      style = headingFont),
                 span(HTML("<i><br>Percentage of students in Performance Levels</i>"),
                      style = "color:#315683;font-size:100%"),
                 bsPopover(
                   id = 'MAPMathTrendInfo',
                   title = '',
                   content = HTML('MAP has five achievement levels: High, High Average, Average, Low Average, and Low. Students who score in the <i>High</i> or <i>High Average</i> levels have a higher likelihood of scoring at or above grade level on the annual state assessment. The proportion of students in the school with <i>High</i> or <i>High Average</i> scores is displayed here, along with a district comparison. <BR> <BR>(HS Only) - Not all students in grades 9 and 10 are required to take MAP Math. These results summarize the performance of students enrolled in Algebra 1, Geometry, and similar classes. Students enrolled in Algebra 2 and higher level math classes are not included.'),
                   placement = 'bottom',
                   trigger = 'hover',
                   options = NULL ),
                 actionButton(
                   inputId = 'MAPMathTrendInfo',
                   label = 'info'),
                 withSpinner(ggiraphOutput('MAPTrendMathDiverging'), 
                             image = 'jeffcoSpin.gif', 
                             image.width = '75',
                             image.height = '75'),
                 hr(),
                 gt_output('MAPTrendMathTable'),
                 br(),
               ) #close column
             ) #close fluidRow
    ), #close tabPanel
tabPanel(value = 'sub-DistrictTestsMapCohort', 
         title = 'MAP Cohort Over Time',
         fluidRow(
           box(
             title = "Introduction",
             closable = FALSE,
             background = 'light-blue',
             width = 12,
             status = "primary",
             solidHeader = TRUE,
             collapsible = FALSE,
             enable_dropdown = TRUE,
             dropdown_icon = icon("info"),
             dropdown_menu = boxDropdown(
               boxDropdownItem(url = "https://www.jeffcopublicschools.org/academics/tests",
                               name = "Jeffco Public Schools")),
             span(HTML('These graphs show the change in student performance from the Beginning of Year to the End of Year, giving you a picture of the flow of student learning across the year. For example, students who started at the Average level (colored gray) in the Beginning of Year and then moved to the High Average (colored blue) at the End of Year are shown in the gray ribbon in the center of the graph that moves to the blue End of Year bar (meaning those students have improved across the year). The width of the ribbon is proportional to the number of students, so wider ribbons show more students and thinner ribbons show fewer students.'), style = "color:#315683; font-size:14px")
           )#close box
         ), #close fluidRow
         fluidRow(
           shinydashboard::box(width = 12,
                               fluidRow(
                                 column(
                                   width = 12,
                                   span(HTML("<i><b>Achievement:</b> <BR>Students in Performance Benchmark at the Beginning of the Year (BOY) compared to the End of the Year (EOY)</i>"),
                                        style = "color:#315683;font-size:100%"),
                                   bsPopover(
                                     id = 'MapCohort',
                                     title = '',
                                     content = 'NWEA MAP has five performance levels: <i>High, High Average, Average, Low Average, Low</i>',
                                     placement = 'bottom',
                                     trigger = 'hover',
                                     options = NULL),
                                   actionButton(
                                     inputId = 'MapCohort',
                                     label = 'info')
                                 ) #close column
                               ), #close fluidRow
                               br(),
                               fluidRow(
                                 column(width = 8,
                                        withSpinner(ggiraphOutput('MapCohortAlluvialReadingBin', 
                                                    height = 600), 
                                                    image = 'jeffcoSpin.gif', 
                                                    image.width = '75',
                                                    image.height = '75')
                                 ), 
                                 column(width = 4,
                                        gt_output('alluvialTableReading')
                                        )#close column
                               ), #close fluidRow
                               hr(),
                               fluidRow(
                                 column(width = 8,
                                        withSpinner(ggiraphOutput('MapCohortAlluvialMathBin', 
                                                                  height = "600px"), 
                                                    image = 'jeffcoSpin.gif', 
                                                    image.width = '75',
                                                    image.height = '75')
                                 ),
                                 column(width = 4,
                                        gt_output('alluvialTableMath')
                                 )#close column
                               ) #close fluidRow
           ) #close box
         ) #close fluidRow
), #close TabPanel
# DIBELS Subtab ----
    tabPanel(value = 'sub-DistrictTestsEarlyLit', 
             title = 'Early Literacy',
             fluidRow(
               box(
                 title = "Introduction",
                 closable = FALSE,
                 background = 'light-blue',
                 width = 12,
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 enable_dropdown = TRUE,
                 dropdown_icon = icon("info"),
                 dropdown_menu = boxDropdown(
                   boxDropdownItem(url = "https://www.jeffcopublicschools.org/academics/tests",
                                   name = "Jeffco Public Schools")),
                 span(HTML("Acadience Reading is a reading test for students in Kindergarten through grade 3 given three times per year. The results give information on early reading skills to help guide teachers on what students needs to learn next. Unlike state tests like CMAS or SAT, Acadience results are meant to be a continual guide for teaching and learning instead of an end-of-year evaluation of what has been learned.<br><br>Acadience Reading results will be provided following district administrations in September, January, and June annually."), style = "color:#315683; font-size:14px")
               ) #close box
             ), #close fluidRow
             fluidRow(column(
               width = 10,
               span(htmlOutput("DIBELSHeader"),
                    style = headingFont),
               span(HTML("formerly <b>D</b>ynamic <b>I</b>ndicators of <b>B</b>asic <b>E</b>arly <b>L</b>iteracy (Grades K-3)"),
                    style = "color:#315683;font-size:100%")
             )), #close column and fluidRow
             br(),
             fluidRow(
               column(
                 width = 6,
                 span(HTML("<i><b>Achievement:</b> <BR>Percentage of Students At or Above Literacy Benchmark</i>"),
                      style = "color:#315683;font-size:100%"),
                 bsPopover(
                   id = 'Dibels',
                   title = '',
                   content = 'Acadience Reading has four achievement levels: Above Benchmark, At Benchmark, Below Benchmark, and Well Below Benchmark. Students who score in the <i>At</i> and <i>Above</i> benchmark levels have a higher likelihood or scoring at above grade level on the grade 3 CMAS English Language Arts (reading/writing) state test. The proportion of students in the school with <i>At</i> or <i>Above</i> benchmark is displayed in these graphs, along with a district comparison.',
                   placement = 'bottom',
                   trigger = 'hover',
                   options = NULL),
                 actionButton(
                   inputId = 'Dibels',
                   label = 'info'),
                 shinydashboard::box(
                   width = 12,
                   radioGroupButtons(inputId = 'dibelsGroupInput', 
                                     label = '', 
                                     size = 'lg',
                                     choices = c('View by Grade Level', 'View by Student Group')),
                   withSpinner(ggiraphOutput('dibelsDemoPlots'), 
                               image = 'jeffcoSpin.gif', 
                               image.width = '75',
                               image.height = '75')
                 ) #close box
               ) #close column
             ) #close fluidRow
    ), #close TabPanel
# DIBELS Trend Subtab ----
    tabPanel(value = 'sub-DistrictTestsEarlyLitTrend', 
             title = 'Early Literacy Cohort Over Time',
             fluidRow(
               box(
                 title = "Introduction",
                 closable = FALSE,
                 background = 'light-blue',
                 width = 12,
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 enable_dropdown = TRUE,
                 dropdown_icon = icon("info"),
                 dropdown_menu = boxDropdown(
                   boxDropdownItem(url = "https://www.jeffcopublicschools.org/academics/tests",
                                   name = "Jeffco Public Schools")),
                 span("Acadience Reading is a reading test for students in Kindergarten through grade 3 given three times per year. The results give information on early reading skills to help guide teachers on what students need to learn next. Unlike state tests like CMAS or SAT, Acadience results are meant to be a continual guide for teaching and learning instead of an end-of-year evaluation of what has been learned.", style = "color:#315683; font-size:14px")
               )#close box
             ), #close fluidRow
             fluidRow(
               column(
                 width = 10,
                 span(HTML("Acadience Reading"),
                      style = headingFont),
                 span(HTML("formerly <b>D</b>ynamic <b>I</b>ndicators of <b>B</b>asic <b>E</b>arly <b>L</b>iteracy (Grades K-3)"),
                      style = "color:#315683;font-size:100%"),
                 br(),
                 br(),
                 span(HTML("These graphs show the change in student performance from the Beginning of Year to the End of Year, giving you a picture of the flow of student learning across the year. For example, students who started at the Below  or Well Below level (colored gray) in the Beginning of Year and then moved to the At or Above level (colored blue) at the End of Year are shown in the gray ribbon in the middle of the graph that moves to the blue End of Year bar (meaning those students have improved on Acadience Reading across the year). The width of the ribbon is proportional to the number of students, so wider ribbons show more students and thinner ribbons show fewer students."),
                      style = "color:#315683;font-size:100%")
               ) #close column
             ), #close fluidRow
             fluidRow(
               shinydashboard::box(width = 12,
                                   fluidRow(
                                     column(
                                       width = 12,
                                       span(HTML("<i><b>Achievement:</b> <BR>Students in Performance Benchmark at the Beginning of the Year (BOY) compared to the End of the Year (EOY)</i>"),
                                            style = "color:#315683;font-size:100%"),
                                       bsPopover(
                                         id = 'DibelsTrend',
                                         title = '',
                                         content = 'Acadience Reading has four achievement levels: Above Benchmark, At Benchmark, Below Benchmark, and Well Below Benchmark. Students who score in the "At" and "Above" benchmark levels have a higher likelihood or scoring at above grade level on the grade 3 CMAS English Language Arts (reading/writing) state test.',
                                         placement = 'bottom',
                                         trigger = 'hover',
                                         options = NULL),
                                       actionButton(
                                         inputId = 'DibelsTrend',
                                         label = 'info')
                                     ) #close column
                                   ), #close fluidRow
                                   br(),
                                   fluidRow(
                                     column(width = 8,
                                            withSpinner(ggiraphOutput('dibelsTrendggplot'), 
                                                        image = 'jeffcoSpin.gif', 
                                                        image.width = '75',
                                                        image.height = '75')
                                     ), 
                                     column(width = 4, 
                                            gt_output('dibelsTrendTable')
                                            )#close column
                                   ) #close fluidRow
               ) #close box
             ) #close fluidRow
    ) #close TabPanel
  ) #close tabBox
) #close tabPanel District Academic
