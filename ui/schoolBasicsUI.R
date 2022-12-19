#### (1) School Basics #####
uiSchoolBasics <- 
  tabPanel(
    value = 'main-SchoolBasics',
    title = 'School Basics',
    hr(),
    fluidRow(
      column(width = 4,
# Enrollment & Attendance ----
             span(HTML("Official Counts"),
                  style = headingFont),
             span(HTML("(2021-2022)"),
                  style = subHeadingFont),
             bsPopover(
               id = 'attend',
               title = '',
               content = 'The enrollment information is based on official state reports from the pupil count held annually in October (includes pre-kindergarten students for elementary schools, and includes students at Mount View Youth Services Center).<br> Annual attendance is the average of overall students present in school on any given day and is based on end of prior school year reports.<br>Trend data are based on end of year values from official state reports.'
               ,
               placement = "bottom",
               trigger = "hover",
               options = NULL
             ),
             actionButton(
               inputId = "attend",
               label = "info",
               onClick = "window.open('https://www.jeffcopublicschools.org/cms/one.aspx?portalId=627965&pageId=926681', '_blank')"
             ),
## Enrollment & Attendance FlipBox ----
             flipBox(
               id ='enrollmentFlip',
               front = list(
                 span('Click to View Trend Information',
                      style = subHeadingFont),
### Current Year ----
                 uiOutput("schoolEnrollment"),
                 uiOutput("schoolGradeRange"),
                 uiOutput("schoolAttendance")
               ), #close Front list
### Prior Years ----
               back = list(
                 span(HTML('Student Count'),
                      style = subHeadingFont),
                 plotOutput('enrollmentTrendBarPlot', height = '130px'),
                 span(HTML('Grade levels included in values may <b>not</b> be consistent year over year'), 
                      style = captionFont),
                 hr(),
                 span(HTML('Average Student Attendance Rate'),
                      style = subHeadingFont),
                 plotOutput('attendanceTrendBarPlot', height = '130px')
               ), #close back list
               width = 12
             ) #close flipBox
      ), #close column
      column(width = 4,
# Map/ Geo Location ----
             column(10,
                    span('Location & Contact Information',
                         style = headingFont),
                    bsPopover(
                      id = 'mapInfo',
                      title = '',
                      content = 'Click blue square on map for school address and website. The shaded area on the map below shows the area served by each neighborhood school.',
                      placement = "bottom",
                      trigger = "hover",
                      options = NULL
                    ),
                    actionButton(
                      inputId = "mapInfo",
                      label = "info",
                      onClick = "window.open('https://www.jeffcopublicschools.org/schools/boundary_maps', '_blank')"
                    )),
 ## Map of School ----
             leafletOutput('geoLocation',
                           width = "95%",
                           height = 335)
      ),
      column(width = 4,
# Website & School Profile----
             column(12,
                    span(HTML(" "),
                         style = headingFont),
                    box(
                      title = "Links to School Resources",
                      width = 12,
                      status = "primary",
                      uiOutput('webResources')
                    )
             )
      ), #close column
# Ratios ----
      fluidRow(
        column(width = 12,
               conditionalPanel('input.SchoolID != 9998',
                                hr(),
                                fluidRow(
                                  column(4,
                                         span(" School-wide Ratio",
                                              style = headingFont),
                                         bsPopover(
                                           id = 'ratioInfo',
                                           title = '',
                                           content = 'Educator student ratios are based on prior year reports from Colorado Department of Education that count all licensed staff in a school, not just classroom teachers.',
                                           placement = "bottom",
                                           trigger = "hover",
                                           options = NULL
                                         ),
                                         actionButton(
                                           inputId = "ratioInfo",
                                           label = "info"
                                         ), #close action button
                                         valueBoxOutput('ratios', width = 12)
                                  ),#close column
# School type ----
                                  column(4,
                                         span("School classification(s)",
                                              style = headingFont),
                                         bsPopover(
                                           id = 'classification',
                                           title = '',
                                           content = '<b>Alternative Education:  </b>Free public school offering a variety of non-traditional school options for students that may be struggling in traditional school settings.<br><b>Charter Schools:</b> Free public school of choice with their own governing board and a contract with either the district or the state’s Charter School Institute.<br><b>Option Schools:</b> Free public school of choice that provide a special focus or educational program. Unlike charter schools, option schools are run by the school district.<br><b>Title I Schools:</b> Free public school with large concentrations of low-income students receiving supplemental funds to assist in meeting educational goals.',
                                           placement = "bottom",
                                           trigger = "hover",
                                           options = NULL
                                         ),
                                         actionButton(
                                           inputId = "classification",
                                           label = "info"),
                                         div(dataTableOutput("SchoolDesignationsServer"),
                                             style = "color:#315683;font-size:95%")
                                  ) #close column
                                ) #close fluidRow
               ) #close conditionalPanel
        ) #close column
      ) #close fluidRow
    ), #close fluidRow
    hr(),
# Program Participation ----
    fluidRow(
      column(4,
             fluidRow(
               column(12, 
               span(HTML('Student Program Participation'),
                    style = headingFont),
               span(HTML("(2021-2022)"),
                    style = subHeadingFont),
               bsPopover(
## Student Subgroups Definition----
                 id = 'program',
                 title = '',
                 content = HTML('Information based on official state reporting each October (including pre-kindergarten student for elementary schools). <br><br><b>FRL</b> = Students Eligible for Free or Reduced Price Lunch including documented financial need, those with Foster Care services, Migrant students, Homeless or Runaway students<br><br><b>ELL</b> = English Language Learners including students classified as Non-English Proficient, Limited English Proficient, and Fluent English Proficient <br><br><b>SpEd</b> = Students with Special Education Programming with an Individualized Education Program <br><br> <b>GT</b> = Students with Gifted & Talented Programming with an Advanced Learning Plan'),
                 placement = "bottom",
                 trigger = "hover",
                 options = NULL
               ),
               actionButton(
                 inputId = "program",
                 label = "info"
               ),
br(),
                      span("Percentage of students with services",
                           style = headingFont),
                      br(),
                      span("Data for groups with fewer than 16 students are suppressed",
                           style = subHeadingFont),
             ggiraphOutput('simpleProgramParticipation',
                           width = "100%",
                           height = '100%'), 
         span(html('The enrollment information is based on official state reports from the pupil count held annually in October (includes pre-kindergarten students for elementary schools, and includes students at Mount View Youth Services Center).<br> Annual attendance is the average of overall students present in school on any given day and is based on end of prior school year reports.<br>Trend data are based on end of year values from official state reports.'), style = subHeadingFont)

      ) #close column 12
    ) #close fluid row
      ),
# Race and Ethnicity ----
      column(width = 4,
             fluidRow(
               column(12, 
                span(HTML("Student Race & Ethnicity"),
                    style = headingFont),
               span(HTML("(2021-2022)"),
                    style = subHeadingFont),
               bsPopover(
## Student Subgroups Definition ----
                 id = 'demos',
                 title = '',
                 content = 'Demographic information based on official state reporting each October (including pre-kindergarten student for elementary schools).',
                 placement = "bottom",
                 trigger = "hover",
                 options = NULL
               ),
               actionButton(
                 inputId = "demos",
                 label = "info"
               ),
              br(),
                      span("Percentage of students in group",
                           style = headingFont),
                      br(),
                      span("Data for groups with fewer than 16 students are suppressed",
                           style = subHeadingFont),
             ggiraphOutput('simpleRace',
                           width = "100%",
                           height = '100%')
) #close column 12
) #close fluid row
), #close column 4
# Choice In & Choice Out ----
      column(width = 4,
             fluidRow(
               column(
                 width = 10,
                 span("School Choice",
                      style = headingFont),
                 span(HTML("(2021-2022)"),
                      style = subHeadingFont),
                 bsPopover(
                   id = 'choice',
                   title = '',
                   content = 'Colorado law allows families to move to a school where they do not live in the boundaries to automatically attend. There are many reasons families choose schools, like matching student needs to different programs, transportation needs, etc.',
                   placement = "bottom",
                   trigger = "hover",
                   options = NULL
                 ),
                 actionButton(
                   inputId = "choice",
                   label = "info")
               )),
## Choice In ----
             flipBox(
               id = 4,
               width = 12,
               front = div(
                 span('Choice In',
                      style = headingFont),
                 br(),
                 span('click to view Choice Out Information',
                      style = subHeadingFont),
                 ggiraphOutput('choiceInPlotPie', height = "200px")
               ),
## Choice Out ----
               back =
                 div(
                   span('Choice Out',
                        style = headingFont),
                   br(),
                   span('click to view Choice In Information',
                        style = subHeadingFont),
                   ggiraphOutput('choiceOutPlotBar', height = "150px")
                 ) #close list
             ) #close Flipbox
      ) #close column
    ),  #close fluidRow
conditionalPanel(
  condition = "input.SchoolID == '9998'",
  hr(),
  fluidRow(
    column(4), 
    column(8,
           span(HTML('Access information on <a href="https://jeffcopublicschools.shinyapps.io/ComparableDistricts">Colorado districts with student characteristics similar to Jeffco</a>'),
                style = "color:#315683; font-size:14px")
    )
  )
)

# fluidRow(
#   # column(1), 
#   column(10,
# span(html('The enrollment information is based on official state reports from the pupil count held annually in October (includes pre-kindergarten students for elementary schools, and includes students at Mount View Youth Services Center).<br> Annual attendance is the average of overall students present in school on any given day and is based on end of prior school year reports.<br>Trend data are based on end of year values from official state reports.'), style = subHeadingFont)
# ))
  ) #close tabPanel