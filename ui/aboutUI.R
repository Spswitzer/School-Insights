#### (6) App Guidance  #####
uiAbout <-  tabPanel(
  value = 'main-About',
  title = 'About Insights',
  br(),
# Introduction ----
  fluidRow(
    column(1),
    column(10,
           box(
             title = "Introduction",
             background = 'light-blue',
             width = 12,
             status = "primary",
             solidHeader = TRUE,
             collapsible = FALSE,
             closable = FALSE,
             enable_dropdown = TRUE,
             dropdown_icon = " ",
             span(HTML("Jeffco Public School&apos;s School Insights brings together multiple sources of data in a single web-based resource. School Insights provides easy access for school leaders, educators, and families to in-depth information about schools to help stay informed and engaged in school quality and improvement efforts."), style = "color:#315683; font-size:14px")
           ) #close box
    ) #Close column
  ), #close fluidRow
  br(),
# FAQs & Definitions ----
  fluidRow(
    column(1),
    column(8, 
           span(HTML("School Insights FAQs & Definitions"),
                style = headingFont),
    ) #close column
  ), #close fluidRow
  fluidRow(
    column(1),
    column(8,
           style = 'display: flex;justify-content: center;align-items: center;height: 110px;border: 0 solid white; padding-left: 50px;',
           span(HTML('Learn more about the School Insights, including how to find information that helps to explain the data, ways to use the information, definitions of some of the terms you will see on Insights, and more.<br><ul style="list-style-type:none;"><li><a href = https://jeffcohelp.freshservice.com/a/solutions/articles/16000041783 target = blank> Insights FAQs & Definitions (English, Spanish, Russian, Chinese, Vietnamese) </a></li></ul>'))
    ) #close column
  ), #close fluidRow
# Tutorials ----
  fluidRow(
    column(1),
    column(8, 
           span(HTML("School Insights Tutorials"),
                style = headingFont),
    ) #close column
  ), #close fluidRow
  br(),
  fluidRow(
    column(1),
    column(8,
           style = 'display: flex;justify-content: center;align-items: center;height: 110px;border: 0 solid white; padding-left: 50px;',
           span(HTML('Explore a quick overview of School Insights, including how to use the information on the site to get a full picture of each school, including strengths and areas where improvements may be helpful.<br><ul style="list-style-type:none;"><li><a href = https://www.canva.com/design/DAEcPWvbNls/skyTTsDrENN8dy0ShJ7VmA/view?utm_content=DAEcPWvbNls&utm_campaign=designshare&utm_medium=link&utm_source=recording_view target = blank> Two-minute Video (English) </a> </li> <br> <li>  <a href = https://jeffcohelp.freshservice.com/a/solutions/articles/16000041782 target = blank> Print Version (English, Spanish, Russian, Chinese, Vietnamese) </a></li> </ul>')))  
  ), #close fluidRow
  br(),
# Feedback ----
  fluidRow(
    column(1),
    column(8, 
           span(HTML("School Insights Feedback"),
                style = headingFont),
    ) #close column
  ), #close fluidRow
  fluidRow(
    column(1),
    column(8,
           style = 'display: flex;justify-content: center;align-items: center;height: 110px;border: 0 solid white; padding-left: 50px;',
           span(HTML('Please let us know if you have feedback about School Insights. Your feedback can help us continue to update the site to make it as clear and helpful as possible to Jeffco Public Schools’ administrators, teachers, and families. Thanks!<br><ul style="list-style-type:none;"> <li> <a href = https://jeffcohelp.freshservice.com/support/catalog/items/663 target = blank> Link to feedback form (Jeffco employees) </a> </li><li> <a href = https://www.jeffcohelp.org target = blank> Link to feedback form (Families and Community) </a> </li> </ul>')))
  ) #close fluidRow
) # Closes tabPanel