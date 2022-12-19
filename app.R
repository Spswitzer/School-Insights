# Load required packages ---- <- 
 source('packages.R', local = TRUE)

# Data ----
 source('data/globalData.R', local = TRUE)

# UI files----
 source('ui/schoolBasicsUI.R', local = TRUE)
 source('ui/schoolcultureUI.R', local = TRUE)
 source('ui/statewideTestsUI.R', local = TRUE)
 source('ui/districtTestsUI.R', local = TRUE)
 source('ui/printUI.R', local = TRUE)
 source('ui/aboutUI.R', local = TRUE)
## Select Input for School Names ----

# UI ----
ui <- function(request) {
  dashboardPage(
    title = 'School Insights',
    scrollToTop = T,
    ## Header ----
    header = shinydashboardPlus::dashboardHeader(
      title = title,
      titleWidth = '250',
      disable = FALSE,
      .list = NULL,
      leftUi = tagList(
        tags$style(
          type = 'text/css',
          ".selectize-input {min-height: 14px; font-size: 14px; line-height: 14px; vertical-align: bottom; margin-top: 1px; border-radius: 5px; min-width: 225px; padding: 1px 4px; color:black;} 
        .selectize-dropdown {min-height: 20px; font-size: 14px; line-height: 14px; margin-top: 1px; padding: 2px 4px;}"
        ),
        selectInput(
          inputId = "SchoolID",
          label = span(HTML("Step 1: Select School"), 
                       style = "color:white; font-weight:bold; font-size:14px"),
          choices = list(
            'District Summary'= 9998,
            'Multi Level' = multiNames,
            'Elementary' = elemNames,
            'Middle' = midNames,
            'High' = highNames
          ),
          selected = 9998
          )
      ),#end left_menu
        # dropDownModuleUi(id = 'main'),
      fixed = TRUE      
    ), #close Header
    ## Sidebar ----
    sidebar = dashboardSidebar(disable = TRUE),
    ## Footer ----
    footer = dashboardFooter(left = "Jeffco Public Schools", 
                             right = "Research and Assessment Design"),
    ## Body ----
    body = dashboardBody(
      #Use cascading style sheet from www folder
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "customJan21.css")), 
      # Userway widget for accessibility
      tags$script(`data-account` = "LYTgLoPEmt", src = "https://cdn.userway.org/widget.js"), 
      #Google translate menu
      HTML("<!DOCTYPE html><html lang='en-US'>
         <div id='google_translate_element'></div>
         <script type='text/javascript'>function googleTranslateElementInit() {new google.translate.TranslateElement({pageLanguage: 'en', includedLanguages: 'en,es,ru,vi,ar,zh-CN,fr,hmn,ko,sw,ku', layout: google.translate.TranslateElement.InlineLayout.SIMPLE}, 'google_translate_element');}</script>
         <script type='text/javascript'>function googleTranslateElementInit()</script><script type='text/javascript' src='https://translate.google.com/translate_a/element.js?cb=googleTranslateElementInit'></script>
         </body>
         </html>"),
      #Google Tag Manager Java script for meta data collection
      tags$head(includeHTML("gtm.html")), 
      # tags$head(includeHTML("gtag.html")),
      #Begin Tabs
      tabsetPanel(
        id = 'tabs',
        type = 'pills',
        ### (1) School Basics Tab ----
        uiSchoolBasics,
        # ### (2) School Culture Tab ----
        uiSchoolculture,
        ### (3) Academic Growth & Achievement State Tab ----
        uiStatewideTests,
        # ### (4) Academic Growth & Achievement Jeffco Tab ----
         uiDistrictTests,
        # ### (5) Print Summary Tab ----
         uiPrint,
        # ### (6) App Guidance  ----
        uiAbout
      ) # Closes tabsetPanel
    ) #close dashboardBody
  ) # closes Dashboard Page Plus
}
#################### SERVER   #################### 
server = function(input, output, session) { 
  
  #user URL input filters to each school 
   source('server/urlObserve.R', local = TRUE)
  # 
  # #### (1) School Basics Server #####
  source('server/schoolBasicsServer.R', local = TRUE)
  # 
  # #### (2) School Culture Server #####
  source('server/schoolCultureServer.R', local = TRUE)
  
  #### (3) Academic Growth & Achievement State Server #####
  source('server/statewideTestsServer.R', local = TRUE)
  
  #### (4) Academic Growth & Achievement Jeffco Server #####
  source('server/districtTestsServer.R', local = TRUE)
  # 
  # #### (5) Print Summary Server #####
   source('server/printServer.R', local = TRUE)
  # 
  # #### Remove Tabs Conditionals ####
  source('server/conditionalTabsServer.R', local = TRUE)
  
}
shinyApp(ui = ui, server = server)
