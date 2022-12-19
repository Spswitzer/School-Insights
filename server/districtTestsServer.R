#### (4) Academic Growth & Achievement Jeffco Server #####
# MAP ----
## MAP Headings - Text ----
output$mapReadingTrend <- renderText({
  paste0('Benchmark: ', mapTestPeriod)
})

output$mapMathTrend <- renderText({
  paste0('Benchmark: ', mapTestPeriod)
})

## MAP reactive data
mapGrowthDf <- reactive({
  
  schoolNumber <-  input$SchoolID
  
  m1 <- mapDemos %>% 
    filter(category == 'grade') %>% 
    filter(subcategory > 2) %>% 
    filter(profNumeric == 4) %>% 
    filter(cdeSchoolNumber == schoolNumber) 
  
  gradesInSchool <- m1 %>% 
    ungroup() %>% 
    distinct(subcategory) %>% 
    pull()
  
  m2 <- mapDemos %>% 
    filter(category == 'grade') %>% 
    filter(subcategory > 2) %>% 
    filter(profNumeric == 4) %>% 
    filter(cdeSchoolNumber == '9998') #filter by district
  
  m3 <- m1 %>% 
    filter(testingPeriodName == 'Fall') %>%  #only return composite of grades for MPG, need all grades for ach plots
    filter(subcategoryN > 15) #suppression
  
  # m4  <-  
  #   bind_rows(m1, m2) %>%  #need both school and district results for ach plots
  #   filter(subcategory %in% gradesInSchool) #Apply suppression rules at grade level
  
  return(list("growth" = m3, "schoolCode" = schoolNumber))  #"ach" = m4,
})
# mapGrowthDf <- reactive({
#   school <-  input$SchoolID
#   
#   m1 <- mapBySchoolbyGrade %>% #school that I have input
#     filter(testedAtSchoolNumber == school) 
#   
#   gradesInSchool <- m1 %>% 
#     ungroup() %>% 
#     distinct(gradeId) %>% 
#     pull()
#   
#   m2 <- mapBySchoolbyGrade %>% 
#     filter(testedAtSchoolNumber == '9998') #filter by district
#   
#   m3 <- m1 %>% 
#     filter(
#       gradeId != 'All\nGrades',
#            testingPeriodName == 'Fall') %>%  #only return composite of grades for MPG, need all grades for ach plots
#     filter(schoolN > 15) #suppression
#   
#   m4  <-  
#     bind_rows(m1, m2) %>%  #need both school and district results for ach plots
#     filter(gradeId %in% gradesInSchool) #Apply suppression rules at grade level
# 
#     return(list("growth" = m3, "ach" = m4, "schoolCode" = school))
# })

## Reading Growth Status Plots ----
output$MapReadingGrowthPlot <- renderggiraph({
  
  growth <- mapGrowthDf()$growth
  
  school <- mapGrowthDf()$schoolCode
  
  suppress <- mapGrowthDf()$growth
  
  # validate(need(school != '4408', 'No growth data for this school, due to inclusion of Jeffco Remote Learning Program'))
  if (school == '4798'){ #Connections had all data suppressed except All Students for Middle of Year
  } else if (nrow(suppress) > 0) {
    mapGrowthPlot(df = mapGrowthDf()$growth, schoolCode = school, q = 'Fall')
  } else{
  }
})
output$MapMathGrowthPlot <- renderggiraph({

  growth <- mapGrowthDf()$growth
  
  school <- mapGrowthDf()$schoolCode
  
  suppress <- mapGrowthDf()$growth
  
  subjectName <- "MATH"
  
  # validate(need(school != '4408', 'No growth data for this school, due to inclusion of Jeffco Remote Learning Program'))
   if (school == '4798'){ #Connections had all data suppressed except All Students for Middle of Year
  } else if (nrow(suppress) > 0) {
    mapGrowthPlot(df = mapGrowthDf()$growth, schoolCode = school, q = 'Fall', subject = subjectName)
  } else{
  }
})
## Map Achievement Plots ----
### Reading ----
#### Composite Plus Each Grade

# #### All Grades -----
output$MAPTrendElaDiverging <- renderggiraph({
  
  SchoolNumber <- input$SchoolID
  
  if (SchoolNumber %in% unique(allMapAllLevels$TestedAtSchoolNumber))
  {
    mapTrendAllPlsPlotFlip(school = SchoolNumber, content = "READING")
  } else {
    p <- ggplot()+
      geom_text(aes(x = 1, y = 1, label = 'Results for this\nassessment are\nnot reported for\nthe selected school.'), size = 8, color = 'grey') +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
    
    ggiraph(code = print(p), 
            tooltip_opacity = 1,
            tooltip_offx = 5,
            tooltip_extra_css = 'color:#333333;stroke:#e26d28;background:white;border:1px solid darkgrey;font-size:10px',
            hover_css = "cursor:pointer;stroke:#EA9563;",
            width = 1
    )
  }
})

output$MAPTrendMathDiverging <- renderggiraph({
  
  SchoolNumber <- input$SchoolID
  
  if (SchoolNumber %in% unique(allMapAllLevels$TestedAtSchoolNumber))
  {
    mapTrendAllPlsPlotFlip(school = SchoolNumber, content = "MATH")
  } else {
    p <- ggplot()+
      geom_text(aes(x = 1, y = 1, label = 'Results for this\nassessment are\nnot reported for\nthe selected school.'), size = 8, color = 'grey') +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
    
    ggiraph(code = print(p), 
            tooltip_opacity = 1,
            tooltip_offx = 5,
            tooltip_extra_css = 'color:#333333;stroke:#e26d28;background:white;border:1px solid darkgrey;font-size:10px',
            hover_css = "cursor:pointer;stroke:#EA9563;",
            width = 1
    )
  }
})

output$MAPTrendElaTable <- render_gt({

  SchoolNumber <- input$SchoolID
  
  if (SchoolNumber %in% unique(allMapAllLevels$TestedAtSchoolNumber))
  {
    mapTrendTable(school = SchoolNumber, content = "READING") #mapTrendAllPLs.R contain functions for plots and tables
  } else {

  }
})

output$MAPTrendMathTable <- render_gt({
  
  SchoolNumber <- input$SchoolID
  
  if (SchoolNumber %in% unique(allMapAllLevels$TestedAtSchoolNumber))
  {
    mapTrendTable(school = SchoolNumber, content = "MATH")#mapTrendAllPLs.R contain functions for plots and tables
  } else {
    
  }
})


output$MapCohortAlluvialReadingBin <- renderggiraph({
  
  SchoolNumber <- input$SchoolID
  
 validatePlot <-  mapAlluvialBinaryLevels %>% 
   filter(CDESchoolNumber == SchoolNumber) %>% 
   filter(contentGroupName == 'READING')
  
  validate(need(SchoolNumber %in% unique(mapAlluvialBinaryLevels$CDESchoolNumber), 'Results for this assessment are not reported for the selected school.'))
  
  validate(need(last(!is.na(validatePlot$totaln)), 'This reading display is not available for the selected school'))
  
  source('data/map/mapFunctions/mapCohortAlluvialPlotFunction.R', local = TRUE)
  mapCohortAlluvialPlotBin(.df = mapAlluvialBinaryLevels, .content =  'READING', .school = SchoolNumber)
})

output$alluvialTableReading <- render_gt({
  SchoolNumber <- input$SchoolID
  
  validatePlot <-  mapAlluvialBinaryLevels %>% 
    filter(CDESchoolNumber == SchoolNumber) %>% 
    filter(contentGroupName == 'MATH')
  
  validate(need(SchoolNumber %in% unique(mapAlluvialBinaryLevels$CDESchoolNumber), ' '))

  validate(need(last(!is.na(validatePlot$totaln)), ' '))
  
  tableData <- mapAlluvialBinaryLevels%>% 
    filter(contentGroupName == "READING") %>% 
    filter(CDESchoolNumber == SchoolNumber) %>% 
    arrange(desc(EndYear)) %>% 
    group_by(EndYear) %>% 
    select(profBOYLabel, profEOYLabel, sum) %>% 
    mutate(sum = prettyNum(sum, big.mark = ",")) %>% 
    mutate(plDescription = case_when(
      profBOYLabel == 'High or \nHigh Average' & profEOYLabel == 'High or \nHigh Average' ~ 'Remain in High or High Average', 
      profBOYLabel == 'High or \nHigh Average' & profEOYLabel == 'Average or \nLower' ~ 'Move down from High or High Average to Average or Lower', 
      profBOYLabel == 'Average or \nLower' & profEOYLabel == 'Average or \nLower' ~ 'Remain in Average or Lower', 
      profBOYLabel == 'Average or \nLower' & profEOYLabel == 'High or \nHigh Average' ~ 'Move up from Average or Lower to High or High Average', 
    )) %>% 
    select(plDescription, sum) %>% 
    arrange(EndYear, plDescription) %>% 
    filter(str_detect(plDescription, 'Move'))
  
  
  gt(tableData, 
     auto_align = F) %>% 
    cols_label(sum = md(''), 
               plDescription = 'Student Movement Between Levels') %>% 
    cols_align(align = 'left', 
               columns = plDescription) %>% 
    tab_options(table.font.size = 12, 
                row_group.font.weight = 'bold') %>% 
    tab_style(
      style = list(
        cell_fill(color = "#98AAC1")
      ),
      locations = cells_body(
        columns = c(plDescription, sum),
        rows = str_detect(plDescription, 'Move down')
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "lightgrey")
      ),
      locations = cells_body(
        columns = c(plDescription, sum),
        rows = str_detect(plDescription, 'Move up')
      )
    )
})

output$MapCohortAlluvialMathBin <- renderggiraph({
  
  SchoolNumber <- input$SchoolID

  validatePlot <-  mapAlluvialBinaryLevels %>% 
    filter(CDESchoolNumber == SchoolNumber) %>% 
    filter(contentGroupName == 'MATH')
  
  validate(need(SchoolNumber %in% unique(mapAlluvialBinaryLevels$CDESchoolNumber), 'Results for this assessment are not reported for the selected school.'))
  
  validate(need(SchoolNumber != '4408', 'No growth data for this school, due to inclusion of Jeffco Remote Learning Program'))
  
  validate(need(last(!is.na(validatePlot$totaln)), 'This math display is not available for the selected school'))
  
  EndYear <- unique(mapAlluvialBinaryLevels$EndYear)
  
  #function for imputing NA data
  mapAlluvialBinaryLevelsAll <- function(.EndYear = '2021-2022') {
    
    mapAlluvialFiltered <- mapAlluvialBinaryLevels  %>% 
      filter(CDESchoolNumber == SchoolNumber) %>%
      filter(EndYear == .EndYear) %>%
      filter(!is.na(EndYear)) %>%
      arrange(EndYear)
    
    mapAlluvialFiltered$sum[is.na(mapAlluvialFiltered$sum)] <- 0
    mapAlluvialFiltered$n[is.na(mapAlluvialFiltered$n)] <- mapAlluvialFiltered$n
    mapAlluvialFiltered$totaln[is.na(mapAlluvialFiltered$totaln)] <- mapAlluvialFiltered$totaln
    mapAlluvialFiltered$totalInBoyPB[is.na(mapAlluvialFiltered$totalInBoyPB)] <- first(mapAlluvialFiltered$totalInBoyPB)
    mapAlluvialFiltered$totalInEoyPB[is.na(mapAlluvialFiltered$totalInEoyPB)] <- nth(mapAlluvialFiltered$totalInEoyPB, 4)
    mapAlluvialFiltered$percentInBoyPB[is.na(mapAlluvialFiltered$percentInBoyPB)] <- first(mapAlluvialFiltered$percentInBoyPB)
    mapAlluvialFiltered$percentInEoyPB[is.na(mapAlluvialFiltered$percentInEoyPB)] <- nth(mapAlluvialFiltered$percentInEoyPB, 4)
    mapAlluvialFiltered$percent[is.na(mapAlluvialFiltered$percent)] <- '0%'
    
    mapAlluvialFiltered
  }
  
  # Combine results of function
  
  mapAlluvialBinaryLevelsAll <-   map_df(EndYear, mapAlluvialBinaryLevelsAll)
  
  source('data/map/mapFunctions/mapCohortAlluvialPlotFunction.R', local = TRUE)
  mapCohortAlluvialPlotBin(.df = mapAlluvialBinaryLevelsAll, .content =  'MATH', .school = SchoolNumber)
  
})

output$alluvialTableMath <- render_gt({
  SchoolNumber <- input$SchoolID
  
  validatePlot <-  mapAlluvialBinaryLevels %>% 
    filter(CDESchoolNumber == SchoolNumber) %>% 
    filter(contentGroupName == 'MATH')
  
  validate(need(SchoolNumber %in% unique(mapAlluvialBinaryLevels$CDESchoolNumber), ' '))
  
  validate(need(SchoolNumber != '4408', ' '))
  
  validate(need(last(!is.na(validatePlot$totaln)), ' '))
  
  tableData <- mapAlluvialBinaryLevels%>% 
    filter(contentGroupName == "MATH") %>% 
    filter(CDESchoolNumber == SchoolNumber) %>% 
    arrange(desc(EndYear)) %>% 
    group_by(EndYear) %>% 
    select(profBOYLabel, profEOYLabel, sum) %>% 
    mutate(sum = prettyNum(sum, big.mark = ",")) %>% 
    mutate(plDescription = case_when(
      profBOYLabel == 'High or \nHigh Average' & profEOYLabel == 'High or \nHigh Average' ~ 'Remain in High or High Average', 
      profBOYLabel == 'High or \nHigh Average' & profEOYLabel == 'Average or \nLower' ~ 'Move down from High or High Average to Average or Lower', 
      profBOYLabel == 'Average or \nLower' & profEOYLabel == 'Average or \nLower' ~ 'Remain in Average or Lower', 
      profBOYLabel == 'Average or \nLower' & profEOYLabel == 'High or \nHigh Average' ~ 'Move up from Average or Lower to High or High Average', 
    )) %>% 
    select(plDescription, sum) %>% 
    arrange(EndYear, plDescription) %>% 
    filter(str_detect(plDescription, 'Move'))
  
  gt(tableData, 
     auto_align = F) %>% 
    cols_label(sum = md(''), 
               plDescription = 'Student Movement Between Levels') %>% 
    cols_align(align = 'left', 
               columns = plDescription) %>% 
    cols_width(plDescription ~ px(250)) %>% 
    tab_options(table.font.size = 12, 
                row_group.font.weight = 'bold') %>% 
    tab_style(
      style = list(
        cell_fill(color = "#98AAC1")
      ),
      locations = cells_body(
        columns = c(plDescription, sum),
        rows = str_detect(plDescription, 'Move down')
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "lightgrey")
      ),
      locations = cells_body(
        columns = c(plDescription, sum),
        rows = str_detect(plDescription, 'Move up')
      )
    )
  
  
})

## MAP Demos ----
#### Reactive ----


mapDemoDataFiltered <- reactive({
  
  schoolNumber <- input$SchoolID
  school <-  input$SchoolID
  
  m1 <- mapDemos %>% 
    filter(cdeSchoolNumber == schoolNumber) %>% 
    filter(subcategory %in% c( "all", 
                               "Free or Reduced Lunch Eligible",
                               "Not Free or Reduced Lunch",
                               "English Language Learner",
                               "Not ELL",
                               "IEP", 
                               "No IEP", 
                               "Ethnic/Racial Minority",
                               "Not Ethnic/Racial Minority",
                              "Not GT",
                              "GT"))
  
  demoFiller <- data.frame(subcategory = c("All Students", 
                                                 "Free or Reduced Lunch Eligible",
                                                 "Not Free or Reduced Lunch Eligible", 
                                                 "English Language Learner", 
                                                 "Not In Engilsh Language Learner Program", 
                                                 "Individualized Education Program",
                                                 "Not In Individualized Education Program",
                                                 "Students of Color of Hispanic", 
                                                 "White Students",
                                                 "Gifted and Talented Program",
                                                 "Not In Gifted and Talented Program"))
  m1 <- demoFiller %>% 
    full_join(m1)
  
  m2 <- mapDemos %>% 
    filter(cdeSchoolNumber == schoolNumber| cdeSchoolNumber == '9998') %>% 
    filter(subcategory %in% c('all', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10')) %>% 
    filter(profNumeric == 4) %>% 
    mutate(
      # testingPeriodName = 'Fall',
    percentAtAboveSuppressed = case_when(
      subcategoryN < 16 ~ '< 16 students', 
      TRUE~ as.character(paste0(round(pctMeetExceed*100), '%')))) %>% 
    mutate(subcategory = factor(subcategory, 
                                levels = c('all', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10'), 
                                labels = c('All Students', 'Grade 1', 'Grade 2', 'Grade 3', 
                                           'Grade 4', 'Grade 5', 'Grade 6', 
                                           'Grade 7', 'Grade 8', 'Grade 9', 
                                           'Grade 10'), 
                                ordered = T))

  
  m4 <- m2 %>% 
    filter(school == 'School')   
  
  m3 <- m2 %>% 
    filter(school == 'District')
  
  gradesInSchool <- m4 %>% 
    ungroup() %>% 
    distinct(subcategory) %>% 
    pull()
  
  m5 <- m4 %>% 
    bind_rows(m3) %>% 
    filter(subcategory %in% gradesInSchool) 
  

  return(list('mapDemoData' = m1, 
              'mapGrades' = m5, 
              'mapGradesDistrict' = m3,
              "schoolCodes" = school))
})
##### Reading By Grade and By Student Demo Group -----
output$mapElaDemos <- renderggiraph({

  subjectName <- 'READING'
  
   validate(need(nrow(mapDemoDataFiltered()$mapDemoData) > 0, 'No results reported for this school, due to small testing population'))
   
  if(input$mapInput == 'View by Student Group'){
  source('data/map/mapFunctions/mapDemosFunction.R')
  mapDemosPlotFunction(.df = mapDemoDataFiltered()$mapDemoData, 
                                   .contentName = subjectName)
  } else {
    if(input$SchoolID == '9998'){
      mapAchPlotBenchmarksByGradeWithComposite(df = mapDemoDataFiltered()$mapGradesDistrict,  
                                               subject = subjectName, 
                                               school = mapDemoDataFiltered()$schoolCodes)
      
    } else{
  mapAchPlotBenchmarksByGradeWithComposite(df = mapDemoDataFiltered()$mapGrades,  
                                           subject = subjectName, 
                                           school = mapDemoDataFiltered()$schoolCodes)
    }
  }
  
})
##### Math By Grade and By Student Demo Group -----
output$mapMathDemos <- renderggiraph({

  validate(need(nrow(mapDemoDataFiltered()$mapDemoData) > 0, 'No results reported for this school, due to small testing population'))
  
  subjectName <- 'MATH'
  if(input$mapInput == 'View by Student Group'){
    source('data/map/mapFunctions/mapDemosFunction.R')
    mapDemosPlotFunction(.df = mapDemoDataFiltered()$mapDemoData, 
                         .contentName = subjectName)
  } else {
    if(input$SchoolID == '9998'){
      mapAchPlotBenchmarksByGradeWithComposite(df = mapDemoDataFiltered()$mapGradesDistrict,  
                                               subject = subjectName, 
                                               school = mapDemoDataFiltered()$schoolCodes)
      
    } else{
      mapAchPlotBenchmarksByGradeWithComposite(df = mapDemoDataFiltered()$mapGrades,  
                                               subject = subjectName, 
                                               school = mapDemoDataFiltered()$schoolCodes)
    }
  }
})

# DIBELS ----
## DIBELS Heading ----

output$DIBELSHeader <- renderText({
  paste0("Acadience Reading <i>(2022-2023)</i>")
})

## DIBELS Trend ggplot ----
output$dibelsTrendggplot <- renderggiraph({
  
  SchoolNumber <- input$SchoolID
  
  validate(need(SchoolNumber %in% unique(dibelsTrend$CDESchoolNumber), 'Results for this assessment are not reported for the selected school.'))
  
  
  # Create variable for table with all possible combinations
  dibelsprof <- tibble::tribble(
    ~profBOY,         ~profEOY,
    "At or Above", "At or Above",
    "Below or Well Below", "Below or Well Below")
  
  CDESchoolNumber <- unique(dibelsTrend$CDESchoolNumber)
  
  EndYear <- unique(dibelsTrend$EndYear)
  
  # Create table with all possible combinations
  dibelsprof_all <- expand.grid('profBOY' = dibelsprof$profBOY,
                                'profEOY' = dibelsprof$profEOY,
                                CDESchoolNumber = CDESchoolNumber,
                                EndYear = EndYear)
  
  # combine all possible combinations with actual results to form complete table for schools with missing data
  dibelsTrendCombinedRemoveMissing <- dibelsTrend %>%
    full_join(dibelsprof_all, by = c("profBOY", "profEOY", "CDESchoolNumber", "EndYear")) %>%
    ungroup() %>% 
    arrange(profBOY)
  
  
  #function for imputing NA data
  handleMissingValues <- function(.EndYear = '2018-2019') {
    
    dibelsTrendFiltered <- dibelsTrendCombinedRemoveMissing  %>% 
      filter(CDESchoolNumber == SchoolNumber) %>%
      filter(EndYear == .EndYear) %>%
      filter(!is.na(EndYear)) %>%
      arrange(EndYear)
    
    dibelsTrendFiltered$sum[is.na(dibelsTrendFiltered$sum)] <- 0
    dibelsTrendFiltered$totaln[is.na(dibelsTrendFiltered$totaln)] <- dibelsTrendFiltered$totaln
    dibelsTrendFiltered$totalNBoy[is.na(dibelsTrendFiltered$totalNBoy)] <- dibelsTrendFiltered$totalNBoy
    dibelsTrendFiltered$totalNEoy[is.na(dibelsTrendFiltered$totalNEoy)] <-  dibelsTrendFiltered$totalNEoy
    dibelsTrendFiltered$totalInBoyPB[is.na(dibelsTrendFiltered$totalInBoyPB)] <- first(dibelsTrendFiltered$totalInBoyPB)
    dibelsTrendFiltered$totalInEoyPB[is.na(dibelsTrendFiltered$totalInEoyPB)] <- nth(dibelsTrendFiltered$totalInEoyPB, 4)
    dibelsTrendFiltered$percentInBoyPB[is.na(dibelsTrendFiltered$percentInBoyPB)] <- first(dibelsTrendFiltered$percentInBoyPB)
    dibelsTrendFiltered$percentInEoyPB[is.na(dibelsTrendFiltered$percentInEoyPB)] <- nth(dibelsTrendFiltered$percentInEoyPB, 4)
    dibelsTrendFiltered$percent[is.na(dibelsTrendFiltered$percent)] <- '0%'
    
    dibelsTrendFiltered
  }
  
  # Combine results of function
  
  dibelsTrendCombined <-   map_df(EndYear, handleMissingValues)
  
  # establish plot theme
  dibelsTrendTheme <- theme_minimal()+
    theme(axis.text.y = element_blank(), 
          axis.title.y = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          legend.position = 'top', 
          legend.text = element_text(size = 12#, #_interactive
                                                 # data_id = 'legend.text', 
                                                 # tooltip = 'At or Above | Below or Well Below', 
                                                 # hover_css = "fill:#22a783;stroke:none;"
                                     ),
          legend.title = element_text(size = 14#, #_interactive
                                                  # data_id = 'legend.title', 
                                                  # tooltip = 'Performance Level',
                                                  # hover_css = "fill:#22a783;stroke:none;"
                                                  ),
          axis.text.x = element_text(size = 16#,  # _interactive 
                                                 # data_id = "axis.text.x",
                                                 # tooltip = "Beginning of Year | End of Year",
                                                 # hover_css = "fill:#22a783;stroke:none;"
                                                 ), 
          strip.text = element_text(size = 18), 
          strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
          plot.caption = element_text(hjust = -.001, vjust = 1, size = 12, color = '#808080'#,  _interactive
                                                  # data_id = 'plot.caption', 
                                                  # tooltip = '*2020 unavailable due to shift to remote learning in March 2020 \n**Results for students without scores in Beginning of Year and End of Year are withheld', 
                                                  # 
                                                  # 
                                                  # hover_css = "fill:#22a783;stroke:none;"
                                                  )
    )
  
  #source function for trend plots
  source('data/dibels/dibelsFunctions/dibelsTrendPlotFunction.R', local = TRUE)
  
  
  if (SchoolNumber == '9998'){ #district only - Need to control font size due to large number of students in district aggregate
    
    dibelsTrendPlot(.fontSize = 4)
    
  } else if (SchoolNumber == '6139') { #Mt Phoenix
    dibelsTrendCombined <- dibelsTrendCombined %>% 
      filter(EndYear != '2019')
    
    p <-  ggplot(dibelsTrendCombined,
                 aes(y = sum,
                     axis1 = str_wrap(profBOY, 10),
                     axis2 = str_wrap(profEOY, 10),
                     fill = profBOY
                 )) +
      geom_flow(color = '#e57a3c', curve_type = 'quintic') +
      scale_x_discrete(limits = c("Beginning \nof Year", "End \nof Year")) +
      scale_fill_manual(values = c("#315683","#6c7070"), 
                        na.value = NA) +
      geom_stratum(aes(fill = profEOY), color = 'grey', width = 2/3) +
      geom_stratum(aes(fill = profBOY), color = 'grey', width = 2/3) +
      geom_text(stat = 'stratum', aes(label = paste0(percentInBoyPB, '%')), vjust = 1, size = 6, color = 'white')+
      geom_text(stat = 'stratum', aes(label = paste0(percentInEoyPB, '%')), vjust = 1, size = 6, color = 'white')+
      labs(fill = 'Performance Level', 
           caption= '*2019-2020 unavailable due to shift to remote learning in March 2020\n**2018-2019 unavailable  due to data anomaly\n***Results for students without scores in Beginning of Year and End of Year are withheld')+
      facet_wrap(vars(factor(EndYear)), 
                 nrow = 1, 
                 scales = 'free_y')+
      theme_minimal()+
      dibelsTrendTheme +
      theme(plot.caption = element_text(hjust = -.001, vjust = 1, size = 12, color = '#808080'#_interactive, 
                                                    # data_id = 'plot.caption', 
                                                    # tooltip =  '*2019-2020 unavailable due to shift to remote learning in March 2020\n**2018-2019 unavailable due to data anomaly\n***Results for students without scores in Beginning of Year and End of Year are withheld', 
                                                    # hover_css = "fill:#22a783;stroke:none;"
                                        )
            )
    
    
    ggiraph(code = print(p), 
            width_svg = 10, tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
    
    
  } else { #All other schools in dataset
    
    dibelsTrendPlot(.fontSize = 5)
    
  }
})


output$dibelsTrendTable <- render_gt({
  SchoolNumber <-  input$SchoolID
  
  dibelsTableData <- dibelsTrend%>% 
    filter(CDESchoolNumber == SchoolNumber) %>% 
    arrange(desc(EndYear)) %>% 
    group_by(EndYear) %>% 
    select(profBOY, profEOY, sum) %>% 
    mutate(sum = prettyNum(sum, big.mark = ",")) %>% 
    mutate(plDescription = case_when(
      profBOY == 'At or Above' & profEOY == 'At or Above' ~ 'Remain in At or Above', 
      profBOY == 'At or Above' & profEOY == 'Below or Well Below' ~ 'Move down from At or Above to Below or Well Below', 
      profBOY == 'Below or Well Below' & profEOY == 'Below or Well Below' ~ 'Remain in Below or Well Below', 
      profBOY == 'Below or Well Below' & profEOY == 'At or Above' ~ 'Move up from Below or Well Below to At or Above', 
    )) %>% 
    select(plDescription, sum) %>% 
    arrange(EndYear, plDescription) %>% 
    filter(str_detect(plDescription, 'Move')) 
  
  gt(dibelsTableData, 
     auto_align = F) %>% 
    cols_label(sum = md(''), 
               plDescription = 'Student Movement Between Levels') %>% 
    cols_align(align = 'left', 
               columns = plDescription) %>% 
    cols_width(plDescription ~ px(250)) %>% 
    tab_options(table.font.size = 12, 
                row_group.font.weight = 'bold') %>% 
    tab_style(
      style = list(
        cell_fill(color = "#98AAC1")
      ),
      locations = cells_body(
        columns = c(plDescription, sum),
        rows = str_detect(plDescription, 'Move down')
      )
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "lightgrey")
      ),
      locations = cells_body(
        columns = c(plDescription, sum),
        rows = str_detect(plDescription, 'Move up')
      )
    )
})

### DIBELS Demos & Grade Levels----
#### Reactive ----
dibelsDemosFiltered <- reactive({
  schoolNumber <- input$SchoolID
  
  m1 <- dibelsDemos %>% 
    filter(cdeSchoolNumber == schoolNumber, 
           profFlag ==1, 
           proficiencyDescription == 'Benchmark', 
           subcategory %in% c("all", "English Language Learner", "Free or Reduced Lunch Eligible",
                              "Gifted and Talented Program", "Individualized Education Program",
                              "No Gifted and Talented Program", "No Individualized Education Program",
                              "Not Engilsh Language Learner", "Not Free or Reduced Lunch Eligible", 
                              "Students of Color of Hispanic", "White")) 
  
  
  dibelsDemoFiller <- data.frame(subcategory = c("all", 
                                                 "Free or Reduced Lunch Eligible",
                                                 "Not Free or Reduced Lunch Eligible", 
                                                 "English Language Learner", 
                                                 "Not Engilsh Language Learner", 
                                                 "Individualized Education Program",
                                                 "No Individualized Education Program",
                                                 "Students of Color of Hispanic", 
                                                 "White",    
                                                 "Gifted and Talented Program", 
                                                 "No Gifted and Talented Program"), 
                                 category = c("All Students", 
                                              rep("Free or Reduced Lunch Eligible Students", 2), 
                                              rep("Students in Enlish Langauge Learner Program", 2),
                                                  rep("Students with Individual Education Program", 2),
                                                      rep("Student Race or Ethnicity", 2), 
                                                          rep("Students in Gifted and Talented Program", 2)
                                 ))
  
  m1 <- dibelsDemoFiller %>% 
    left_join(m1) %>% 
    mutate(subcategory = factor(subcategory,
                                                  levels = c("all",
                                                             "Free or Reduced Lunch Eligible",
                                                             "Not Free or Reduced Lunch Eligible",
                                                             "English Language Learner",
                                                             "Not Engilsh Language Learner",
                                                             "Individualized Education Program",
                                                             "No Individualized Education Program",
                                                             "Students of Color of Hispanic",
                                                             "White",
                                                             "Gifted and Talented Program",
                                                             "No Gifted and Talented Program" ),
                                                  labels = c("All Students",
                                                             "Free or Reduced Lunch Eligible",
                                                             "Not Free or Reduced Lunch Eligible",
                                                             "English Language Learner",
                                                             "Not In Engilsh Language Learner Program",
                                                             "Individualized Education Program",
                                                             "Not In Individualized Education Program",
                                                             "Students of Color of Hispanic",
                                                             "White Students",
                                                             "Gifted and Talented Program",
                                                             "Not In Gifted and Talented Program"),
                                                  ordered = T)) %>%
    mutate(category = factor(category, 
                             levels = c("All Students", 
                                        "Free or Reduced Lunch Eligible Students", 
                                        "Students in Enlish Langauge Learner Program",
                                        "Students with Individual Education Program",
                                        "Student Race or Ethnicity", 
                                        "Students in Gifted and Talented Program"
                             ), 
                             ordered = T)) %>% 
    arrange(category, subcategory) %>% 
    mutate(across(where(~ anyNA(.) & is.numeric(.)), ~ replace_na(., 0))) %>% 
    mutate(pctMeetExceedLabel = case_when(
      pctMeetExceed > 0 ~ paste0(round(pctMeetExceed * 100), '%'), 
      TRUE ~ '< 16 students'
    ))
  
  m2 <- dibelsDemos %>% 
    filter(cdeSchoolNumber == schoolNumber| cdeSchoolNumber == '9998', 
           profFlag ==1, 
           proficiencyDescription == 'Benchmark', 
           subcategory %in% c("all", '0', '1', '2', '3')) %>% 
    mutate(subcategory = factor(subcategory, 
                                levels = c('all', '0', '1', '2', '3'),
                                labels = c("All Students", 
                                           'Kindergarten', 
                                           '1st Grade', 
                                           '2nd Grade', 
                                           '3rd Grade'), 
                                ordered = T)) %>% 
    mutate(percentAtAboveSuppressed = case_when(
             subcategoryN < 16 ~ '< 16 students', 
             TRUE~ as.character(paste0(round(pctMeetExceed*100), '%'))))
  
  return(list('dibelsDemoData' = m1, 
              'dibelsGrade' = m2))
  }
)

### DIBELS Demos Plot ----
output$dibelsDemoPlots <- renderggiraph({
 
 filteredDibels <-  dibelsDemosFiltered()$dibelsDemoData
 
 gradeDibels <- dibelsDemosFiltered()$dibelsGrade

  themeCode <-      
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          plot.title = element_text(size = 24, 
                                    face = 'bold',
                                    color = '#315683', 
                                    hjust = 0),
          axis.text.x = element_blank(), 
          axis.text.y.left = element_text(size  = 22#, #_interactive
                                                      # hjust = 0,
                                               #        data_id = 'axisYText',
                                               #        tooltip = unique(filteredDibels$category),
                                               # hover_css = "fill:#22a783;stroke:none;"
                                          ),
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.text = element_text(size = 24, #_interactive
                                                # data_id = 'stripText', 
                                                # tooltip = 'Grade Levels', 
                                               hjust = 0, 
                                               face ='bold'#,
                                               # hover_css = "fill:#22a783;stroke:none;"
                                               ),
          strip.placement = 'outside',
          strip.background = element_rect(fill = 'white', color = 'white'),
          panel.background = element_rect(fill = '#EBF1F6',
                                          colour = '#EBF1F6'),
          panel.spacing.y=unit(.2, "lines"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          plot.caption = element_text(size = 30),
          legend.position = 'none')
  
  

  if(input$dibelsGroupInput == 'View by Student Group'){
    # validate(need(nrow(filteredDibels) > 0), 'No data reported for this school')
    
    if(input$SchoolID %in% filteredDibels$cdeSchoolNumber){
 p <-  ggplot(data = filteredDibels,
         mapping = aes(
           x = fct_rev(subcategory),
                       y = pctMeetExceed, 
                       fill = subcategory)) +
    geom_col_interactive( #_interactive
      aes(tooltip = paste0('<b>Percentage At or Above Benchmark: </b>',
                                    round(pctMeetExceed*100), '%',
                                    '<br>' , 
                           scales::comma(profN, accuracy = 1), 
                           ' out of ', 
                           scales::comma(as.numeric(subcategoryNLabel), accuracy = 1), 
                           ' students'
                           )
          )
      ) +
   geom_text(aes(label = pctMeetExceedLabel), 
             size = 10, 
             hjust = -0.1) +
   scale_fill_manual(values = c('#315683',
                                '#315683', 'grey',
                                '#315683', 'grey',
                                '#315683', 'grey',
                                '#315683', 'grey',
                                '#315683', 'grey')) +
    facet_wrap(~category,
               ncol = 1, 
               scales = 'free') +
    coord_flip(expand = F)+
   ylim(0, 1.2) +
   themeCode
  
 ggiraph(
   code = print(p),
   tooltip_opacity = 1,
   tooltip_offx = 50,
   tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
   hover_css = "cursor:pointer;stroke:#EA9563;",
   width = 1,
   width_svg = 14,
   height_svg = 8)
    } else {
      p <- ggplot()+
        annotate('text', x = 2017, y = 50, 
                 label = 'No school \nresults \navailable', size = 20, 
                 color = 'darkgrey')+
        ylim(c(0,100))+
        scale_fill_manual(values = c("lightblue", "lightgrey"))+
        theme_minimal() +
        theme(axis.text = element_blank(), 
              axis.title = element_blank(), 
              panel.grid = element_blank())
      
      ggiraph(print(p), 
              width_svg = 15,
              height_svg = 5)
    }
  } else {
    
       source('data/dibels/dibelsFunctions/dibelsPlottingFunction.R', local = TRUE)
    
    if(input$SchoolID %in% gradeDibels$cdeSchoolNumber & input$SchoolID != '9998'){
      
      gradesInSchool <- gradeDibels %>% # 
        filter(school == 'School') %>% 
        ungroup() %>% 
        distinct(subcategory) %>% 
        pull()
      
      dibelsBySchool <-  gradeDibels %>% #gradeDibels
        filter(subcategory %in% c(gradesInSchool))
      
      dibelsPlotting()
      
    }else if (input$SchoolID == '9998'){
      dibelsPlotting()
    }else{
      p <- ggplot()+
        annotate('text', x = 2017, y = 50, 
                 label = 'No school \nresults \navailable', size = 20, 
                 color = 'darkgrey')+
        ylim(c(0,100))+
        scale_fill_manual(values = c("lightblue", "lightgrey"))+
        theme_minimal() +
        theme(axis.text = element_blank(), 
              axis.title = element_blank(), 
              panel.grid = element_blank())
      
      ggiraph(print(p), 
              width_svg = 15,
              height_svg = 5)
    }
  }
})
  