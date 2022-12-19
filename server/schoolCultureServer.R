# (2) School Culture Tab ----
## Link to Culture App ----
output$schoolCultureHelper <- renderUI({
  
  SchoolNumber <- input$SchoolID
  
  schoolCode <- masterSchoolProfileData %>%
    filter(cdeSchoolCode == SchoolNumber)%>% 
    ungroup() %>% 
    distinct(cdeSchoolCode) %>% 
    select(cdeSchoolCode) %>% 
    pull()
  
  link <- paste0('https://jeffcopublicschools.shinyapps.io/SchoolCulture/?SchoolID=', schoolCode) 
  
  if (SchoolNumber != '9998'){
    a(
      # "School Culture Dashboard", 
      target = "_blank", 
      tags$img(src="culture-buttonDepth.png", 
               alt="School Culture", 
               style="width:10%;height:10%;box-shadow: inset 0 0 2px 0 rgba(255,255,255,.04), 
                inset 0 0 3px 0 rgba(0,0,0,.04), 
                inset 0 0 3px 0px rgba(0,0,0,0), 
                2px 2px 4px 0 rgba(0,0,0,0);
"), 
      href = link)
  } else {
    a(
      # "School Culture Dashboard", 
      target = "_blank", 
      tags$img(src="culture-buttonDepth.png", 
               alt="School Culture", 
               style="width:10%;height:10%;box-shadow: inset 0 0 2px 0 rgba(255,255,255,.04), 
                inset 0 0 3px 0 rgba(0,0,0,.04), 
                inset 0 0 3px 0px rgba(0,0,0,0), 
                2px 2px 4px 0 rgba(0,0,0,0);
"), 
      href = 'https://jeffcopublicschools.shinyapps.io/SchoolCulture')
  }
})

output$schoolCultureLinkTlcc <- renderUI({
  
     SchoolNumber <- input$SchoolID
    
    schoolCode <- masterSchoolProfileData %>%
      filter(cdeSchoolCode == SchoolNumber)%>% 
      ungroup() %>% 
      distinct(cdeSchoolCode) %>% 
      pull()
    
    link <- paste0('https://jeffcopublicschools.shinyapps.io/SchoolCulture/?SchoolID=', schoolCode) 
    
    if (SchoolNumber != '9998'){
      a("Click here for each question", target = "_blank", href = link)
    } else {
      a("Click here for each question", target = "_blank", href = 'https://jeffcopublicschools.shinyapps.io/SchoolCulture')
    }
})

output$schoolCultureLinkFsp <- renderUI({
  
  SchoolNumber <- input$SchoolID
  
  schoolCode <- masterSchoolProfileData %>%
    filter(cdeSchoolCode == SchoolNumber)%>% 
    ungroup() %>% 
    distinct(cdeSchoolCode) %>% 
    pull()
  
  link <- paste0('https://jeffcopublicschools.shinyapps.io/SchoolCulture/?SchoolID=', schoolCode) 
  
  if (SchoolNumber != '9998'){
    a("Click here for each question and group", target = "_blank", href = link)
  } else {
    a("Click here for each question and group", target = "_blank", href = 'https://jeffcopublicschools.shinyapps.io/SchoolCulture')
  }
})

output$schoolCultureLinkMyvh <- renderUI({
  
  SchoolNumber <- input$SchoolID
  
  schoolCode <- masterSchoolProfileData %>%
    filter(cdeSchoolCode == SchoolNumber)%>% 
    ungroup() %>% 
    distinct(cdeSchoolCode) %>% 
    pull()
  
  link <- paste0('https://jeffcopublicschools.shinyapps.io/SchoolCulture/?SchoolID=', schoolCode) 
  
  if (SchoolNumber != '9998'){
    a("Click here for each question and group", target = "_blank", href = link)
  } else {
    a("Click here for each question and group", target = "_blank", href = 'https://jeffcopublicschools.shinyapps.io/SchoolCulture')
  }
})
## Make Your Voice Heard Student Survey ----
### Overall Favorability ----
output$MYVHOverallFavor <- renderUI({
  
  SchoolNumber <- input$SchoolID
  
  SchoolYear <- input$yearMyvh
  
  myvhOverallFiltered <- myvhOverall %>% 
    filter(cdeSchoolNumber == SchoolNumber) %>% 
    filter(endYear == SchoolYear) %>% 
    filter(n_respondents >15) %>% 
    mutate(percent_pos= round(percent_pos*100)) %>% 
    mutate(percent_pos = case_when(
      percent_pos == 'NaN' ~ '-', 
      TRUE ~ as.character(percent_pos))) %>% 
    mutate(percent_pos = paste0(percent_pos, '%'))
  
  if (SchoolNumber %in% myvhOverallFiltered$cdeSchoolNumber) {
    descriptionBlock(
      number = '', 
      numberColor = "aqua", 
      numberIcon =NULL,
      header = first(myvhOverallFiltered$percent_pos), 
      text = "Overall Favorability", 
      rightBorder = FALSE,
      marginBottom = FALSE
    )
  } else if (SchoolNumber == '9998'){
    myvhDistrict <-   myvhTrendDistrictYear %>% 
      filter(endYear == SchoolYear)
    
    descriptionBlock(
      number = '', 
      numberColor = "aqua", 
      numberIcon =NULL,
      header = paste0(round(myvhDistrict$percent_pos*100), '%'),
      text = "Overall Favorability", 
      rightBorder = FALSE,
      marginBottom = FALSE
    )
  } else{
    
  }
})

### MYVH Table  ----
output$MYVHTableServer <- render_gt(
  width = px(500),
  {
    
    SchoolNumber <- input$SchoolID
    
    myvhLevels <-  c('Staff-Student Relationships', 
                     'School Connection', 
                     'Family Support for Learning', 
                     'Perceptions of Discipline Practices',
                     'Perceptions of Safety', 
                     'Future Aspirations', 
                     'Academic Challenge', 
                     'Teacher Feedback',  
                     'Attendance and Engagement')
    
    myvhLabels <- c('Staff-Student Relationships', 
                    'School Connection', 
                    'Family Support for Learning', 
                    'Discipline Practices', 
                    'Safety', 
                    'Future Aspirations', 
                    'Academic Challenge', 
                    'Teacher Feedback',  
                    'Attendance & Engagement')
    
    myvhColors <- c('#F6F6F6', 
                    '#F6F6F6', 
                    '#F6F6F6', 
                    '#FFFFFF', 
                    '#FFFFFF', 
                    '#FFFFFF', 
                    '#F6F6F6', 
                    '#F6F6F6', 
                    '#F6F6F6')
    
    MYVH1 <- myvhTableAllYears %>% 
      filter(cdeSchoolNumber == SchoolNumber) %>% 
      filter(endYear == input$yearMyvh) %>%  #input$yearMyvh
      arrange(classificationName) %>% 
      ungroup() %>% 
      dplyr::select(-cdeSchoolNumber, -endYear) %>% 
      mutate(Elem = paste0(as.numeric(Elem), '%'), 
             Mid = paste0(as.numeric(Mid), '%'), 
             High = paste0(as.numeric(High), '%'), 
             DistrictEl = paste0(as.numeric(DistrictEl), '%'), 
             DistrictMid = paste0(as.numeric(DistrictMid), '%'), 
             DistrictHi = paste0(as.numeric(DistrictHi), '%')) %>% 
      ungroup() %>% 
      mutate(classificationName = factor(classificationName, 
                                         levels = myvhLevels, 
                                         labels = myvhLabels, 
                                         ordered = TRUE)) %>% 
      arrange(classificationName) %>% 
      mutate(constructName = case_when(
        classificationName == 'Staff-Student Relationships' | 
          classificationName == 'School Connection' |
          classificationName == 'Family Support for Learning' 
        ~ 'Affective', 
        classificationName == 'Discipline Practices' |
          classificationName == 'Safety'|
          classificationName == 'Future Aspirations' 
        ~ 'Behavioral', 
        classificationName == 'Academic Challenge' |
          classificationName == 'Teacher Feedback' |
          classificationName == 'Attendance & Engagement'
        ~ 'Cognitive')) %>% 
      dplyr::select(constructName, everything())
    
    MYVH1[is.na(MYVH1)] <- '-'
    
    myvhOverallA <- MYVHResponseRates %>% 
      filter(cdeSchoolNumber == SchoolNumber) %>% 
      filter(year == input$yearMyvh) 
    
    MYVHDistrictElem <- myvhTableAllYears %>% 
      ungroup() %>% 
      group_by(classificationName, endYear) %>% 
      arrange(desc(DistrictEl)) %>% 
      summarise(DistrictEl = first(DistrictEl))
    
    MYVHDistrictMid <- myvhTableAllYears %>% 
      ungroup() %>% 
      group_by(classificationName, endYear) %>% 
      arrange(desc(DistrictMid)) %>% 
      summarise(DistrictMid = first(DistrictMid)) %>% 
      ungroup() %>% 
      dplyr::select(DistrictMid)
    
    MYVHDistrictHi <- myvhTableAllYears %>% 
      ungroup() %>% 
      group_by(classificationName, endYear) %>% 
      arrange(desc(DistrictHi)) %>% 
      summarise(DistrictHi = first(DistrictHi)) %>% 
      ungroup() %>% 
      dplyr::select(DistrictHi)
    
    MYVHDistrict <- MYVHDistrictElem %>%  
      bind_cols(MYVHDistrictMid) %>% 
      bind_cols(MYVHDistrictHi) %>% 
      filter(endYear == input$yearMyvh) %>% 
      arrange(classificationName) %>% 
      mutate(DistrictEl = paste0(as.numeric(DistrictEl), '%'), 
             DistrictMid = paste0(as.numeric(DistrictMid), '%'), 
             DistrictHi = paste0(as.numeric(DistrictHi), '%')) %>% 
      ungroup() %>% 
      mutate(classificationName = factor(classificationName, 
                                         levels = myvhLevels, 
                                         labels = myvhLabels, 
                                         ordered = TRUE)) %>% 
      arrange(classificationName) %>% 
      mutate(constructName = case_when(
        classificationName == 'Staff-Student Relationships' | 
          classificationName == 'School Connection' |
          classificationName == 'Family Support for Learning' 
        ~ 'Affective', 
        classificationName == 'Discipline Practices' |
          classificationName == 'Safety'|
          classificationName == 'Future Aspirations' 
        ~ 'Behavioral', 
        classificationName == 'Academic Challenge' |
          classificationName == 'Teacher Feedback' |
          classificationName == 'Attendance & Engagement'
        ~ 'Cognitive')) %>% 
      dplyr::select(constructName, classificationName, DistrictEl, DistrictMid, DistrictHi)
    
    MYVHE <- MYVH1 %>% 
      summarise(number_nas = sum(Elem == "0%"))
    
    MYVHM <- MYVH1 %>% 
      summarise(number_nas = sum(Mid == "0%"))
    
    MYVHH <- MYVH1 %>% 
      summarise(number_nas = sum(High == "0%"))
    
    
    validate(need(nrow(MYVH1) > 0 || SchoolNumber == '9998', "Data redacted, fewer than 16 responses"))
    
    #function for tables
    myvhTableFunction <- function(df = MYVH1, 
                          level = "High", #needs to be unquoted- column name
                          districtLevel = 'DistrictHi', #needs to be unquoted- column name
                          levelFlag = 'HighFlag', #needs to be unquoted- column name
                          .label = 'Grades 9-12') {
      df %>% 
        select(constructName, classificationName, .data[[level]], .data[[districtLevel]], .data[[levelFlag]]) %>% 
        gt(groupname_col = 'constructName', 
           auto_align = F) %>% 
        tab_options(row_group.background.color = "#FFEFDB80", 
                    row_group.padding = 1,
                    table.font.size = 12, 
                    table.align = 'left', 
                    footnotes.font.size = 13) %>% 
        tab_spanner(label = .label, 
                    columns = c(.data[[level]], .data[[districtLevel]])) %>% 
        cols_align(align = "left", columns = 'classificationName') %>% 
        tab_style(style = cell_fill(color = '#eeca87'),
                  locations = cells_body(columns=c(.data[[level]]),
                                         rows = .data[[levelFlag]] == 1)) %>% 
        tab_style(style = cell_fill(color = '#c5c5c5'),
                  locations = cells_body(columns=c(.data[[level]]),
                                         rows = .data[[levelFlag]] == 3)) %>%
        tab_style(style = cell_fill(color = '#4EB89B'),
                  locations = cells_body(columns=c(.data[[level]]),
                                         rows = .data[[levelFlag]] == 2)) %>% 
        cols_hide(columns = c(.data[[levelFlag]]))  %>% 
        tab_footnote(footnote = paste0(myvhOverallA$percentStarted, ' of ',
                                       first(myvhOverallA$totalStudents), ' students participated'),
                     locations = cells_column_labels(columns = c(classificationName))) #https://github.com/rstudio/gt/issues/702 known issue with cols_name
    }

      
   #### Elementary Middle High ----
   if (MYVHE$number_nas == 0 & MYVHM$number_nas == 0 & MYVHH$number_nas == 0 & SchoolNumber != '9998'){
      MYVH1 %>% 
        gt(groupname_col = 'constructName', 
           auto_align = F) %>% 
        tab_options(row_group.background.color = "#FFEFDB80", 
                    row_group.padding = 1, 
                    table.font.size = 10, 
                    table.align = 'left', 
                    footnotes.font.size = 13) %>% 
        tab_spanner(label = 'Grades 2-5', columns = c(Elem, DistrictEl)) %>% 
        tab_spanner(label = 'Grades 6-8', columns = c(Mid, DistrictMid)) %>% 
        tab_spanner(label = 'Grades 9-12', columns = c(High, DistrictHi)) %>% 
        cols_label(classificationName = 'Category', 
                   Elem = 'School', DistrictEl = 'District', 
                   Mid = 'School', DistrictMid = 'District', 
                   High = 'School', DistrictHi = 'District') %>% 
        cols_align(align = "left", columns = 'classificationName') %>% 
        tab_style(style = cell_fill(color = '#eeca87'), 
                  locations = cells_body(columns=c(Elem), 
                                         rows = ElemFlag == 1)) %>% 
        tab_style(style = cell_fill(color = '#c5c5c5'), 
                  locations = cells_body(columns=c(Elem), 
                                         rows = ElemFlag == 3)) %>% 
        tab_style(style = cell_fill(color = '#4EB89B'), 
                  locations = cells_body(columns=c(Elem), 
                                         rows = ElemFlag == 2)) %>% 
        tab_style(style = cell_fill(color = '#eeca87'), 
                  locations = cells_body(columns=c(Mid), 
                                         rows = MidFlag == 1)) %>% 
        tab_style(style = cell_fill(color =  '#c5c5c5'), 
                  locations = cells_body(columns=c(Mid), 
                                         rows = MidFlag == 3)) %>% 
        tab_style(style = cell_fill(color = '#4EB89B'), 
                  locations = cells_body(columns=c(Mid), 
                                         rows = MidFlag == 2)) %>% 
        tab_style(style = cell_fill(color = '#eeca87'), 
                  locations = cells_body(columns=c(High), 
                                         rows = HighFlag == 1)) %>% 
        tab_style(style = cell_fill(color = '#c5c5c5'), 
                  locations = cells_body(columns=c(High), 
                                         rows = HighFlag == 3)) %>% 
        tab_style(style = cell_fill(color = '#4EB89B'), 
                  locations = cells_body(columns=c(High), 
                                         rows = HighFlag == 2)) %>% 
        cols_hide(columns = c(ElemFlag, MidFlag, HighFlag))  %>%
        tab_footnote(footnote = paste0(myvhOverallA$percentStarted, ' of ',
                                       first(myvhOverallA$totalStudents), ' students participated'),
                     locations = cells_column_labels(columns = c(classificationName)))
     #### JVA ----
   } else if (SchoolNumber == '4408' & input$yearMyvh == '2022'){
     
     MYVH1 %>% 
       select(-Elem, -ElemFlag, -Mid, -DistrictMid, -MidFlag, -DistrictEl) %>% 
       gt(groupname_col = 'constructName', 
          auto_align = F) %>% 
       tab_options(row_group.background.color = "#FFEFDB80", 
                   row_group.padding = 1,
                   table.font.size = 10, 
                   table.align = 'left', 
                   footnotes.font.size = 13) %>% 
       # tab_spanner(label = 'Grades 6-8', columns = c(Mid, DistrictMid)) %>% 
       tab_spanner(label = 'Grades 9-12', columns = c(High, DistrictHi)) %>% 
       cols_label(classificationName = 'Category', 
                  # Mid = 'School', DistrictMid = 'District', 
                  High = 'School', DistrictHi = 'District') %>% 
       cols_align(align = "left", columns = 'classificationName') %>% 
       # tab_style(style = cell_fill(color = '#eeca87'), 
       #           locations = cells_body(columns=c(Mid), 
       #                                  rows = MidFlag == 1)) %>% 
       # tab_style(style = cell_fill(color = '#c5c5c5'), 
       #           locations = cells_body(columns=c(Mid), 
       #                                  rows = MidFlag == 3)) %>% 
       # tab_style(style = cell_fill(color = '#4EB89B'), 
       #           locations = cells_body(columns=c(Mid), 
       #                                  rows = MidFlag == 2)) %>% 
       tab_style(style = cell_fill(color = '#eeca87'), 
                 locations = cells_body(columns=c(High), 
                                        rows = HighFlag == 1)) %>% 
       tab_style(style = cell_fill(color = '#c5c5c5'), 
                 locations = cells_body(columns=c(High), 
                                        rows = HighFlag == 3)) %>% 
       tab_style(style = cell_fill(color = '#4EB89B'), 
                 locations = cells_body(columns=c(High), 
                                        rows = HighFlag == 2)) %>% 
       cols_hide(columns = c(HighFlag)) %>% 
       tab_footnote(footnote = paste0(myvhOverallA$percentStarted, ' of ',
                                      first(myvhOverallA$totalStudents), ' students participated'),
                    locations = cells_column_labels(columns = c(classificationName))) %>% 
       tab_source_note('Includes students with secondary enrollment status as of January 6, 2022')
     #### High Schools ----
     } else if (MYVHE$number_nas != 0 & MYVHM$number_nas != 0 & MYVHH$number_nas == 0){ 
      myvhTableFunction() %>% 
         cols_label(classificationName = 'Category', High = 'School', DistrictHi = 'District') 
#### Middle Schools There are no schools with this configuration of 7th and 8th only ----
      } else if (MYVHE$number_nas != 0 & MYVHM$number_nas == 0 & MYVHH$number_nas != 0){
        myvhTableFunction(df = MYVH1, 
                  level = "Mid", #needs to be unquoted- column name
                  districtLevel = 'DistrictMid', #needs to be unquoted- column name
                  levelFlag = 'MidFlag', #needs to be unquoted- column name
                  .label = 'Grades 6-8') %>% 
         cols_label(classificationName = 'Category', Mid = 'School', DistrictMid = 'District') 
        ### Elementary ----
      } else if (MYVHE$number_nas == 0 & MYVHM$number_nas != 0 & MYVHH$number_nas != 0){
        myvhTableFunction(df = MYVH1, 
                  level = "Elem", #needs to be unquoted- column name
                  districtLevel = 'DistrictEl', #needs to be unquoted- column name
                  levelFlag = 'ElemFlag', #needs to be unquoted- column name
                  .label = 'Grades 2-5') %>% 
          cols_label(classificationName = 'Category', Elem = 'School', DistrictEl = 'District') 
        ### Middle with High ----
  } else if (MYVHE$number_nas != 0 & MYVHM$number_nas == 0 & MYVHH$number_nas == 0){
        MYVH1 %>% 
          select(-Elem, -ElemFlag, -DistrictEl) %>% 
          gt(groupname_col = 'constructName', 
             auto_align = F) %>% 
          tab_options(row_group.background.color = "#FFEFDB80", 
                      row_group.padding = 1,
                      table.font.size = 10, 
                      table.align = 'left', 
                      footnotes.font.size = 13) %>% 
          tab_spanner(label = 'Grades 6-8', columns = c(Mid, DistrictMid)) %>% 
          tab_spanner(label = 'Grades 9-12', columns = c(High, DistrictHi)) %>% 
          cols_label(classificationName = 'Category', 
                     Mid = 'School', DistrictMid = 'District', 
                     High = 'School', DistrictHi = 'District') %>% 
          cols_align(align = "left", columns = 'classificationName') %>% 
          tab_style(style = cell_fill(color = '#eeca87'), 
                    locations = cells_body(columns=c(Mid), 
                                           rows = MidFlag == 1)) %>% 
          tab_style(style = cell_fill(color = '#c5c5c5'), 
                    locations = cells_body(columns=c(Mid), 
                                           rows = MidFlag == 3)) %>% 
          tab_style(style = cell_fill(color = '#4EB89B'), 
                    locations = cells_body(columns=c(Mid), 
                                           rows = MidFlag == 2)) %>% 
          tab_style(style = cell_fill(color = '#eeca87'), 
                    locations = cells_body(columns=c(High), 
                                           rows = HighFlag == 1)) %>% 
          tab_style(style = cell_fill(color = '#c5c5c5'), 
                    locations = cells_body(columns=c(High), 
                                           rows = HighFlag == 3)) %>% 
          tab_style(style = cell_fill(color = '#4EB89B'), 
                    locations = cells_body(columns=c(High), 
                                           rows = HighFlag == 2)) %>% 
          cols_hide(columns = c(MidFlag, HighFlag)) %>% 
          tab_footnote(footnote = paste0(myvhOverallA$percentStarted, ' of ',
                                         first(myvhOverallA$totalStudents), ' students participated'),
                       locations = cells_column_labels(columns = c(classificationName)))
     ### District Only ----
     } else if (MYVHE$number_nas == 0 & MYVHM$number_nas == 0 & MYVHH$number_nas == 0 & SchoolNumber == '9998'){
        
        MYVHDistrict %>% 
          gt(groupname_col = 'constructName', 
             auto_align = F) %>% 
          tab_options(row_group.background.color = "#FFEFDB80", 
                      row_group.padding = 1, 
                      table.font.size = 10, 
                      table.align = 'left', 
                      footnotes.font.size = 13) %>% 
          cols_label(classificationName = 'Category', DistrictEl = 'Grades 2-5', DistrictMid = 'Grades 6-8', DistrictHi = 'Grades 9-12') %>% 
          cols_align(align = "left", columns = 'classificationName')
       ### Elementary with Middle ----
      } else if (MYVHE$number_nas == 0 & MYVHM$number_nas == 0 & MYVHH$number_nas != 0){
        
        MYVH1 %>% 
          select(constructName, classificationName, Elem, DistrictEl, Mid, DistrictMid, ElemFlag, MidFlag) %>% 
          gt(groupname_col = 'constructName', 
             auto_align = F) %>% 
          tab_options(row_group.background.color = "#FFEFDB80", 
                      row_group.padding = 1, 
                      table.font.size = 10, 
                      table.align = 'left', 
                      footnotes.font.size = 13) %>% 
          tab_spanner(label = 'Grades 2-5', columns = c(Elem, DistrictEl)) %>% 
          tab_spanner(label = 'Grades 6-8', columns = c(Mid, DistrictMid)) %>% 
          cols_label(classificationName = 'Category', 
                     Elem = 'School', DistrictEl = 'District', 
                     Mid = 'School', DistrictMid = 'District') %>% 
          cols_align(align = "left", columns = 'classificationName') %>% 
          tab_style(style = cell_fill(color = '#eeca87'), 
                    locations = cells_body(columns=c(Elem), 
                                           rows = ElemFlag == 1)) %>% 
          tab_style(style = cell_fill(color = '#c5c5c5'), 
                    locations = cells_body(columns=c(Elem), 
                                           rows = ElemFlag == 3)) %>% 
          tab_style(style = cell_fill(color = '#4EB89B'), 
                    locations = cells_body(columns=c(Elem), 
                                           rows = ElemFlag == 2)) %>% 
          tab_style(style = cell_fill(color = '#eeca87'), 
                    locations = cells_body(columns=c(Mid), 
                                           rows = MidFlag == 1)) %>% 
          tab_style(style = cell_fill(color = '#c5c5c5'), 
                    locations = cells_body(columns=c(Mid), 
                                           rows = MidFlag == 3)) %>% 
          tab_style(style = cell_fill(color = '#4EB89B'), 
                    locations = cells_body(columns=c(Mid), 
                                           rows = MidFlag == 2)) %>% 
          cols_hide(columns = c(ElemFlag, MidFlag)) %>% 
          tab_footnote(footnote = paste0(myvhOverallA$percentStarted, ' of ',
                                         first(myvhOverallA$totalStudents), ' students participated'),
                       locations = cells_column_labels(columns = c(classificationName)))
      }
    
  })
### MYVH Trend Plot ----
output$myvhTrendPlot <- renderPlot({
  
  SchoolNumber <- input$SchoolID
  
  yearFiller <- data.frame('endYear' = c('2020', '2021', '2022'))
  
  myvhTrendYear <- myvhTrendYear %>% 
    filter(cdeSchoolNumber == SchoolNumber) %>% 
    filter(n_respondents >15) %>% 
    full_join(yearFiller) 
  
  myvhTheme <- theme(axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),
                     axis.text.y = element_blank(),
                     axis.text.x = element_text(size = 15),
                     plot.caption = element_text(size = 12),
                     legend.position = c(0.1, 0.89),
                     legend.title = element_blank(),
                     legend.key.size = unit(3, "mm"),
                     legend.spacing.x = unit(1.0, 'mm'),
                     legend.background = element_rect(fill = "white", color = "darkgrey"),
                     panel.background = element_rect(fill = '#EBF1F6', 
                                                     colour = '#EBF1F6'),
                     plot.background = element_rect(fill = '#EBF1F6', 
                                                    colour = '#EBF1F6'),
                     panel.spacing.y=unit(.2, "lines"), 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor.x = element_blank(), 
                     panel.grid = element_blank())
  
  if (SchoolNumber %in% myvhTrendYear$cdeSchoolNumber){
    
    ggplot(data = myvhTrendYear,
           mapping = aes(x = endYear, y = percent_pos))+
      geom_bar(stat = "identity",
               position = 'dodge',
               color = 'darkgrey',
               fill = '#315683')+
      geom_text(data = myvhTrendYear, 
                aes(x = endYear, 
                    y= percent_pos, 
                    label = paste0(round(percent_pos, 2)*100, '%')), 
                position = position_dodge(width = 1), 
                vjust = 1.2, 
                size = 8, 
                color = 'white')+
      ylim(c(0, 1))+
      theme_minimal()+
      myvhTheme
    
  } else if (SchoolNumber == '9998'){
    ggplot(data = myvhTrendDistrictYear,
           mapping = aes(x = endYear, y = percent_pos))+
      geom_bar(stat = "identity",
               position = 'dodge',
               color = 'darkgrey',
               fill = '#315683')+
      geom_text(data = myvhTrendDistrictYear, 
                aes(x = endYear, 
                    y= percent_pos, 
                    label = paste0(round(percent_pos, 2)*100, '%')), 
                position = position_dodge(width = 1), 
                vjust = 1.2, 
                size = 8, 
                color = 'white')+
      ylim(c(0, 1))+
      theme_minimal()+
      myvhTheme
  } else {
    ggplot(data = myvhTrendDistrictYear,
           mapping = aes(x = endYear, y = 1))+
      ylim(c(0, 1))+
      theme_minimal()+
      annotate('text',  y = .5, x = '2020', label  = "No trend data \nfor this site", size = 8, color = '#515151')+
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            legend.position = c(0.1, 0.89),
            legend.title = element_blank(),
            legend.key.size = unit(3, "mm"),
            legend.spacing.x = unit(1.0, 'mm'),
            legend.background = element_rect(fill = "white", color = "darkgrey"),
            panel.background = element_rect(fill = '#EBF1F6', 
                                            colour = '#EBF1F6'),
            plot.background = element_rect(fill = '#EBF1F6', 
                                           colour = '#EBF1F6'),
            panel.spacing.y=unit(.2, "lines"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid = element_blank())
  }
})

# Family School Partnership Survey ----
### Overall Favorability ----
output$FSPOverallFavor <- renderUI({
  SchoolNumber <- input$SchoolID
  inputYear  <- input$fspYear
  
  fspOverallFiltered <- fspOverall %>% 
    filter(cdeSchoolNumber == SchoolNumber) %>% 
    filter(endYear == inputYear)
  
  if (SchoolNumber %in% fspOverallFiltered$cdeSchoolNumber){
    
    descriptionBlock(
      number = "", 
      numberColor = "aqua", 
      numberIcon =NULL,
      header = fspOverallFiltered$OverallFav, 
      text =  "Overall Favorability", 
      rightBorder = FALSE,
      marginBottom = FALSE
    )
  } else {
    
  }
})
### FSP Table  ----
output$OverallConstructsTableServer <- render_gt(   
  width = px(500),
  {
    SchoolNumber <- input$SchoolID
    inputYear  <- input$fspYear
    
    fspBlank <- fspAllYearsQuartiled %>% 
      filter(cdeSchoolNumber == SchoolNumber, 
             endYear == inputYear) %>% 
      ungroup() %>% 
      mutate(School = as.numeric(schConstPctAgree),
             District = as.numeric(distConstPctAgreeAll)) %>%
      select(-cdeSchoolNumber, -schConstPctAgree, -distConstPctAgreeAll, -endYear) %>% 
      mutate(School = paste0(round(School), '%'),
             District = paste0(round(District), '%'))
    
    if (SchoolNumber == '9998') {
      fsp <- fspAllYearsQuartiled %>%
        filter(cdeSchoolNumber == '0951', #any site with data is fine, just pulling district column
               endYear == inputYear) %>%
        ungroup() %>%
        mutate(District = as.numeric(distConstPctAgreeAll)) %>%
        select(-cdeSchoolNumber, -schConstPctAgree, -distConstPctAgreeAll, -endYear, -School, -edlevel) %>%
        mutate(District = paste0(round(District), '%'))
      
      fsp %>%
        gt() %>%
        tab_options(table.font.size = 12,
                    table.align = 'left', 
                    footnotes.font.size = 13) %>%
        cols_align(align = "left", columns = 'classificationName') %>%
        cols_label(classificationName = "Category") %>%
        cols_hide(columns = c(quartFlag, nRespondents, nRespondentsDistrict)) %>% 
        tab_footnote(footnote = paste0('Families responding to one or more questions = ', first(fsp$nRespondentsDistrict)),
                     locations = cells_column_labels(columns = c(District))) %>% 
        tab_footnote(footnote = paste0('Percent of families responding not available'),
                     locations = cells_column_labels(columns = c(District)))
    } else if(SchoolNumber == '4408'){
      fsp <- fspAllYearsQuartiled %>% 
        filter(cdeSchoolNumber == SchoolNumber, 
               endYear == inputYear) %>% 
        ungroup() %>% 
        mutate(School = as.numeric(schConstPctAgree),
               District = as.numeric(distConstPctAgreeAll)) %>%
        select(-cdeSchoolNumber, -schConstPctAgree, -distConstPctAgreeAll, -endYear, -edlevel) %>% 
        mutate(School = paste0(round(School), '%'),
               District = paste0(round(District), '%'))
      
      fsp %>%
        gt() %>%
        tab_options(table.font.size = 12,
                    table.align = 'left', 
                    footnotes.font.size = 13) %>%
        cols_align(align = "left", columns = 'classificationName') %>%
        cols_label(classificationName = "Category") %>%
        tab_style(style = cell_fill(color = '#eeca87'),
                  locations = cells_body(columns=c(School),
                                         rows = quartFlag == 1)) %>%
        tab_style(style = cell_fill(color = '#c5c5c5'),
                  locations = cells_body(columns=c(School),
                                         rows = quartFlag == 3)) %>%
        tab_style(style = cell_fill(color = '#4EB89B'),
                  locations = cells_body(columns=c(School),
                                         rows = quartFlag == 2)) %>%
        cols_hide(columns = c(quartFlag, nRespondents, nRespondentsDistrict)) %>%
        tab_footnote(footnote = paste0('Families responding to one or more questions = ', max(fsp$nRespondents)),
                     locations = cells_column_labels(columns = c(School))) %>% 
        tab_footnote(footnote = paste0('Percent of families responding not available'),
                     locations = cells_column_labels(columns = c(School))) %>% 
        tab_footnote(footnote = paste0('Excludes Jeffo Remote Learning Program'),
                     locations = cells_column_labels(columns = c(School)))
    
    } else if (nrow(fspBlank) == 0 & SchoolNumber != '9998') {
      validate(need(nrow(fspBlank) > 0 || SchoolNumber == '9998', "Data redacted, fewer than 16 responses"))
    } else {
      fsp <- fspAllYearsQuartiled %>% 
        filter(cdeSchoolNumber == SchoolNumber, 
               endYear == inputYear) %>% 
        ungroup() %>% 
        mutate(School = as.numeric(schConstPctAgree),
               District = as.numeric(distConstPctAgreeAll)) %>%
        select(-cdeSchoolNumber, -schConstPctAgree, -distConstPctAgreeAll, -endYear, -edlevel) %>% 
        mutate(School = paste0(round(School), '%'),
               District = paste0(round(District), '%'))
      
      fsp %>%
        gt() %>%
        tab_options(table.font.size = 12,
                    table.align = 'left', 
                    footnotes.font.size = 13) %>%
        cols_align(align = "left", columns = 'classificationName') %>%
        cols_label(classificationName = "Category") %>%
        tab_style(style = cell_fill(color = '#eeca87'),
                  locations = cells_body(columns=c(School),
                                         rows = quartFlag == 1)) %>%
        tab_style(style = cell_fill(color = '#c5c5c5'),
                  locations = cells_body(columns=c(School),
                                         rows = quartFlag == 3)) %>%
        tab_style(style = cell_fill(color = '#4EB89B'),
                  locations = cells_body(columns=c(School),
                                         rows = quartFlag == 2)) %>%
        cols_hide(columns = c(quartFlag, nRespondents, nRespondentsDistrict)) %>%
        tab_footnote(footnote = paste0('Families responding to one or more questions = ', max(fsp$nRespondents)),
                     locations = cells_column_labels(columns = c(School))) %>% 
        tab_footnote(footnote = paste0('Percent of families responding not available'),
                     locations = cells_column_labels(columns = c(School)))
    }
  })
### FSP Trend Plot ----
output$fspTrendPlot <-  renderPlot({
  
  SchoolNumber <- input$SchoolID
  fspTrendYear <- fspOverall %>% 
    filter(cdeSchoolNumber == SchoolNumber) 
  
  if (SchoolNumber == '4408'){
    yearFiller <- data.frame('endYear' = c('2020', '2021', '2022')) 
    
    fspTrendYearB <- fspTrendYear %>% 
      full_join(yearFiller) 
    
    ggplot(data = fspTrendYearB,
           mapping = aes(x = endYear, y = percentPos))+
      geom_bar(stat = "identity", 
               position = 'dodge', 
               color = 'darkgrey', 
               fill = '#315683')+
      geom_text(data = fspTrendYearB, 
                aes(x = endYear, 
                    y= percentPos, 
                    label = paste0(round(percentPos*100, 0),'%')), 
                position = position_dodge(width = 1), 
                vjust = 1.2, 
                size = 8, 
                color = 'white')+
      labs(caption = 'Excludes Jeffco Remote Learning Program') +
      ylim(c(0, 1))+
      theme_minimal()+
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 15),
            legend.position = c(0.1, 0.89),
            legend.title = element_blank(),
            legend.key.size = unit(3, "mm"),
            legend.spacing.x = unit(1.0, 'mm'),
            legend.background = element_rect(fill = "white", color = "darkgrey"),
            panel.background = element_rect(fill = '#EBF1F6', 
                                            colour = '#EBF1F6'),
            plot.background = element_rect(fill = '#EBF1F6', 
                                           colour = '#EBF1F6'),
            plot.caption = element_text(size = 12),
            panel.spacing.y=unit(.2, "lines"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid = element_blank(), 
            strip.text = element_text(size = 14), 
            strip.background = element_rect(color = 'lightblue'))
  } else if (SchoolNumber == '9998') {
    
    ggplot(data = fspTrendYear,
           mapping = aes(x = endYear, y = percentPos))+
      geom_bar(stat = "identity", 
               position = 'dodge', 
               color = 'darkgrey', 
               fill = '#315683')+
      geom_text(data = fspTrendYear, 
                aes(x = endYear, 
                    y= percentPos, 
                    label = paste0(round(percentPos*100, 0),'%')), 
                position = position_dodge(width = 1), 
                vjust = 1.2, 
                size = 8, 
                color = 'white')+
      ylim(c(0, 1))+
      theme_minimal()+
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 15),
            legend.position = c(0.1, 0.89),
            legend.title = element_blank(),
            legend.key.size = unit(3, "mm"),
            legend.spacing.x = unit(1.0, 'mm'),
            legend.background = element_rect(fill = "white", color = "darkgrey"),
            panel.background = element_rect(fill = '#EBF1F6', 
                                            colour = '#EBF1F6'),
            plot.background = element_rect(fill = '#EBF1F6', 
                                           colour = '#EBF1F6'),
            plot.caption = element_text(size = 12),
            panel.spacing.y=unit(.2, "lines"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid = element_blank())
  } else if (SchoolNumber %in% unique(fspTrendYear$cdeSchoolNumber)) {
    yearFiller <- data.frame('endYear' = c('2020', '2021', '2022')) 
    
    fspTrendYearB <- fspTrendYear %>% 
      full_join(yearFiller) 
    
    ggplot(data = fspTrendYearB,
           mapping = aes(x = endYear, y = percentPos))+
      geom_bar(stat = "identity", 
               position = 'dodge', 
               color = 'darkgrey', 
               fill = '#315683')+
      geom_text(data = fspTrendYearB, 
                aes(x = endYear, 
                    y= percentPos, 
                    label = paste0(round(percentPos*100, 0),'%')), 
                position = position_dodge(width = 1), 
                vjust = 1.2, 
                size = 8, 
                color = 'white')+
      ylim(c(0, 1))+
      # labs(caption = 'Excludes Jeffco Remote Learning Program') +
      theme_minimal()+
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 15),
            legend.position = c(0.1, 0.89),
            legend.title = element_blank(),
            legend.key.size = unit(3, "mm"),
            legend.spacing.x = unit(1.0, 'mm'),
            legend.background = element_rect(fill = "white", color = "darkgrey"),
            panel.background = element_rect(fill = '#EBF1F6', 
                                            colour = '#EBF1F6'),
            plot.background = element_rect(fill = '#EBF1F6', 
                                           colour = '#EBF1F6'),
            plot.caption = element_text(size = 12),
            panel.spacing.y=unit(.2, "lines"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid = element_blank(), 
            strip.text = element_text(size = 14), 
            strip.background = element_rect(color = 'lightblue'))
    
  } else {
    ggplot(data = fspTrendYear,
           mapping = aes(x = 1, y = 1))+
      geom_bar(stat = "identity", 
               position = 'dodge', 
               color = '#EBF1F6', 
               fill = '#EBF1F6')+
      ylim(c(0, 1))+
      theme_minimal()+
      annotate('text',  y = .5, x = 1, label  = "No trend data \nfor this site", size = 8, color = '#515151')+
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            legend.position = c(0.1, 0.89),
            legend.title = element_blank(),
            legend.key.size = unit(3, "mm"),
            legend.spacing.x = unit(1.0, 'mm'),
            legend.background = element_rect(fill = "#515151", color = "#515151"),
            panel.background = element_rect(fill = '#EBF1F6', 
                                            colour = '#EBF1F6'),
            plot.background = element_rect(fill = '#EBF1F6', 
                                           colour = '#EBF1F6'),
            panel.spacing.y=unit(.2, "lines"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid = element_blank())
  }
})
# Teaching and Learning Conditions in Colorado Survey ----
### Overall Favorability ----
output$TLCCOverallFavor <- renderUI({
  
  SchoolNumber <- input$SchoolID
  
  YearTlccInput <- input$yearTlcc
  
  TLCCAll <- tlccTrend %>% 
    mutate(OverallFav = paste0(round(percentAgreementOverallFav * 100, 0), "%")) 
  
  TLCCoverall <- TLCCAll %>% 
    filter(CDESchoolNumber == SchoolNumber) %>% 
    filter(year == YearTlccInput) 
  
  if (SchoolNumber %in% TLCCoverall$CDESchoolNumber){
    
    descriptionBlock(
      number = "", 
      numberColor = "aqua", 
      numberIcon =NULL,
      header = TLCCoverall$OverallFav, 
      text =  "Overall Favorability", 
      rightBorder = FALSE,
      marginBottom = FALSE)
    
  } else if (SchoolNumber == '9998'){
    
    tlccDistrict <- TLCCQuartiles %>% 
      filter(cdeSchoolNumber == '0109') %>% #any school is fine
      filter(endYear == YearTlccInput)
    
    descriptionBlock(
      number = "", 
      numberColor = "aqua", 
      numberIcon =NULL,
      header = paste0(first(tlccDistrict$OverallFavDis), '%'), 
      text =  "Overall Favorability", 
      rightBorder = FALSE,
      marginBottom = FALSE)
  }
})
### TLCC Table ----
output$TLCCTableServer <- render_gt(   
  width = px(500),{
    
    SchoolNumber <- input$SchoolID
    YearTlccInput <- input$yearTlcc
    
    tlccLevels <- c("School Leadership", #
                    "Teacher Leadership", #"Staff Leadership"
                    "Managing Student Conduct", #
                    "Instructional Practices and Support", #
                    "Professional Development",#
                    "Time", 
                    "Facilities and Resources", #
                    "Community Support and Involvement", #
                    "New Teacher Questions",
                    "District Supports",
                    "Overall Reflection"#"General Reflection" 
                    )
    
    tlccLabels <- c("School Leadership", 
                    "Teacher (Staff) Leadership", 
                    "Managing Student Conduct", 
                    "Instr. Practices & Support", 
                    "Professional Development",
                    "Time", 
                    "Facilities & Resources", 
                    "Comm. Support & Involvement", 
                    "New Staff Questions",
                    "District Supports",
                    "Overall (General) Reflection"
                    )

    #### District Only ----
    if (SchoolNumber == '9998'){
      if(YearTlccInput == '2020'| YearTlccInput == '2022'){
        tlccDistrict <- TLCCQuartiles %>% 
          filter(cdeSchoolNumber == '1318' & endYear == '2022' | cdeSchoolNumber == '0664' & endYear == '2020') %>% 
          filter(endYear == YearTlccInput) %>% 
          mutate(District = as.numeric(District)) %>% 
          mutate(District = paste0(round(District), '%')) %>% 
          ungroup() %>% 
          select(ClassificationName, District, footnote) %>% 
          mutate(ClassificationName = factor(ClassificationName, 
                                             levels = tlccLevels, 
                                             labels = tlccLabels,
                                             ordered = TRUE)) %>% 
          arrange(ClassificationName)
        
        tlccDistrict %>% 
          gt(auto_align = F) %>% 
          tab_options(table.font.size = 12, 
                      table.align = 'left', 
                      footnotes.font.size = 13) %>% 
          cols_label(ClassificationName = "Category", District = "District") %>%
          cols_align(align = "left", columns = 'ClassificationName') %>% 
          cols_hide(footnote) %>% 
          tab_footnote(footnote = first(tlccDistrict$footnote), 
                       locations = cells_column_labels(
                         columns = ClassificationName
                       )) %>% 
          cols_width(c(ClassificationName) ~ '200px', 
                     c(District) ~'50px')
      } else {
        
        tlccLevels <- c("School Leadership", #
                        "Teacher Leadership", #"Staff Leadership"
                        "Managing Student Conduct", #
                        "Instructional Practices and Support", #
                        "Professional Development",#
                        "Time", 
                        "Facilities and Resources", #
                        "Community Support and Involvement", #
                        "New Teacher Questions",
                        "District Supports",
                        "Overall Reflection"#"General Reflection" 
        )
        
        tlccLabels <- c("School Leadership", 
                        "Teacher Leadership", 
                        "Managing Student Conduct", 
                        "Instr. Practices & Support", 
                        "Professional Development",
                        "Time", 
                        "Facilities & Resources", 
                        "Comm. Support & Involvement", 
                        "New Teacher Questions",
                        "District Supports",
                        "Overall Reflection"
        )
        
        # tlccLevels <- c("School Leadership", #
        #                 "Teacher Leadership", #"Staff Leadership"
        #                 "Managing Student Conduct", #
        #                 "Instructional Practices and Support", #
        #                 "Professional Development",#
        #                 "Time", 
        #                 "Facilities and Resources", #
        #                 "Community Support and Involvement", #
        #                 "Overall Reflection"#"General Reflection" 
        # )
        # 
        # tlccLabels <- c("School Leadership", 
        #                 "Teacher (Staff) Leadership", 
        #                 "Managing Student Conduct", 
        #                 "Instr. Practices & Support", 
        #                 "Professional Development",
        #                 "Time", 
        #                 "Facilities & Resources", 
        #                 "Comm. Support & Involvement", 
        #                 "Overall (General) Reflection"
        # )
        # 
        
        #Calculation Details found in No Upload Files
        newTeacher2021 <- data.frame(ClassificationName = 'New Teacher Questions', 
                                     District = '66%', 
                                     footnote = '')
        
      tlccDistrict <- TLCCQuartiles %>% 
        filter(cdeSchoolNumber == '0033' & endYear == '2021') %>% 
        filter(endYear == YearTlccInput) %>% 
        mutate(District = as.numeric(District)) %>% 
        mutate(District = paste0(round(District), '%')) %>% 
        ungroup() %>% 
        select(ClassificationName, District, footnote) %>% 
        bind_rows(newTeacher2021) %>% 
        mutate(ClassificationName = factor(ClassificationName, 
                                           levels = tlccLevels, 
                                           labels = tlccLabels,
                                           ordered = TRUE)) %>% 
        arrange(ClassificationName) 
      
      tlccDistrict %>% 
        gt(auto_align = F) %>% 
        tab_options(table.font.size = 12, 
                    table.align = 'left', 
                    footnotes.font.size = 13) %>% 
        cols_label(ClassificationName = "Category", District = "District") %>%
        cols_align(align = "left", columns = 'ClassificationName') %>% 
        cols_hide(footnote) %>% 
        tab_footnote(footnote = first(tlccDistrict$footnote), 
                     locations = cells_column_labels(
                       columns = ClassificationName
                     )) %>% 
        cols_width(c(ClassificationName) ~ '200px', 
                   c(District) ~'50px')
      }
    } else { 
      ### All others ----
      TLCCQuartilesb <- TLCCQuartiles %>% 
        filter(cdeSchoolNumber == SchoolNumber) %>% 
        filter(endYear == YearTlccInput) %>% 
        ungroup() %>%
        filter(ClassificationName != 'New Teacher Questions') %>% 
        mutate(ClassificationName = factor(ClassificationName, 
                                           levels = tlccLevels, 
                                           ordered = TRUE)) 
      
      validate(need(nrow(TLCCQuartilesb)>0, "Data redacted, fewer than 16 responses"))
      
      TLCCOverall <- TLCCQuartilesb %>% 
        ungroup() %>% 
        summarise(School = first(OverallFav), 
                  District = first(OverallFavDis), 
                  # constructn, 
                  completionRate, headcount) %>% 
        mutate(ClassificationName = "Overall Favorability") %>% 
        select(ClassificationName, School, District, 
               # constructn, 
               completionRate, headcount) %>% 
        mutate(School = as.numeric(School))
      
      TLCCOverallb <- TLCCOverall %>% 
        bind_rows(TLCCQuartilesb) %>% 
        select(ClassificationName, School, District, quartFlag, completionRate, headcount, footnote) %>%  #rate
        mutate(ClassificationName = factor(ClassificationName,
                                           levels = tlccLevels,
                                           labels = tlccLabels,
                                           ordered = TRUE)) %>%
        arrange(ClassificationName) %>% 
        mutate(District = as.numeric(District)) %>% 
        mutate(School = paste0(School, '%'), 
               District = paste0(District, '%')) %>% 
        filter(ClassificationName != "Overall Favorability") %>%
        mutate(completionRate = paste0(round(completionRate), "%"))
      
      TLCCOverallb %>% 
        gt() %>% 
        tab_options(table.font.size = 12, 
                    table.align = 'left', 
                    footnotes.font.size = 13) %>% 
        cols_align(align = "left", columns = 'ClassificationName') %>%  
        cols_label(ClassificationName = "Category") %>% 
        tab_style(style = cell_fill(color = '#eeca87'), 
                  locations = cells_body(columns=c(School), 
                                         rows = quartFlag == 1)) %>% 
        tab_style(style = cell_fill(color = '#c5c5c5'), 
                  locations = cells_body(columns=c(School), 
                                         rows = quartFlag == 3)) %>% 
        tab_style(style = cell_fill(color = '#4EB89B'), 
                  locations = cells_body(columns=c(School), 
                                         rows = quartFlag == 2)) %>% 
        cols_hide(columns = c(quartFlag,  completionRate, headcount, footnote)) %>% # %>% #rate
        tab_footnote(footnote = paste0(max(TLCCOverallb$completionRate), 
                                       ' of ',  
                                       max(TLCCOverallb$headcount), 
                                       ' educators participated.'),
                     locations = cells_column_labels(
                       columns = c(School))) %>% 
        tab_footnote(footnote = paste0(first(TLCCOverallb$footnote)),
                     locations = cells_column_labels(
                       columns = c(School))) %>% 
        cols_width(c(ClassificationName) ~ '200px', 
                   c(School) ~ '50px',
                   c(District) ~'50px')
    }
  })

### TLCC Trend Plot ----
output$tlccTrendPlot <-  renderPlot({
  
  SchoolNumber <- input$SchoolID
  
  yearFiller <- data.frame('year' = c('2020', '2021', '2022'))
  
  tlccTrend <- tlccTrend %>% 
    filter(CDESchoolNumber == SchoolNumber) %>% 
    full_join(yearFiller)
  
  yearFiller <- data.frame('year' = c('2020', '2021', '2022'))
  if (SchoolNumber %in% tlccTrend$CDESchoolNumber){
    #### Schools with data  ----
    ggplot(data = tlccTrend,
           mapping = aes(x = year, y = percentAgreementOverallFav))+
      geom_bar(stat = "identity", 
               position = 'dodge', 
               color = 'darkgrey', 
               fill = '#315683')+
      geom_text(data = tlccTrend, 
                aes(x = year, 
                    y= percentAgreementOverallFav, 
                    label = paste0(round(percentAgreementOverallFav, 2)*100, '%')), 
                position = position_dodge(width = 1), 
                vjust = 1.2, 
                size = 8, 
                color = 'white')+
      ylim(c(0, 1))+
      theme_minimal()+
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 15),
            legend.position = c(0.1, 0.89),
            legend.title = element_blank(),
            legend.key.size = unit(3, "mm"),
            legend.spacing.x = unit(1.0, 'mm'),
            legend.background = element_rect(fill = "white", color = "darkgrey"),
            panel.background = element_rect(fill = '#EBF1F6', 
                                            colour = '#EBF1F6'),
            plot.background = element_rect(fill = '#EBF1F6', 
                                           colour = '#EBF1F6'),
            plot.caption = element_text(size = 12),
            panel.spacing.y=unit(.2, "lines"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid = element_blank())
  } else if (SchoolNumber == '9998') {
    #### District Only ----
    tlccTrendDistrict <- data.frame(year  = c('2020', '2021', '2022'), 
                                    percentAgreementOverallFav = c(.77,  .77, .78))
    
    ggplot(data = tlccTrendDistrict,
           mapping = aes(x = year, y = percentAgreementOverallFav))+
      geom_bar(stat = "identity", 
               position = 'dodge', 
               color = 'darkgrey', 
               fill = '#315683') +
      geom_text(data = tlccTrendDistrict, 
                aes(x = year, 
                    y= percentAgreementOverallFav, 
                    label = paste0(round(percentAgreementOverallFav, 2)*100, '%')), 
                position = position_dodge(width = 1), 
                vjust = 1.2, 
                size = 8, 
                color = 'white') +
      ylim(c(0, 1))+
      theme_minimal()+
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 15),
            legend.position = c(0.1, 0.89),
            legend.title = element_blank(),
            legend.key.size = unit(3, "mm"),
            legend.spacing.x = unit(1.0, 'mm'),
            legend.background = element_rect(fill = "white", color = "darkgrey"),
            panel.background = element_rect(fill = '#EBF1F6', 
                                            colour = '#EBF1F6'),
            plot.background = element_rect(fill = '#EBF1F6', 
                                           colour = '#EBF1F6'),
            plot.caption = element_text(size = 12),
            panel.spacing.y=unit(.2, "lines"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid = element_blank())
### Schools with NO data ----
  } else {
    tlccTrend <- tlccTrend %>% 
      filter(CDESchoolNumber == '0030')
    
    ggplot(data = tlccTrend,
           mapping = aes(x = year, y = percentAgreementOverallFav))+
      annotate('text',  y = .5, x = '2020.5', label  = "No trend data \nfor this site", size = 8, color = '#515151')+
      ylim(c(0, 1))+
      theme_minimal()+
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            legend.position = c(0.1, 0.89),
            legend.title = element_blank(),
            legend.key.size = unit(3, "mm"),
            legend.spacing.x = unit(1.0, 'mm'),
            legend.background = element_rect(fill = "white", color = "darkgrey"),
            panel.background = element_rect(fill = '#EBF1F6', 
                                            colour = '#EBF1F6'),
            plot.background = element_rect(fill = '#EBF1F6', 
                                           colour = '#EBF1F6'),
            panel.spacing.y=unit(.2, "lines"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid = element_blank())
  }
})


# output$cultureFrame <- renderUI({
#   
#   tags$iframe(class = "iframe-placeholder", 
#               src = 'https://jeffcopublicschools.shinyapps.io/SchoolCulture/', 
#               height = 800, 
#               width = 900
#               )
#   
# 
# })