# (3) Academic Growth & Achievement State Tab ----
# CMAS ----

## CMAS GROWTH SEE ....server/CMASGrowth.R

### Statewide Data Reactive -----

stateTestData <- reactive({
  
 schoolNumber <-  input$SchoolID
 
  m1 <- statewideAllLevelsComplete %>% 
    filter(cdeSchoolNumber == schoolNumber)
  
  return(list('statewideData' = m1))
})

### CMAS 2021 UserBox ELA Achievement ---- 
output$cmasElaBoxAch <- renderUI({
  schoolNumber <- input$SchoolID
  
  cmas <- stateTestData()$statewideData %>% #stateTestData()$
    filter(contentName == 'LANGUAGE ARTS', 
           # cdeSchoolNumber == schoolNumber, 
           profFlag == 1, 
           category == 'all',
           profDescription == 'Met') %>% 
    group_by(grade, participationRate, participationRateChar, totNumOverall, participationThreshold, 
             overallMeanSsco, pctBinPerfChar, nCountSgpCohort, medianSgpCohort, cdeSchoolNumber) %>% 
    summarise() %>% 
    ungroup()
    
    
  #### ELA Box Content ----
  
  source('data/cmas/cmasFunctions/cmas2022TableFunction.R', local = TRUE)
  validate(need(schoolNumber %in%  unique(cmas$cdeSchoolNumber), #statewideAllLevelsComplete
                message = ' '))
  
  cmas2022Table()

})

### CMAS 2021 UserBox Math Achievement ---- 
output$cmasMathBoxAch <- renderUI({
  
  schoolNumber <- input$SchoolID
  
  cmas <- stateTestData()$statewideData %>% 
    filter(contentName == 'MATH', 
           # cdeSchoolNumber == schoolNumber, 
           profFlag == 1, 
           category == 'all',
           profDescription == 'Met') %>% 
    group_by(grade, participationRate, participationRateChar, totNumOverall, participationThreshold, 
             overallMeanSsco, pctBinPerfChar, nCountSgpCohort, medianSgpCohort, cdeSchoolNumber) %>% 
    summarise() %>% 
    ungroup()

  
  #### Math Box Content ----
  source('data/cmas/cmasFunctions/cmas2022TableFunction.R', local = TRUE)
  validate(need(schoolNumber %in%  unique(cmas$cdeSchoolNumber), #statewideAllLevelsComplete
                message = ' '))
  
  cmas2022Table(.id = 'cmasMath', 
                .subject = 'Math', 
                .image = 'keys.png', 
                .navPillsId = 'cmasMathPills', 
                .navPillsSubject = 'CMAS Math 2022')
})

#### ELA Diverg Plot ----
output$cmasDivergEla <- renderggiraph({
  schoolNumber <- input$SchoolID
  
  source('data/cmas/cmasFunctions/cmasDivergingFunction.R')
  
  # validate(need(schoolNumber %in% cmasAllLevelsComplete$cdeSchoolCode), 'Data not available')
  validate(need(schoolNumber %in%  unique(stateTestData()$statewideData$cdeSchoolNumber), #statewideAllLevelsComplete
                message = 'Data not reported due to small number of students testing.'))
  
  
  cmasDiverging(.df = stateTestData()$statewideData, 
                # .school = schoolNumber, 
                .content = 'LANGUAGE ARTS')
  
})

#### math Diverg Plot ----
output$cmasDivergMath <- renderggiraph({
  schoolNumber <- input$SchoolID
  
  validate(need(schoolNumber %in%  unique(stateTestData()$statewideData$cdeSchoolNumber),
                message = 'Data not reported due to small number of students testing.'))
  
  cmasDiverging(.df = stateTestData()$statewideData, 
                # .school = schoolNumber, 
                .content = 'MATH')
  
})

# PSAT and SAT 2022 ----
## PSAT & SAT 2022 UserBox ELA Achievement ---- 
output$psatEbrw <- renderUI({
  
  
  schoolNumber <- input$SchoolID
  
  psat <- stateTestData()$statewideData %>% 
    filter(contentName == 'LANGUAGE ARTS', 
           grade  %in% c('Gr9', 'Gr10'),
           category == 'all',
           profDescription == 'Meet or Exceed Benchmark') %>% 
    select(grade, participationRate,  participationRateChar,
           totNumOverall, participationThreshold, 
           overallMeanSsco, pctBinPerfChar, medianSgpCohort, nCountSgpCohort, cdeSchoolNumber)   %>% 
    mutate(grade = factor(grade, 
                          levels = c('Gr9', 'Gr10', 'Gr11')#,
                          # labels = c('Grade 9', 'Grade 10', 'Grade 11')
    ))
  
  validate(need(nrow(psat) > 0, message = 'Data not reported due to small number of students testing.'))
  ### Math Box Content ----
  source('data/cmas/cmasFunctions/cmas2022TableFunction.R', local = TRUE)
  
  cmas2022Table(.df = psat,
                .id = 'satEla', 
                .subject = 'Reading & Writing', 
                .subtitle = 'Pre SAT',
                .image = 'bookBlank.png', 
                .navPillsId = 'satElaPills', 
                .navPillsSubject = 'Evidence-Based Reading and Writing 2022')

})
## SAT Diverging EBRW ----
output$satDivergEbrw <- renderggiraph({
  schoolNumber <- input$SchoolID
  
  source('data/sat/satFunctions/satDivergingFunction.R')
  satValidate <- stateTestData()$statewideData  %>% 
    filter(
      # cdeSchoolNumber == schoolNumber, 
           grade  %in% c('Gr9', 'Gr10'))
  
  validate(need(schoolNumber %in% unique(satValidate$cdeSchoolNumber), #jefferson Acad
                message = 'Data not reported due to small number of students testing.'))
  
  validate(need(nrow(satValidate) > 4, #Brady, Longview
                message = 'Data not displayed due small number of students in performance levels.'))
  
  satDiverging(.df = stateTestData()$statewideData, 
               # .school = schoolNumber, 
               .content = 'LANGUAGE ARTS')
  
})

## SAT Diverging Math ----
output$satDivergMath <- renderggiraph({
  schoolNumber <- input$SchoolID
  
  source('data/sat/satFunctions/satDivergingFunction.R')
  satAllLevelsComplete <- stateTestData()$statewideData %>% 
    filter(!is.na(totNumOverall))
  
  satValidate <- stateTestData()$statewideData %>% 
    filter(
      # cdeSchoolNumber == schoolNumber, 
           grade  %in% c('Gr9', 'Gr10'), 
           contentName == 'MATH')
  
  validate(need(schoolNumber %in% unique(satValidate$cdeSchoolNumber), #jefferson Acad
                message = 'Data not reported due to small number of students testing.'))
  
  
  validate(need(nrow(satValidate) > 3,  #brady
                message = 'Data not displayed due small number of students in performance levels'))
  
  satDiverging(.df = stateTestData()$statewideData, 
               # .school = schoolNumber, 
               .content = 'MATH')
})

## PSAT & SAT 2022 Userbox Math Achievement ----
output$psatMath<- renderUI({
  
  schoolNumber <- input$SchoolID
  
  psat <- stateTestData()$statewideData %>% 
    filter(contentName == 'MATH', 
           grade  %in% c('Gr9', 'Gr10'),
           category == 'all',
           # cdeSchoolNumber == schoolNumber, 
           profDescription == 'Meet or Exceed Benchmark') %>% 
    select(grade, participationRate,  participationRateChar,
           totNumOverall, participationThreshold, 
           overallMeanSsco, pctBinPerfChar, medianSgpCohort, nCountSgpCohort, cdeSchoolNumber)   %>% 
    mutate(grade = factor(grade, 
                                                                     levels = c('Gr9', 'Gr10', 'Gr11')#,
                                                                     # labels = c('Grade 9', 'Grade 10', 'Grade 11')
                          ))
  
  validate(need(nrow(psat) > 0,
                message = 'Data not reported due to small number of students testing.'))
  ### Math Box Content ----
  source('data/cmas/cmasFunctions/cmas2022TableFunction.R', local = TRUE)
  
  cmas2022Table(.df = psat,
                .id = 'satMath', 
                .subject = 'Mathematics', 
                .subtitle = 'Pre SAT',
                .image = 'keys.png', 
                .navPillsId = 'satMathPills', 
                .navPillsSubject = 'Mathematics 2022')
})

#### * Within group Heat Map -----
output$WithInGroupPlotHeat <- renderPlot({
  
  SchoolNumber <- input$SchoolID
  if (SchoolNumber %in% withinGroupDataForPlots$schoolNumber){
    withinGroupPlotHeat(schoolCode = SchoolNumber)
  }else{
    noWithinGroupPlot()
  }
})
### * Within group Heat Map -----
output$WithInGroupPlotHeatMath <- renderPlot({
  
  SchoolNumber <- input$SchoolID
  
  if (SchoolNumber %in% withinGroupDataForPlots$schoolNumber){
    withinGroupPlotHeat(schoolCode = SchoolNumber, subject = "Math")
  } else {
  }
})

### Student Demos Groups CMAS ----

output$cmasElaGroup <- renderggiraph({
  schoolNumber <- input$SchoolID
  
  validate(need(schoolNumber %in%  unique(stateTestData()$statewideData$cdeSchoolNumber), #
                message = 'Data not reported due to small number of students testing.'))

  cmasElaGroups <- stateTestData()$statewideData %>% 
    filter(gradeLevel %in% c('Grade 3', 'Grade 4', 
                             'Grade 5', 'Grade 6', 
                             'Grade 7', 'Grade 8'))
  

  source('data/cmas/cmasFunctions/statewideAssessmentsDemosFunction.R')
  
  statewideAssessmentsDemos(.df = cmasElaGroups, 
                                      .contentName = 'LANGUAGE ARTS')
})

output$cmasMathGroup <- renderggiraph({
  
  schoolNumber <- input$SchoolID
  
  validate(need(schoolNumber %in%  unique(stateTestData()$statewideData$cdeSchoolNumber), 
                message = 'Data not reported due to small number of students testing.'))

  cmasMathGroups <- stateTestData()$statewideData %>% 
    filter(gradeLevel %in% c('Grade 3', 'Grade 4', 
                             'Grade 5', 'Grade 6', 
                             'Grade 7', 'Grade 8'))
  
  source('data/cmas/cmasFunctions/statewideAssessmentsDemosFunction.R')
  statewideAssessmentsDemos(.df = cmasMathGroups, 
                            .contentName = 'MATH')
})

### Student Demos Groups PSAT ----
output$psatElaGroup <- renderggiraph({
  schoolNumber <- input$SchoolID
  
  psatEbrwGroups <- stateTestData()$statewideData %>% 
    filter(gradeLevel %in% c('Grade 9', 'Grade 10'))
  
  validate(need(schoolNumber %in%  unique(stateTestData()$statewideData$cdeSchoolNumber), 
                message = 'Data not reported due to small number of students testing.'))
  
  source('data/cmas/cmasFunctions/statewideAssessmentsDemosFunction.R')
  statewideAssessmentsDemos(.df = psatEbrwGroups, 
                            .contentName = 'LANGUAGE ARTS')
})

output$psatMathGroup <- renderggiraph({
  
  schoolNumber <- input$SchoolID
  
  psatMathGroups <- stateTestData()$statewideData %>% 
    filter(gradeLevel %in% c('Grade 9', 'Grade 10'))
  
  validate(need(schoolNumber %in%  unique(stateTestData()$statewideData$cdeSchoolNumber), 
                message = 'Data not reported due to small number of students testing.'))
  
  source('data/cmas/cmasFunctions/statewideAssessmentsDemosFunction.R')
  statewideAssessmentsDemos(.df = psatMathGroups, 
                            .contentName = 'MATH')
})

## SPF Helper Button ----
output$spfHelper <- renderUI({
  
  SchoolNumber <- input$SchoolID
  
  schoolCode <- masterSchoolProfileData %>%
    ungroup() %>% 
    filter(cdeSchoolCode == SchoolNumber)%>% 
    distinct(cdeSchoolCode) %>% 
    pull()
  
  link <- paste0('https://www.cde.state.co.us/schoolview/frameworks/official/1420/', schoolCode) 
  
  if (SchoolNumber != '9998'){
    a("School Performance Framework", target = "_blank", href = link)
  } else {
    a("District Performance Framework", target = "_blank", href = 'https://www.cde.state.co.us/schoolview/frameworks/official/1420')
  }
})
## UIP Helper ----
output$uipHelper <- renderUI({
  
  SchoolNumber <- input$SchoolID
  
  schoolCode <- masterSchoolProfileData %>%
    ungroup() %>% 
    filter(cdeSchoolCode == SchoolNumber)%>% 
    distinct(cdeSchoolCode) %>% 
    pull()
  
  link <- paste0('http://co-uip-cde.force.com/?dcode=1420&scode=', schoolCode) 
  
  if (SchoolNumber != '9998'){
    a("Unified Improvement Plan", target = "_blank", href = link)
  } else {
    a("Unified Improvement Plan", target = "_blank", href = 'http://co-uip-cde.force.com/?dcode=1420')
  }
})
# PWR Summary ----
## SAT -----
output$SATTable <- render_gt(
  # width = '6100px', 
  {
    schoolNumber <- input$SchoolID 
    
    satBenchmark <- stateTestData()$statewideData %>% 
      filter(grade  == 'Gr11',
             # cdeSchoolNumber == schoolNumber, 
             category == 'all',
             profDescription == 'College Ready') %>% 
      mutate(pctBinPerfChar = paste0(round(pctBinPerf*100, 1), '%')) %>% 
      select(cdeSchoolNumber, contentName, overallMeanSsco, pctBinPerfChar) %>% 
      mutate(contentName = recode(contentName, 'MATH' ="Mathematics", 'LANGUAGE ARTS' = "Evidence Based Reading & Writing" ))
    
    validate(need(schoolNumber %in% unique(satBenchmark$cdeSchoolNumber),
                  message = 'SAT data not reported due to small number of students testing.'))
    # 
    # if (SchoolNumber %in% unique(satBenchmark$cdeSchoolNumber)) {
      satBenchmark %>% 
        gt() %>% 
        tab_options(table.font.size = 12) %>% 
        tab_header(
          title = md("2021-2022 College Board SAT Results")) %>% 
        cols_label(contentName = html('<b>','Content Area</b>'), 
                   overallMeanSsco = html('<b>Mean Scale Score</b>'),
                   pctBinPerfChar= html('<b>','Percentage On Track for College Readiness</b>')) %>% 
        cols_width(
          c(contentName) ~ '200px',
                   c(overallMeanSsco) ~'150px',
                   c(pctBinPerfChar) ~'150px'
          ) %>%
        cols_align(
          align = "left",
          columns = c(contentName)) %>% 
        cols_align(
          align = "center",
          columns = c(overallMeanSsco, pctBinPerfChar)) %>% 
        cols_hide(cdeSchoolNumber) %>% 
        tab_options(table.font.size = 14)
    # } else {
    # }
  })
## Four Year Grad Rate ----
output$FourYearGradRate <- renderValueBox({
  SchoolNumber <- input$SchoolID
  
  if (SchoolNumber %in% PWRData$CDESchoolCode) {
    
    FourYearGrad <- pwr2021 %>%
      filter(cdeSchoolCode == SchoolNumber) %>% 
      filter(str_detect(pwrType, "4$")) %>%
      mutate(rate = paste0(round(rate * 100, 1), "%"))

    valueBox(FourYearGrad$rate, #FourYearGrad$PWR_GRAD_RATE_4YR, 
             subtitle = "4-year Graduation Rate", 
             icon = icon('graduation-cap', verify_fa = FALSE))
  } else {
  }
})
##  Seven Year Grad Rate ----
output$SevenYearGradRate <- renderValueBox({
  SchoolNumber <- input$SchoolID
  
  if (SchoolNumber %in% PWRData$CDESchoolCode) {
    SevenYearGrad <- pwr2021 %>%
      filter(cdeSchoolCode == SchoolNumber) %>% 
      filter(str_detect(pwrType, "7$")) %>%
      mutate(rate = paste0(round(rate * 100, 1), "%"))

    valueBox(SevenYearGrad$rate, #SevenYearGrad$PWR_GRAD_RATE_7YR, 
             subtitle = "7-year Graduation Rate", 
             icon = icon('graduation-cap', verify_fa = FALSE))
  } else {
  }
})
## Dropout Rate ----
output$DropOut<- renderValueBox({
  SchoolNumber <- input$SchoolID

  if (SchoolNumber %in% PWRData$CDESchoolCode) {
    
    DropOutRate <- pwr2021 %>%
      filter(cdeSchoolCode == SchoolNumber,
             pwrType == "drop") %>%
      mutate(rate = paste0(round(rate * 100, 1), "%"))

    valueBox(DropOutRate$rate, #DropOutRate$PWR_RATE, 
             subtitle = HTML("Drop Out Rate<br>"))
  } else {
  }
})


## ACCESS for ELLs----
### Reactive ----
  accessData <- reactive({
    
    # schoolNumber <- input$SchoolID
    
      m1 <- accessComplete %>% 
        filter(schNumber == input$SchoolID)
      
      return(list('access'= m1))
  })
### Growth -----

output$accessGro <- render_gt({
  # schoolNumber <- input$SchoolID
  
  accessGrowthValidate <- accessData()$access %>% 
    # filter(schNumber == schoolNumber) %>% 
    select(schNumber, medianSgp, pctOnTrack, growthN, emhLevel) %>% 
      mutate(pctOnTrack = paste0(round(pctOnTrack*100, 1), '%')) %>%
      filter(!is.na(medianSgp)) 
               
  validate(need(accessGrowthValidate$growthN > 19, 'Student growth for this school are suppressed due to fewer than 20 students with growth results'))
    
  accessGrowth <- accessGrowthValidate %>%
    ungroup() %>% 
    select(emhLevel, growthN, medianSgp, pctOnTrack) %>% 
    group_by( emhLevel) %>% 
      summarise(medianSgp = first(medianSgp),
                pctOnTrack = first(pctOnTrack), 
                emhLevel = first(emhLevel)) %>%
      mutate(medianSgp = scales::ordinal(medianSgp)) %>% 
    mutate(emhLevel = factor(emhLevel, 
                             levels = c('Elementary School', 
                                        'Middle School', 
                                        'High School'), 
                             ordered = T)) %>% 
    arrange(emhLevel)
  
  gt(accessGrowth) %>% 
    cols_label(medianSgp = md('Median Student<br>Growth Percentile'), 
               pctOnTrack = md('Percentage of Students<br>On Track for EL Proficiency'), 
               emhLevel = '') %>% 
    cols_align(align = 'center') %>% 
    cols_align('left', 
               emhLevel) %>% 
    tab_options(table.align = 'left',
                column_labels.font.weight = 'bold',
                table.border.right.style = 'solid',
                table.border.right.width = '2px',
                table.border.right.color = 'grey',
                table.border.left.style = 'solid',
                table.border.left.width = '2px',
                table.border.left.color = 'grey',
      table.width = '70%') %>% 
    opt_row_striping(row_striping = TRUE)
})

### Achievement Table-----s
output$accessAch <- render_gt({
  schoolNumber <- input$SchoolID
  
  accessLevels <-  accessData()$access %>% 
    # filter(schNumber == schoolNumber) %>% 
     select(schNumber, totalN, levelN, pctLevel, pctLevelLabel, proficiencyLevel, gradeLevel, totalNLabel) %>% 
    ungroup() %>% 
    filter(!is.na(gradeLevel))
  
  validate(need(nrow(accessLevels)>0, 'No results for this school'))
  
  validate(need(accessLevels$totalN > 15, 'Student levels for this school are suppressed due to fewer than 16 students tested'))
  
  plFiller <- expand.grid(
    gradeLevel = unique(accessLevels$gradeLevel),
    proficiencyLevel = c( 'Level 1 Entering',
      'Level 2 Beginning', 'Level 3 Developing',
      'Level 4 Expanding', 'Level 5 Bridging',
      'Level 6 Reaching'
    ))

  accessWide <- accessLevels %>% 
    full_join(plFiller) %>% 
    pivot_wider(id_cols = c(schNumber, gradeLevel, totalN, levelN, totalNLabel), 
                names_from = proficiencyLevel, 
                values_from = pctLevelLabel) %>% 
    group_by(gradeLevel, schNumber) %>% 
    filter(!is.na(schNumber)) %>%
    select(schNumber,  `Level 1 Entering`, `Level 2 Beginning`, `Level 3 Developing`,
           `Level 4 Expanding`, `Level 5 Bridging`,
           `Level 6 Reaching`, gradeLevel, totalN, totalNLabel) %>% 
    group_by(gradeLevel, schNumber) %>% 
    summarise(across(.fns = ~na.omit(.)[1])) %>% #Squish rows with same grade , but NAs in levels
    ungroup()  
  
  gt(accessWide) %>% 
    cols_label(
      gradeLevel = md('Grade<br>Level<br>Cluster'), 
      totalNLabel = md('Total<br>Students with<br>Overall Score')
    ) %>%
    cols_align(align = 'left', 
               columns = gradeLevel) %>% 
    cols_align(align = 'center', columns = c(`Level 1 Entering`, `Level 2 Beginning`, `Level 3 Developing`,
                                             `Level 4 Expanding`, `Level 5 Bridging`,
                                             `Level 6 Reaching`, totalNLabel)) %>%
    sub_missing(missing_text = ' - ') %>% #Substitute missing values in the table body
    tab_spanner(label = 'Percentage of Students in Each Proficiency Level', 
                columns = c(`Level 1 Entering`, `Level 2 Beginning`, `Level 3 Developing`,
                            `Level 4 Expanding`, `Level 5 Bridging`,
                            `Level 6 Reaching`)) %>% 
    tab_options(
      table.align = 'left',
      table.width = '100%', 
      table.border.right.style = 'solid',
      table.border.right.width = '2px',
      table.border.right.color = 'grey',
      table.border.left.style = 'solid',
      table.border.left.width = '2px',
      table.border.left.color = 'grey',
      column_labels.font.weight = 'bold') %>% 
    cols_hide(c(schNumber, totalN)) %>% 
    cols_width(
      gradeLevel ~ px(100),
      starts_with('Level') ~ px(85), 
      totalNLabel ~ px(100)) %>% 
    opt_row_striping(row_striping = TRUE)
  
})
  
###Achievement Plot-----
output$accessPlotSelect <- ggiraph::renderggiraph({
  schoolNumber <- input$SchoolID
  gradeSelect <- input$accessGrade
  
  accessLevelsPlot <-  accessData()$access %>% 
    # filter(schNumber == schoolNumber) %>% 
    filter(gradeLevel == gradeSelect) %>% 
    mutate(proficiencyLevel = str_remove(proficiencyLevel, 'Level '))
  
  validate(need(nrow(accessLevelsPlot)>0, 'No results for this school'))
  
  
  validate(need(accessLevelsPlot$totalN > 15, ' '))
  
  plFiller <- expand.grid(
    gradeLevel = unique(accessLevelsPlot$gradeLevel),
    proficiencyLevel = c( '1 Entering',
                          '2 Beginning', '3 Developing',
                          '4 Expanding', '5 Bridging',
                          '6 Reaching'
    ))
  
  accessPlots <- accessLevelsPlot %>% 
    full_join(plFiller)
  
p <- ggplot(data = accessPlots, 
         mapping = aes(x = fct_rev(proficiencyLevel), 
                       y = pctLevel, 
                       fill = proficiencyLevel))+
    geom_col_interactive(width = .9,
                         aes(
                             tooltip = paste0('<b>Proficiency Level: </b>', proficiencyLevel,
                                              '<br>', '<b>Total Students Tested: </b>', totalN,
                                              '<br>', '<b>Total Students in Level: </b>', levelN,
                                              '<br>', '<b>Percentage of Students in Level: </b>',
                                              round(pctLevel*100), '%'
                                              )))+
    # geom_col(width = .9) + 
    geom_text(aes(label = paste0(round(pctLevel*100), '%')), 
              hjust = -0.1,
              vjust = 0.5,
              size = 25) +
    scale_fill_manual(values = c('#FFC000', '#E46C0A',
                                 '#C3D69B', '#00B050',
                                 '#007979','#336699')) +
    ylim(0, 1) +
    facet_wrap(~gradeLevel, 
               ncol = 1, 
               scales = 'free_y') +
    coord_flip(expand = T) +
    theme_minimal() +
    theme(axis.title = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(size = 70, 
                                                 hjust = 0, 
                                                 # data_id = 'yAxis', 
                                                 # tooltip = '<b>Proficiency Levels</b><br>1 Entering<br>2 Beginning<br>3 Developing<br>4 Expanding<br>5 Bridging<br>6 Reaching'
                                     ), 
          legend.position = 'none',
          panel.grid = element_blank(),
          plot.background = element_rect(color = "grey", linewidth = 3),
          strip.text = element_text(hjust = 0.5, 
                                                # data_id = 'strip.text', 
                                                # tooltip = 'Grade Level',
                                    face = 'bold',
                                    size = 90
          ))
ggiraph::ggiraph(code = print(p), 
        tooltip_opacity = 0.9, 
        tooltip_offx = 50, 
        tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
        hover_css = "cursor:pointer;stroke:#EA9563;",
        width = 1,
        width_svg = 30,
        height_svg = 12
)
})

###Access UI -----

observeEvent(input$SchoolID, {

  schoolNumber <- input$SchoolID
  
  accessFiltered <- accessComplete %>%
    filter(schNumber == schoolNumber) %>%
    filter(totalN >15) %>%
    ungroup() %>%
    distinct(gradeLevel)

updateRadioGroupButtons(session,
                        inputId = 'accessGrade',
                  label = 'TestAccess',
                  choices = unique(as.character(accessFiltered$gradeLevel)),
                  selected = unique(first(as.character(accessFiltered$gradeLevel))),
                  size = 'xs')


}
)