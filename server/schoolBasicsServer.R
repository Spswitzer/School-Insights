# (1) School Basics Server ----
##  Total Enrollment, Grades, and Attendance ----
### Enrollment ----
#### Enrollment Header ----
output$EnrollmentHeader <- renderText({
  paste0("Student Enrollment <i>(",masterTimePeriod,")</i>")
})

enrollment <- reactive(

    masterSchoolProfileData %>%
      filter(cdeSchoolCode == input$SchoolID) %>% 
      mutate(totalPKthru12Count = formatC(totalPKthru12Count, format = 'd', big.mark = ','))
  ) %>% 
  bindCache(input$SchoolID)

#### Student Enrollment - Total Enrollment ----
output$schoolEnrollment <- renderUI({

  if (input$SchoolID == '9234' | input$SchoolID == '9245'| input$SchoolID == '1001') { #Warren Tech sites use secondary enrollment
    secondaryEnrollment <- secondaryEnrollment %>% 
      filter(cdeSchoolNumber == input$SchoolID, 
             enrollmentType == 'S') %>% 
      mutate(totalStudents = formatC(totalStudents, format = 'd', big.mark = ','))
    
    valueBox(value = HTML(paste0(first(secondaryEnrollment$totalStudents))), 
             subtitle = span(HTML(paste0('Students with Primary or Secondary Enrollment'))),
             width = 12)
  } else if (input$SchoolID == '4408'){
    jva <- masterSchoolDataProfileJVA %>% 
      filter(cdeSchoolCode == input$SchoolID) %>% 
      mutate(totalStudents = formatC(totalPKthru12Count, format = 'd', big.mark = ','))
    
    valueBox(value = HTML(paste0(first(jva$totalStudents))), 
             subtitle = span(HTML(paste0('Student Enrollment <br>(Excludes Jeffco Remote Learning Program)'))),
             width = 12)
  } else if (is.na(enrollment()$homeBasedN)) { #sites with no or low homebased learners
    valueBox(value = HTML(paste0(enrollment()$totalPKthru12Count)), 
             subtitle = 'Student Enrollment', 
             width = 12)
  } else { #sites with homebased learners
    valueBox(value = HTML(paste0(enrollment()$totalPKthru12Count)),
             subtitle = span(HTML(paste0('Student Enrollment <br>(Home-based: ', enrollment()$homeBasedN, ')'))),
             width = 12)
  }
})
#### Student Enrollment - Enrollment Trend ----
output$enrollmentTrendBarPlot <- renderPlot({
  SchoolNumber <- input$SchoolID
  
  attendanceTrendOneSite <- attendanceTrend %>% 
    mutate(attendanceRateNumeric = round(attendanceRateNumeric)) %>% 
    filter(cdeSchoolCode == SchoolNumber) %>% 
    mutate(ylimEnrollmentMax = max(studentEnrollment) +1) %>%
    mutate(studentEnrollment = round(studentEnrollment)) %>% 
    filter(endYear %in% c(2022, 2021, 2020)) %>% 
    mutate(endYear = as.character(endYear)) %>% 
    mutate(endYear = recode(endYear,  
                            '2022' = '2021-2022',
                            '2021' = '2020-2021',
                            '2020' = '2019-2020'))
  
  if  (SchoolNumber == '9234' | SchoolNumber == '9245'| SchoolNumber == '1001') {
    ggplot(mapping = aes(x = 1, y = 1))+
      geom_text(mapping = aes(label = 'Due to high proportion \nof students in secondary\n enrollment status, \n no trend data available'), size = 6, color = 'grey')+
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(), 
            axis.ticks = element_blank(), 
            panel.grid = element_blank())
  } else {
    ggplot(data = attendanceTrendOneSite, 
           mapping = aes(x = factor(endYear), 
                         y = studentEnrollment)) +
      geom_col(color = 'white',
               fill = '#315683') +
      geom_label(aes(label = scales::number(studentEnrollment, accuracy = 1, big.mark = ',')), 
                 size = 6, 
                 fill = 'white',
                 color = '#315683', 
                 vjust = 1.1, 
                 label.padding = unit(0.3, "lines")) +
      ylim(0, first(attendanceTrendOneSite$ylimEnrollmentMax)) +
      facet_wrap(~endYear, scales = 'free', strip.position = "bottom") +
      theme_minimal() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            panel.grid = element_blank(), 
            panel.background = element_rect(fill = 'white', color = 'lightgrey'),
            strip.text = element_text(size = 16), 
            strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'))
  }
})
#### Student Enrollment - Grade Range of school ----
output$schoolGradeRange <- renderUI({
  
  SchoolNumber <- input$SchoolID
  
  grades <-enrollment()
  if (SchoolNumber == '9234'| SchoolNumber == '9245'| SchoolNumber == '1001') { #Warren Tech sites accept larger range of grade, but as exception
    valueBox(value = paste0(11, ' - ', 12), 
             subtitle = 'Grades Served', 
             width = 12)
  } else if (SchoolNumber == '4408') { #JVA in 2021-22
    valueBox(value = paste0(6, ' - ', 12), 
             subtitle = 'Grades Served', 
             width = 12)
  } else {
    valueBox(value = paste0(grades$startGrade, ' - ', grades$endGrade), 
             subtitle = 'Grades Served', 
             width = 12)
  }
})
#### Student Enrollment - Average Attendance Rate ----
output$schoolAttendance <- renderUI({
  
  SchoolNumber <- input$SchoolID
  
  attendance <- Attendance %>%
    filter(cdeSchoolCode == SchoolNumber) %>%
    select(attendanceRateNumeric)
  
  if (SchoolNumber == '1001'){ #Warren Tech South - 2021 Only!!
    valueBox(value = HTML('Not Applicable'), 
             subtitle = 'Average Attendance Rate',
             width = 12)
  } else {
    valueBox(value = HTML(paste0(attendance$attendanceRateNumeric, '%')), 
             subtitle = 'Average Attendance Rate',
             width = 12)
  }
})
#### Student Enrollment - Average Attendance Trend ----
output$attendanceTrendBarPlot <- renderPlot({
  SchoolNumber <- input$SchoolID
  
  attendanceTrendOneSite <- attendanceTrend %>% 
    mutate(attendanceRateNumeric = round(attendanceRateNumeric)) %>% 
    filter(cdeSchoolCode == SchoolNumber) %>% 
    filter(endYear %in% c(2022, 2021, 2020)) %>% 
    mutate(endYear = as.character(endYear)) %>% 
    mutate(endYear = recode(endYear,  
                            '2022' = '2021-2022',
                            '2021' = '2020-2021',
                            '2020' = '2019-2020'))
  
  if  (SchoolNumber == '9234' | SchoolNumber == '9245'| SchoolNumber == '1001') { #Warren Tech
    ggplot(mapping = aes(x = 1, y = 1))+
      geom_text(mapping = aes(label = 'Due to high proportion \nof students in secondary\n enrollment status, \n no trend data available'), size = 6, color = 'grey')+
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(), 
            axis.ticks = element_blank(), 
            panel.grid = element_blank())
  } else {
    ggplot(data = attendanceTrendOneSite, 
           mapping = aes(x = factor(endYear), 
                         y = attendanceRateNumeric)) +
      geom_col(color = 'white',
               fill = '#315683')+
      geom_label(aes(label = paste0(attendanceRateNumeric, '%')), 
                 size = 6, 
                 fill = 'white', 
                 color = '#315683', 
                 vjust = 1.1, 
                 label.padding = unit(0.3, "lines")) +
      facet_wrap(~endYear, scales = 'free', strip.position = "bottom") +
      ylim(0, 100) +
      theme_minimal() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            panel.grid = element_blank(), 
            panel.background = element_rect(fill = 'white', color = 'lightgrey'),
            strip.text = element_text(size = 16), 
            strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'))
  }
})
# Map/Geo Location ----
output$geoLocation <- renderLeaflet({
  
  SchoolNumber <- input$SchoolID
  
  icons <- awesomeIcons(
    icon = 'id-card',
    iconColor = '#ffffff',
    library = 'fa',
    squareMarker = TRUE,
    markerColor = 'darkblue'
  )
  
  mapEachEmhLevel <- function(levelCampCodChar) {
    
    JeffcoLatLngB <-  JeffcoLatLng %>% 
      mutate(SchoolID = str_pad(SchoolID, width = 4, side = 'left', pad = '0')) %>% 
      filter(SchoolID == SchoolNumber) %>% 
      mutate(popupLabel = paste0('<b>', 'Link to Website: ', '</b>', 
                                 '<a href =', 
                                 websiteSchool, '# target=, # `"`_blank`"``,>', 
                                 School, '</a>',
                                 '<br>', '<b>', 'Articulation Area: ', '</b>', 
                                 SchoolArea, 
                                 '<br>', streetAddress, 
                                 '<br>', City, ' ', PhysicalZipcode))
    
    map <- tm_shape(Boundaries)+
      tm_fill(col=levelCampCodChar,
              title = "",
              alpha = 0.2, 
              interactive = F,
              palette = '#673785')+
      tm_borders(col = "#971b72")+
      tm_layout("",
                legend.text.size = 1)
    
    Map <-  tmap_leaflet(map) %>% 
      setView(lng = Boundaries$longitude, 
              lat = Boundaries$latitude, 
              zoom = 13) %>% 
      addProviderTiles(providers$Esri.WorldStreetMap) %>% 
      addAwesomeMarkers(lng = JeffcoLatLngB$longitude,
                        lat = JeffcoLatLngB$latitude,
                        icon = icons,
                        popup = JeffcoLatLngB$popupLabel) %>%
      clearControls() 
    Map$x$options = append(Map$x$options, list("zoomControl" = TRUE, "attributionControl"=FALSE))
    Map
  }
  
  if (SchoolNumber %in% schoolsInElemShape) { ### plot Elementary schools with boundaries ----
    
    Boundaries <- JeffcoMapElem[JeffcoMapElem@data$ES_CampCod == SchoolNumber , ]
    
    mapEachEmhLevel(levelCampCodChar = 'ES_CampCod')
    
  } else if (SchoolNumber %in% schoolsInMiddleShape){ ### plot Middle schools with boundaries ----
    
    Boundaries <- JeffcoMapMiddle[JeffcoMapMiddle@data$MS_CampCod == SchoolNumber , ]
    
    mapEachEmhLevel(levelCampCodChar = 'MS_CampCod')
    
  } else if (SchoolNumber %in% schoolsInHighShape){ ### plot High schools with boundaries ----
    
    Boundaries <- JeffcoMapHigh[JeffcoMapHigh@data$HS_CampCod == SchoolNumber , ]
    
    mapEachEmhLevel(levelCampCodChar = 'HS_CampCod')
    
  } else { ### plot schools without boundaries ----
    JeffcoLatLngB <-  JeffcoLatLng %>% 
      mutate(SchoolID = str_pad(SchoolID, width = 4, side = 'left', pad = '0')) %>% 
      filter(SchoolID == SchoolNumber) %>% 
      mutate(popupLabel = paste0('<b>', 'Link to Website: ', '</b>', 
                                 '<a href =', 
                                 websiteSchool, 
                                 '# target=, # `"`_blank`"``,>', 
                                 School, '</a>',
                                 '<br>', '<b>', 'Articulation Area: ', '</b>', 
                                 SchoolArea, 
                                 '<br>', streetAddress, 
                                 '<br>', City, ' ', PhysicalZipcode))
    Map <- qtm(shp=schoolDistrict,
               fill = NULL)
    
    Map %>% leaflet(options = leafletOptions(zoomControl = TRUE, 
                                             attributionControl=FALSE)) %>% 
      addTiles() %>% 
      setView(lng = JeffcoLatLngB$longitude, 
              lat = JeffcoLatLngB$latitude, 
              zoom = 13) %>% 
      addProviderTiles(providers$Esri.WorldStreetMap) %>% 
      addAwesomeMarkers(lng = JeffcoLatLngB$longitude,
                        lat = JeffcoLatLngB$latitude,
                        icon = icons, 
                        popup = JeffcoLatLngB$popupLabel)
  }
})

# Websites & School Profile ----
output$webResources <- renderUI({                
  
  SchoolNumber <-  input$SchoolID
  
  websites <- JeffcoLatLng %>% 
    filter(SchoolID == SchoolNumber) 
  
  image <- paste0(SchoolNumber, "schoolWebsite.png")
  
  validate(need(SchoolNumber %in% websites$SchoolID, 'Select a school'))
  
  URL <-  tags$a(
    href = websites$websiteSchool, 
    target="_blank",
    tags$img(src = image, 
             title = "School weblink", 
             width = "175",
             height = "155") )
  
  imageEj <-  paste0('EJMay12.png')
  
  ejUrl <- tags$a(
    href = websites$choiceWebsite, 
    target="_blank",
    tags$img(src = imageEj,
             title = "School Profile", 
             width = "190",
             height = "155") )
  
  productList(
    productListItem(
      URL,
      title = 'School Website',
      image = 'jeffcoTree_sm_white.png'
    ),
    productListItem(
      ejUrl,
      title = 'School Profile',
      image = 'jeffcoTree_sm_white.png'))
})
# Ratios ----
output$ratios <- renderValueBox({
  
  SchoolNumber <- input$SchoolID
  
  ratios <- ratios %>% 
    filter(cdeSchoolCode == SchoolNumber) %>% 
    select(ratio)
  
  if (input$SchoolID == 9998){
    valueBox(value = paste0('1:', ratios), 
             subtitle = "Educator to Student Ratio (not class size)",  
             width = 12)
  } else if (ratios$ratio < 2){
    valueBox(value = " ", 
             subtitle = "Not applicable", 
             width = 12)
  } else {
    valueBox(value = paste0('1:', ratios), 
             subtitle = HTML("Educator to Student Ratio <BR>(not class size)"), 
             width = 12)
  }
})
# Student Group Plot ----    

### Student Group Plot - Program participation ----

output$simpleProgramParticipation <- renderggiraph({
  
  SchoolNumber <- input$SchoolID
  # Is the total population large enough to report?
  populationCheck <- masterSchoolProfileData %>% 
    ungroup() %>%
    filter(cdeSchoolCode == SchoolNumber) %>% 
    select(totalPKthru12Count)
  
  Demographics <- masterSchoolProfileData %>% 
    filter(cdeSchoolCode == SchoolNumber | cdeSchoolCode == '9998') %>%
    mutate(totalPKthru12Count = formatC(totalPKthru12Count, format = 'd', big.mark = ',')) %>% 
    select(cdeSchoolCode, 
           School = "veryAbbreviatedName", 
           percentFreeAndReduced, 
           elPct, 
           spedPct, 
           gtPct, homeBasedN) %>% 
    mutate(cdeSchoolCode = str_pad(cdeSchoolCode, width = 4, side = 'left', pad = '0'))
  
  DistrictDemos <- Demographics %>% 
    filter(cdeSchoolCode == "9998") %>% 
    mutate(site = 'District')
  
  Statedemos <- data.frame(cdeSchoolCode = '9999', #not provided in data, needs to be update manually, annually
                           School = "State", 
                           percentFreeAndReduced = 0.37, 
                           elPct = 0.12, 
                           spedPct = 0.12, 
                           gtPct = 0.07, 
                           site = "State")
  # Need this for conditional plotting
  DemographicsSchool <-Demographics %>% 
    filter(cdeSchoolCode == SchoolNumber) %>% 
    bind_rows(DistrictDemos) %>% 
    bind_rows(Statedemos) %>% 
    select(-cdeSchoolCode, -School)
  
  DemographicsSchoolPlot <- Demographics %>% 
    filter(cdeSchoolCode == SchoolNumber) %>% 
    mutate(site = 'School') %>% 
    bind_rows(DistrictDemos) %>% 
    bind_rows(Statedemos) %>% 
    select(-cdeSchoolCode) %>% 
    mutate('FRL' = percentFreeAndReduced, 
           'ELL' = elPct, 
           'GT' = gtPct, 
           'Sped' = spedPct) %>%
    ungroup() %>% 
    select(site, FRL, ELL, GT,  Sped) %>% 
    pivot_longer(cols = FRL:Sped, 
                 names_to = 'var', 
                 values_to = 'value') %>% 
    mutate(var = factor(var, 
                        levels = c('GT','Sped', 'ELL', 'FRL'), 
                        labels = c('Gifted & Talented','Special Education', 
                                   'English Language Learners', 
                                   'Free or Reduced Lunch Eligible'),
                        ordered = TRUE)) %>% 
    arrange() %>% 
    mutate(value = as.numeric(value), 
           value = round(value * 100)) %>% 
    mutate(valueLabel = case_when(
      value <.01 ~ paste0(round(value, 1), '%'), 
      TRUE ~ paste0(round(value), '%')))   %>% 
    mutate(site = factor(site, 
                         levels = c('School', 'District', 'State'), 
                         ordered = TRUE)) %>% 
    mutate(valueLabel = case_when(
      valueLabel == 'NA%' ~ 'fewer than 16 students in group', 
      TRUE ~ valueLabel
    )) %>% 
    mutate(value = case_when(
      is.na(value) ~ 0, 
      TRUE ~ value
    ))
  
  
  # Set a theme for each plot in conditional to follow
  enrollmentTheme <- theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.text.x = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.text.y = element_text(size = 18, hjust = 0#, data_id = 'axis.text.y', _interactive
                                                                  # tooltip = 'Reporting levels include<br>State, District, and School'
                                                                  ),
                           panel.background = element_rect(fill = '#FFFFFF',
                                                           colour = '#FFFFFF'),
                           plot.background = element_rect(fill = '#F6F6F6',
                                                          colour = '#F6F6F6'),
                           plot.caption = element_text(size  = 18),
                           panel.grid.minor.x = element_blank(),
                           panel.grid.major.x = element_line(),
                           panel.grid.major.y = element_blank(),
                           legend.position = 'none',
                           strip.text = element_text(size = 18, #_interactive
                                                                 face = 'bold',
                                                                 hjust = 0#,
                                                                 # data_id = 'strip.text', 
                                                                 # tooltip = 'Student programs include<br>Free or Reduced Lunch Eligible,
                                                                 #   <br>English Language Learners,<br>Special Education,
                                                                 #   <br>and Gifted Education'
                                                     ), 
                           strip.background = element_rect(color = 'white', fill = 'white'))
  
  if (SchoolNumber == 9998){ #### Plot for district display only ----
    p <- ggplot()+
      geom_bar_interactive(data = filter(DemographicsSchoolPlot, site != 'School'), #_interactive
                           mapping = aes(x = fct_rev(site), 
                                         y = value,
                                         fill = site,
                                         tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                          '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                          '<b>Student Group: </b>', var )
                                         ), 
                           stat = 'identity', alpha = 1, width = 0.7, color = 'grey')+
      geom_text_interactive(data = filter(DemographicsSchoolPlot, site != 'School'), #_interactive
                            mapping = aes(x = fct_rev(site), 
                                          y = value, 
                                          label = paste0(value, '%'), 
                                          tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                           '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                           '<b>Student Group: </b>', var )
                                          ), 
                            hjust = -0.1,
                            size = 7)+
      facet_wrap(vars(fct_rev(var)), 
                 ncol = 1)+
      scale_fill_manual(values=c("#315683", "#3688c8", 'grey')) +
      coord_flip(expand = F)+
      ylim(0, 100)+
      enrollmentTheme 
    
    ggiraph(print(p), 
            width_svg = 8, 
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
    
  } else if (!is.na(first(DemographicsSchool$homeBasedN))){  ### Plot for sites with home based learning ----
    p <-  ggplot(mapping = aes(x = 0, y = 100))+
      annotate('text', x = 0, y = 100, label = "Data withheld due to \nlarge proportion of \nhome-based enrollment",
               size = 14, color = 'grey')+
      theme_minimal()+
      theme(axis.title = element_blank(), 
            axis.text =element_blank(), 
            axis.ticks = element_blank(), 
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid = element_blank())
    
    ggiraph(print(p), 
            width_svg = 8, 
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
    
  } else if (SchoolNumber == '9234' | SchoolNumber == '9245' | SchoolNumber == '1001'){ #### Plots for Warren Tech due to secondary enrollment ----
    secondaryEnrollmentWide <- secondaryEnrollment %>%
      ungroup() %>% 
      filter(cdeSchoolNumber == SchoolNumber) %>% 
      filter(group == 'gt' |group == 'iep' |group == 'frl' | group == 'ell') %>% 
      pivot_wider(names_from =group, values_from = percent)   %>%
      select(cdeSchoolCode = cdeSchoolNumber, elPct = ell, spedPct = iep, gtPct = gt, percentFreeAndReduced = frl) %>%
      mutate(site = 'School') %>%
      ungroup() %>% 
      summarise(
        percentFreeAndReduced = na.omit(percentFreeAndReduced), 
        elPct = na.omit(elPct), 
        spedPct = na.omit(spedPct), 
        gtPct = na.omit(gtPct),
        site = first(site), 
        cdeSchoolCode = first(cdeSchoolCode)) %>% 
      group_by(site) %>% 
      summarise(percentFreeAndReduced = first(percentFreeAndReduced), 
                elPct = first(elPct), 
                spedPct = first(spedPct), 
                gtPct = first(gtPct), 
                site = first(site), 
                cdeSchoolCode = first(cdeSchoolCode)) %>% 
      bind_rows(DistrictDemos) %>%
      bind_rows(Statedemos) %>%
      select(-cdeSchoolCode, -School) %>% 
      mutate('FRL' = percentFreeAndReduced, 
             'ELL' = elPct, 
             'GT' = gtPct, 
             'Sped' = spedPct) %>%
      select(site, FRL, ELL, GT,  Sped) %>% 
      pivot_longer(cols = FRL:Sped, 
                   names_to = 'var', 
                   values_to = 'value') %>% 
      mutate(var = factor(var, 
                          levels = c('GT','Sped', 'ELL', 'FRL'), 
                          labels = c('Gifted & Talented','Special Education', 
                                     'English Language Learners', 
                                     'Free or Reduced Lunch Eligible'),
                          ordered = TRUE)) %>% 
      arrange() %>% 
      mutate(value = as.numeric(value), 
             value = round(value * 100)) %>% 
      mutate(valueLabel = case_when(
        value <.01 ~ paste0(round(value, 1), '%'), 
        TRUE ~ paste0(round(value), '%')))   %>% 
      mutate(site = factor(site, 
                           levels = c('School', 'District', 'State'), 
                           ordered = TRUE)) %>% 
      mutate(valueLabel = case_when(
        valueLabel == 'NA%' ~ 'fewer than 16 students in group', 
        TRUE ~ valueLabel
      )) %>% 
      mutate(value = case_when(
        is.na(value) ~ 0, 
        TRUE ~ value
      ))
    
    p <-  ggplot()+
      geom_bar_interactive(data = secondaryEnrollmentWide, mapping = aes(x = fct_rev(site), #_interactive
                                                                         y = value,
                                                                         fill = site, 
                                                                         tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                                                          '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                                                          '<b>Student Group: </b>', var )
                                                                         ), 
                           stat = 'identity', alpha = 1, width = 0.7, color = 'grey')+
      geom_text(data = secondaryEnrollmentWide, 
                mapping = aes(x = fct_rev(site), 
                              y = value, 
                              label = paste0(value, '%')), 
                hjust = -0.1,
                size = 6)+
      facet_wrap(vars(fct_rev(var)), 
                 ncol = 1)+
      scale_fill_manual(values=c("#315683", "#3688c8", 'grey')) +
      coord_flip(expand = F)+
      ylim(0, 100)+
      labs(caption = '*Includes students with Primary and Secondary Enrollment Status \nGrades 11-12')+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 16, hjust = 0#,  _interactive
                                                   # data_id = 'axis.text.y', 
                                                   # tooltip = 'Reporting levels include<br>State, District, and School'
                                                   ),
            panel.background = element_rect(fill = '#FFFFFF',
                                            colour = '#FFFFFF'),
            plot.background = element_rect(fill = '#F6F6F6',
                                           colour = '#F6F6F6'),
            plot.caption = element_text(size  = 14 #, _interactive
                                                    # data_id = 'plot.caption', 
                                                    # tooltip = '*Includes students with Primary and Secondary Enrollment Status \nGrades 11-12', 
                                                    # hover_css = "fill:#22a783;stroke:none;"
                                                    ),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_line(),
            panel.grid.major.y = element_blank(),
            legend.position = 'none',
            strip.text = element_text(size = 18#,_interactive
                                      # data_id = 'strip.text', 
                                      #             tooltip = 'Student programs include<br>Free or Reduced Lunch Eligible,<br>English Language Learners,<br>Special Education,<br>and Gifted Education'
                                      ), 
            strip.background = element_rect(color = 'white', fill = 'white'))
    
    ggiraph(print(p), 
            width_svg = 8, 
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
  } else if (SchoolNumber == '4408' ){ #### Plots for JVA due to JRLP program separation ----
    plotData <- masterSchoolDataProfileJVA %>%
      ungroup() %>% 
      filter(cdeSchoolCode == SchoolNumber) %>% 
      select(cdeSchoolCode, elPct, spedPct, gtPct, percentFreeAndReduced) %>%
      mutate(site = 'School') %>%
      ungroup() %>% 
      summarise(
        percentFreeAndReduced = na.omit(percentFreeAndReduced), 
        elPct = na.omit(elPct), 
        spedPct = na.omit(spedPct), 
        gtPct = na.omit(gtPct),
        site = first(site), 
        cdeSchoolCode = first(cdeSchoolCode)) %>% 
      group_by(site) %>% 
      summarise(percentFreeAndReduced = first(percentFreeAndReduced), 
                elPct = first(elPct), 
                spedPct = first(spedPct), 
                gtPct = first(gtPct), 
                site = first(site), 
                cdeSchoolCode = first(cdeSchoolCode)) %>% 
      bind_rows(DistrictDemos) %>%
      bind_rows(Statedemos) %>%
      select(-cdeSchoolCode, -School) %>% 
      mutate('FRL' = percentFreeAndReduced, 
             'ELL' = elPct, 
             'GT' = gtPct, 
             'Sped' = spedPct) %>%
      select(site, FRL, ELL, GT,  Sped) %>% 
      pivot_longer(cols = FRL:Sped, 
                   names_to = 'var', 
                   values_to = 'value') %>% 
      mutate(var = factor(var, 
                          levels = c('GT','Sped', 'ELL', 'FRL'), 
                          labels = c('Gifted & Talented','Special Education', 
                                     'English Language Learners', 
                                     'Free or Reduced Lunch Eligible'),
                          ordered = TRUE)) %>% 
      arrange() %>% 
      mutate(value = as.numeric(value), 
             value = round(value * 100)) %>% 
      mutate(valueLabel = case_when(
        value <.01 ~ paste0(round(value, 1), '%'), 
        TRUE ~ paste0(round(value), '%')))   %>% 
      mutate(site = factor(site, 
                           levels = c('School', 'District', 'State'), 
                           ordered = TRUE)) %>% 
      mutate(valueLabel = case_when(
        valueLabel == 'NA%' ~ 'fewer than 16 students in group', 
        TRUE ~ valueLabel
      )) %>% 
      mutate(value = case_when(
        is.na(value) ~ 0, 
        TRUE ~ value
      ))
    
    p <-  ggplot()+
      geom_bar_interactive(data = plotData, mapping = aes(x = fct_rev(site),# _interactive
                                                                         y = value,
                                                                         fill = site, 
                                                                         tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                                                          '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                                                          '<b>Student Group: </b>', var )
                                              ), 
                           stat = 'identity', alpha = 1, width = 0.7, color = 'grey')+
      geom_text(data = plotData, 
                mapping = aes(x = fct_rev(site), 
                              y = value, 
                              label = paste0(value, '%')), 
                hjust = -0.1,
                size = 6)+
      facet_wrap(vars(fct_rev(var)), 
                 ncol = 1)+
      scale_fill_manual(values=c("#315683", "#3688c8", 'grey')) +
      coord_flip(expand = F)+
      ylim(0, 100)+
      labs(caption = '*Excludes students in the Jeffco Remote Learning Program')+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 16, hjust = 0#, #_interactive
                                                   # data_id = 'axis.text.y', 
                                                   # tooltip = 'Reporting levels include<br>State, District, and School'
                                                   ),
            panel.background = element_rect(fill = '#FFFFFF',
                                            colour = '#FFFFFF'),
            plot.background = element_rect(fill = '#F6F6F6',
                                           colour = '#F6F6F6'),
            plot.caption = element_text(size  = 14 #_interactive, 
                                                    # data_id = 'plot.caption', 
                                                    # tooltip = '*Includes students with Primary and Secondary Enrollment Status \nGrades 11-12', 
                                                    # hover_css = "fill:#22a783;stroke:none;"
                                                    ),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_line(),
            panel.grid.major.y = element_blank(),
            legend.position = 'none',
            strip.text = element_text(size = 18 #, _interactive
                                                  # data_id = 'strip.text', 
                                                  # tooltip = 'Student programs include<br>Free or Reduced Lunch Eligible,<br>English Language Learners,<br>Special Education,<br>and Gifted Education'
                                                  ), 
            strip.background = element_rect(color = 'white', fill = 'white'))
    
    ggiraph(print(p), 
            width_svg = 8, 
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
    
  } else if (populationCheck$totalPKthru12Count < 16){ #Connections
    p <-  ggplot(mapping = aes(x = 0, y = 100))+
      annotate('text', x = 0, y = 100, label = "Data withheld due to \nsmall student population",
               size = 14, color = 'grey')+
      theme_minimal()+
      theme(axis.title = element_blank(), 
            axis.text =element_blank(), 
            axis.ticks = element_blank(), 
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid = element_blank())
    
    ggiraph(print(p), 
            width_svg = 8, 
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
  } else if (is.na(first(DemographicsSchool$homeBasedN))) { #plot for sites with no/small home based learning
    p <- ggplot()+
      geom_bar_interactive(data = DemographicsSchoolPlot, mapping = aes(x = fct_rev(site), #_interactive
                                                                        y = value,
                                                                        fill = site,
                                                                        tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                                                         '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                                                         '<b>Student Group: </b>', var )
                                                                        ), 
                           stat = 'identity', alpha = 1, width = 0.7, color = 'grey')+
      geom_text(data = DemographicsSchoolPlot, 
                mapping = aes(x = fct_rev(site), 
                              y = value, 
                              label = paste0(value, '%')), 
                hjust = -0.1,
                size = 7)+
      facet_wrap(vars(fct_rev(var)), 
                 ncol = 1)+
      scale_fill_manual(values=c("#315683", "#3688c8", 'grey')) +
      coord_flip(expand = F)+
      ylim(0, 100)+
      enrollmentTheme
    
    ggiraph(print(p), 
            width_svg = 8,
            # width = 1,
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
  } 
})

#  Race & Ethnicity Viz ----    

### Race & Ethnicity Viz - Students in population ----
output$simpleRace <- renderggiraph({
  
  SchoolNumber <- input$SchoolID
  
  
  populationCheck <- masterSchoolProfileData %>% 
    ungroup() %>%
    filter(cdeSchoolCode == SchoolNumber) %>% 
    select(totalPKthru12Count)
  
  Demographics <- masterSchoolProfileData %>%
    ungroup() %>% 
    mutate(ethnicityCount = as.numeric(ethnicityCount), 
           asianCount = as.numeric(asianCount), 
           indianCount = as.numeric(indianCount), 
           blackCount = as.numeric(blackCount), 
           hawaiianCount = as.numeric(hawaiianCount), 
           whiteCount = as.numeric(whiteCount), 
           twoOrMoreCount = as.numeric(twoOrMoreCount)
    ) %>% 
    mutate(ethnicityCount = case_when(
      ethnicityCount < 15  ~ NA_real_, 
      TRUE ~ ethnicityCount)) %>% 
    mutate(asianCount = case_when(
      asianCount < 15  ~ NA_real_, 
      TRUE ~ asianCount)) %>% 
    mutate(indianCount = case_when(
      indianCount < 15  ~ NA_real_, 
      TRUE ~ indianCount)) %>% 
    mutate(blackCount = case_when(
      blackCount < 15  ~ NA_real_, 
      TRUE ~ blackCount)) %>% 
    mutate(hawaiianCount = case_when(
      hawaiianCount < 15  ~ NA_real_, 
      TRUE ~ hawaiianCount)) %>% 
    mutate(whiteCount = case_when(
      whiteCount < 15  ~ NA_real_, 
      TRUE ~ whiteCount)) %>% 
    mutate(twoOrMoreCount = case_when(
      twoOrMoreCount < 15  ~ NA_real_, 
      TRUE ~ twoOrMoreCount)) %>% 
    mutate(ethnicityPct = case_when(
      is.na(ethnicityCount) ~ NA_real_, 
      TRUE ~ ethnicityPct)) %>% 
    mutate(asianPct = case_when(
      is.na(asianCount)  ~ NA_real_, 
      TRUE ~ asianPct)) %>% 
    mutate(indianPct = case_when(
      is.na(indianCount)  ~ NA_real_, 
      TRUE ~ indianPct)) %>% 
    mutate(blackPct = case_when(
      is.na(blackCount) ~ NA_real_, 
      TRUE ~ blackPct)) %>% 
    mutate(hawaiinPct = case_when(
      is.na(hawaiianCount)  ~ NA_real_, 
      TRUE ~ hawaiinPct)) %>% 
    mutate(whitePct = case_when(
      is.na(whiteCount)  ~ NA_real_, 
      TRUE ~ whitePct)) %>% 
    mutate(twoOrMorePct = case_when(
      is.na(twoOrMoreCount)  ~ NA_real_, 
      TRUE ~ twoOrMorePct)) %>% 
    select(cdeSchoolCode, 
           School = "veryAbbreviatedName", 
           ethnicityPct, whitePct, asianPct, indianPct, blackPct, hawaiinPct, twoOrMorePct, homeBasedN) %>% 
    mutate(cdeSchoolCode = str_pad(cdeSchoolCode, width = 4, side = 'left', pad = '0'))
  
  DemographicsJVA <- masterSchoolDataProfileJVA %>%
    ungroup() %>% 
    mutate(ethnicityCount = as.numeric(ethnicityCount), 
           asianCount = as.numeric(asianCount), 
           indianCount = as.numeric(indianCount), 
           blackCount = as.numeric(blackCount), 
           hawaiianCount = as.numeric(hawaiianCount), 
           whiteCount = as.numeric(whiteCount), 
           twoOrMoreCount = as.numeric(twoOrMoreCount)
    ) %>% 
    mutate(ethnicityCount = case_when(
      ethnicityCount < 15  ~ NA_real_, 
      TRUE ~ ethnicityCount)) %>% 
    mutate(asianCount = case_when(
      asianCount < 15  ~ NA_real_, 
      TRUE ~ asianCount)) %>% 
    mutate(indianCount = case_when(
      indianCount < 15  ~ NA_real_, 
      TRUE ~ indianCount)) %>% 
    mutate(blackCount = case_when(
      blackCount < 15  ~ NA_real_, 
      TRUE ~ blackCount)) %>% 
    mutate(hawaiianCount = case_when(
      hawaiianCount < 15  ~ NA_real_, 
      TRUE ~ hawaiianCount)) %>% 
    mutate(whiteCount = case_when(
      whiteCount < 15  ~ NA_real_, 
      TRUE ~ whiteCount)) %>% 
    mutate(twoOrMoreCount = case_when(
      twoOrMoreCount < 15  ~ NA_real_, 
      TRUE ~ twoOrMoreCount)) %>% 
    mutate(ethnicityPct = case_when(
      is.na(ethnicityCount) ~ NA_real_, 
      TRUE ~ ethnicityPct)) %>% 
    mutate(asianPct = case_when(
      is.na(asianCount)  ~ NA_real_, 
      TRUE ~ asianPct)) %>% 
    mutate(indianPct = case_when(
      is.na(indianCount)  ~ NA_real_, 
      TRUE ~ indianPct)) %>% 
    mutate(blackPct = case_when(
      is.na(blackCount) ~ NA_real_, 
      TRUE ~ blackPct)) %>% 
    mutate(hawaiinPct = case_when(
      is.na(hawaiianCount)  ~ NA_real_, 
      TRUE ~ hawaiinPct)) %>% 
    mutate(whitePct = case_when(
      is.na(whiteCount)  ~ NA_real_, 
      TRUE ~ whitePct)) %>% 
    mutate(twoOrMorePct = case_when(
      is.na(twoOrMoreCount)  ~ NA_real_, 
      TRUE ~ twoOrMorePct)) %>% 
    select(cdeSchoolCode, 
           School = "veryAbbreviatedName", 
           ethnicityPct, whitePct, asianPct, indianPct, blackPct, hawaiinPct, twoOrMorePct) %>% 
    mutate(cdeSchoolCode = str_pad(cdeSchoolCode, width = 4, side = 'left', pad = '0'))
  
  
  DistrictDemos <- Demographics %>% 
    filter(cdeSchoolCode == "9998") %>% 
    mutate(site = 'District')
  
  DemographicsSchool <-Demographics %>% 
    filter(cdeSchoolCode == SchoolNumber) 
  
  Statedemos <- data.frame(cdeSchoolCode = '9999', 
                           School = "State", 
                           ethnicityPct = .345, 
                           whitePct = .519, 
                           asianPct = .032, 
                           indianPct = .006, 
                           blackPct = .045, 
                           hawaiinPct = .003, 
                           twoOrMorePct = .050,
                           site = "State")
  
  DemographicsSchoolPlot <-Demographics %>% 
    filter(cdeSchoolCode == SchoolNumber) %>% 
    mutate(site = 'School') %>% 
    bind_rows(DistrictDemos) %>% 
    bind_rows(Statedemos) %>% 
    select(-cdeSchoolCode) %>% 
    select(site, ethnicityPct, whitePct, asianPct, indianPct, blackPct, hawaiinPct, twoOrMorePct) %>% 
    pivot_longer(cols = ethnicityPct:twoOrMorePct, 
                 names_to = 'var', 
                 values_to = 'value') %>% 
    mutate(valueLabel = case_when(
      value <.01 ~ paste0(round(value*100, 1), '%'), 
      TRUE ~ paste0(round(value*100), '%')))   %>% 
    mutate(var = factor(var, 
                        levels = c('asianPct', 'blackPct', 'ethnicityPct', 'indianPct', 'hawaiinPct', 'twoOrMorePct', 'whitePct' ),
                        labels = c('Asian', 'Black or African American', 'Hispanic or Latino/a/x', 'American Indian or Alaska Native', 'Native Hawaiian or Pacific Islander', 'Two or more races', 'White'),
                        ordered = TRUE)) %>% 
    arrange(desc(var)) %>% 
    mutate(site = factor(site, 
                         levels = c('School', 'District', 'State'), 
                         ordered = TRUE)) %>% 
    mutate(valueLabel = case_when(
      value <.01 ~ paste0(round(value*100, 1), '%'), 
      TRUE ~ paste0(round(value*100), '%'))) %>% 
    mutate(valueLabel = case_when(
      valueLabel == 'NA%' ~ 'fewer than 16 students in group', 
      TRUE ~ valueLabel
    )) %>% 
    mutate(value = case_when(
      is.na(value) ~ 0, 
      TRUE ~ value
    ))
  
  
  #Set a theme for each plot in conditional to follow
  enrollmentTheme <- theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.text.x = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.text.y = element_text(size = 18, hjust = 0),
                           panel.background = element_rect(fill = '#FFFFFF',
                                                           colour = '#FFFFFF'),
                           plot.background = element_rect(fill = '#F6F6F6',
                                                          colour = '#F6F6F6'),
                           plot.caption = element_text(size  = 13 #, _interactive
                                                                   # data_id = 'plot.caption', 
                                                                   # tooltip = '*Includes students with Primary and Secondary Enrollment Status \nGrades 11-12', 
                                                                   # hover_css = "fill:#22a783;stroke:none;"
                                                       ),
                           panel.grid.minor.x = element_blank(),
                           panel.grid.major.x = element_line(),
                           panel.grid.major.y = element_blank(),
                           legend.position = 'none',
                           strip.text = element_text(size = 18, #_interactive
                                                                 face = 'bold',
                                                                 hjust = 0#,
                                                                 # data_id = 'strip.text', 
                                                                 # tooltip = 'Race / Ethnicity includes:
                                                                 #   Asian,
                                                                 #   <br>Black or African American,
                                                                 #   <br>Hispanic or Latino, 
                                                                 #   <br>American Indian or Alaska Native,
                                                                 #   <br>Native Hawaiian or Pacific Islander,
                                                                 #   <br>Two or more races, 
                                                                 #   <br>and White'
                                                     ), 
                           strip.background = element_rect(color = 'white', fill = 'white'))
  
  if (SchoolNumber == 9998) { #### Plot district values ----
    p <-ggplot()+
      geom_bar_interactive(data = filter(DemographicsSchoolPlot, site != "School"),#_interactive
                           mapping = aes(x = fct_rev(site), 
                                         y = round(value*100),
                                         fill = site, 
                                         tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                          '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                          '<b>Student Group: </b>', var )
                                         ), 
                           stat = 'identity', alpha = 1, width = 0.7, color = 'grey')+ #_interactive
      geom_text_interactive(data = filter(DemographicsSchoolPlot, site != "School"),
                            mapping = aes(x = fct_rev(site), 
                                          y = value*100, 
                                          label = valueLabel, 
                                          tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                           '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                           '<b>Student Group: </b>', var )
                                          ), 
                            hjust = -0.15,
                            size = 7)+
      facet_wrap(vars(var), 
                 ncol = 1)+
      scale_fill_manual(values=c("#315683", "#3688c8", 'grey')) +
      coord_flip(expand = F)+
      ylim(0, 100)+
      enrollmentTheme
    
    ggiraph(print(p), 
            width_svg = 8, 
            height_svg = 8,
            width = 1,
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
    
  } else if (SchoolNumber == '9234' | SchoolNumber == '9245' | SchoolNumber == '1001') { ### Plot Warren Tech with secondary enrollment ----
    
    secondaryEnrollmentWide <- secondaryEnrollment %>%
      ungroup() %>% 
      filter(cdeSchoolNumber == SchoolNumber) %>% 
      filter(group != 'gt', group != 'iep', group != 'frl',  group != 'ell') %>% 
      filter(enrollmentType == 'S') %>% #n > 15, 
      pivot_wider(names_from =group, values_from = percent)   %>%
      select(n, cdeSchoolCode = cdeSchoolNumber, ethnicityPct = ethnicityStudent, 
             asianPct = raceAsianStudent, whitePct = raceWhiteStudent, twoOrMorePct = twoOrMoreMinority) %>%
      mutate(site = 'School' , 
             School = "Warren Tech Central") %>%
      ungroup() %>% 
      summarise(cdeSchoolCode = first(cdeSchoolCode), 
                School = first(School), 
                ethnicityPct = na.omit(ethnicityPct), 
                asianPct = na.omit(asianPct), 
                whitePct = na.omit(whitePct), 
                twoOrMorePct = na.omit(twoOrMorePct), 
                site = first(site), 
                n = n) %>% 
      bind_rows(DistrictDemos) %>%
      bind_rows(Statedemos) %>%
      filter(n > 15| is.na(n)) %>% 
      select(-cdeSchoolCode, -School) %>% 
      group_by(site, ethnicityPct, asianPct, whitePct,  
               indianPct,
               blackPct,
               hawaiinPct,
               twoOrMorePct, 
               homeBasedN
               ) %>% 
      summarise() %>% 
      pivot_longer(cols = ethnicityPct:twoOrMorePct, 
                   names_to = 'var', 
                   values_to = 'value') %>% 
      mutate(valueLabel = case_when(
        value <.01 ~ paste0(round(value*100, 1), '%'), 
        TRUE ~ paste0(round(value*100), '%')))   %>% 
      mutate(var = factor(var, 
                          levels = c('asianPct', 'blackPct', 'ethnicityPct', 'indianPct', 'hawaiinPct', 'twoOrMorePct', 'whitePct' ),
                          labels = c('Asian', 'Black or African American', 'Hispanic or Latino/a/x', 'American Indian or Alaska Native', 'Native Hawaiian or Pacific Islander', 'Two or more races', 'White'),
                          ordered = TRUE)) %>% 
      arrange(desc(var)) %>% 
      mutate(site = factor(site, 
                           levels = c('School', 'District', 'State'), 
                           ordered = TRUE)) %>% 
      mutate(valueLabel = case_when(
        valueLabel == 'NA%' ~ 'fewer than 16 students in group', 
        TRUE ~ valueLabel
      )) %>% 
      mutate(value = case_when(
        is.na(value) ~ 0, 
        TRUE ~ value
      ))
    
    p <- ggplot()+
      geom_bar_interactive(data = secondaryEnrollmentWide, #_interactive
                           mapping = aes(x = fct_rev(site), 
                                         y = round(value*100),
                                         fill = site, 
                                         tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                          '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                          '<b>Student Group: </b>', var )
                                         ), 
                           stat = 'identity', alpha = 1, width = 0.7, color = 'grey')+
      geom_text_interactive(data = secondaryEnrollmentWide, #_interactive
                            mapping = aes(x = fct_rev(site), 
                                          y = value*100, 
                                          label = valueLabel, 
                                          tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                           '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                           '<b>Student Group: </b>', var )
                                          ), 
                            hjust = -0.15,
                            size = 5)+
      facet_wrap(vars(var), 
                 ncol = 1) +
      scale_fill_manual(values=c("#315683", "#3688c8", 'grey')) +
      coord_flip(expand = F) +
      ylim(0, 100) +
      labs(caption = '*Includes students with Primary and Secondary Enrollment Status \nGrades 11-12')+
      enrollmentTheme +
      theme(axis.text.y = element_text(size = 15))

        ggiraph(print(p), 
            width_svg = 8, 
            height_svg = 8,
            width = 1,
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
  } else if (SchoolNumber == '4408' ){ #### Plots for JVA due to JRLP program separation ----
    plotData <- DemographicsJVA %>%
      ungroup() %>% 
      filter(cdeSchoolCode == SchoolNumber) %>% 
      select(cdeSchoolCode, ethnicityPct, asianPct, indianPct, blackPct, whitePct,hawaiinPct, twoOrMorePct) %>%
      mutate(site = 'School' , 
             School = "JVA") %>%
      ungroup() %>% 
      bind_rows(DistrictDemos) %>%
      bind_rows(Statedemos) %>%
      select(-cdeSchoolCode, -School) %>% 
      group_by(site, ethnicityPct,asianPct, whitePct,  indianPct, blackPct, hawaiinPct,twoOrMorePct, homeBasedN) %>% 
      summarise() %>% 
      pivot_longer(cols = ethnicityPct:twoOrMorePct, 
                   names_to = 'var', 
                   values_to = 'value') %>% 
      mutate(valueLabel = case_when(
        value <.01 ~ paste0(round(value*100, 1), '%'), 
        TRUE ~ paste0(round(value*100), '%')))   %>% 
      mutate(var = factor(var, 
                          levels = c('asianPct', 'blackPct', 'ethnicityPct', 'indianPct', 'hawaiinPct', 'twoOrMorePct', 'whitePct' ),
                          labels = c('Asian', 'Black or African American', 'Hispanic or Latino/a/x', 'American Indian or Alaska Native', 'Native Hawaiian or Pacific Islander', 'Two or more races', 'White'),
                          ordered = TRUE)) %>% 
      arrange(desc(var)) %>% 
      mutate(site = factor(site, 
                           levels = c('School', 'District', 'State'), 
                           ordered = TRUE)) %>% 
      mutate(valueLabel = case_when(
        valueLabel == 'NA%' ~ 'fewer than 16 students in group', 
        TRUE ~ valueLabel
      )) %>% 
      mutate(value = case_when(
        is.na(value) ~ 0, 
        TRUE ~ value
      ))
    
    p <- ggplot()+
      geom_bar_interactive(data = plotData, #_interactive
                           mapping = aes(x = fct_rev(site), 
                                         y = round(value*100),
                                         fill = site, 
                                         tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                          '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                          '<b>Student Group: </b>', var )
                                         ), 
                           stat = 'identity', alpha = 1, width = 0.7, color = 'grey') +
      geom_text_interactive(data = plotData, #_interactive
                            mapping = aes(x = fct_rev(site), 
                                          y = value*100, 
                                          label = valueLabel, 
                                          tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                           '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                           '<b>Student Group: </b>', var )
                                          ), 
                            hjust = -0.15,
                            size = 5) +
      facet_wrap(vars(var), 
                 ncol = 1) +
      scale_fill_manual(values=c("#315683", "#3688c8", 'grey')) +
      coord_flip(expand = F) +
      ylim(0, 100) +
      labs(caption = '*Excludes Students from Jeffco Remote Learning Program')+
      enrollmentTheme +
      theme(axis.text.y = element_text(size = 15))
    
    ggiraph(print(p), 
            width_svg = 8, 
            height_svg = 8,
            width = 1,
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
    
  } else if (!is.na(first(DemographicsSchool$homeBasedN))) {  #plot  home based learning
    p <- ggplot(mapping = aes(x = 0, y = 100))+
      annotate('text', x = 0, y = 100, label = "Data withheld due to \nlarge proportion of \nhome-based enrollment", 
               size = 14, color = 'grey')+
      theme_minimal()+
      theme(axis.title = element_blank(), 
            axis.text =element_blank(), 
            axis.ticks = element_blank(), 
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid = element_blank())
    
    ggiraph(print(p), 
            width_svg = 8, 
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
    
  }else if (populationCheck$totalPKthru12Count < 16){ #Connections
    p <-  ggplot(mapping = aes(x = 0, y = 100))+
      annotate('text', x = 0, y = 100, label = "Data withheld due to \nsmall student population",
               size = 14, color = 'grey')+
      theme_minimal()+
      theme(axis.title = element_blank(), 
            axis.text =element_blank(), 
            axis.ticks = element_blank(), 
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid = element_blank())
    
    ggiraph(print(p), 
            width_svg = 8, 
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
  } else if (is.na(first(DemographicsSchool$homeBasedN))) { #plot home no based learning
    p <- ggplot()+
      geom_bar_interactive(data = DemographicsSchoolPlot, #_interactive
                           mapping = aes(x = fct_rev(site), 
                                         y = round(value*100),
                                         fill = site,
                                         tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                          '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                          '<b>Student Group: </b>', var )
                                         ), 
                           stat = 'identity', alpha = 1, width = 0.7, color = 'grey')+
      geom_text_interactive(data = DemographicsSchoolPlot, #_interactive
                            mapping = aes(x = fct_rev(site), 
                                          y = value*100, 
                                          label = valueLabel, 
                                          tooltip = paste0('<b>Reporting level: </b>', site, '<br>',
                                                           '<b>Percentage of students in group: </b>', valueLabel, '<br>',
                                                           '<b>Student Group: </b>', var )
                                          ),
                            hjust = -0.15,
                            size = 6) +
      facet_wrap(vars(var), 
                 ncol = 1)+
      scale_fill_manual(values=c("#315683", "#3688c8", 'grey')) +
      coord_flip(expand = F)+
      ylim(0, 100)+
      enrollmentTheme
    
    ggiraph(print(p), 
            width_svg = 8, 
            height_svg = 8,
            width = 1,
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
  }
})

# School Designations Table ----
output$SchoolDesignationsServer <- renderDataTable({
  
  SchoolNumber <- input$SchoolID
  
  if (SchoolNumber != '9998') { ### Display All Schools with data ----
    SchoolDesignations <- masterSchoolProfileData %>%
      filter(cdeSchoolCode == input$SchoolID) %>% #enrollment() %>% 
      ungroup() %>% 
      select(cdeSchoolCode, veryAbbreviatedName, schoolCategory, 
             "Charter School" = charterSchool, 
             "Option School" = optionSchool, 
             "Alternative Ed." = alternateEducationCampus, 
             "Title I Designated" = titleISchool) %>% 
      mutate(Neighborhood = case_when(
        schoolCategory == 'Neighborhood' ~ TRUE, 
        TRUE ~ FALSE
      )) %>% 
      gather(key = "Class", value = Label, "Charter School":"Neighborhood") %>%  #Title I Designated
      mutate(Label = case_when(
        Label == "TRUE" ~ as.character(icon("check-circle", verify_fa = FALSE)),
        Label == "FALSE" ~ as.character(icon("ban", verify_fa = FALSE)))) %>% 
      mutate(fillerCol = "", fillerCol2 = "", fillerCol3 = "", fillerCol4 = "", fillerCol5 = "", fillerCol6 = "") %>% 
      select(Class, Label) %>% 
      filter(!is.na(Label)) %>% 
      spread(key = Class, value = Label)
    
    datatable(SchoolDesignations,
              class = c('nowrap display'),
              rownames = FALSE, 
              colnames = c('Alternative <br>Education', 'Charter <br>School',
                           'Neighborhood <br>School', 'Option <br>School', 'Title I<br> School'),
              escape = FALSE,
              options = list(pageLength = 10, 
                             dom = 'tp', 
                             paging = FALSE, 
                             ordering = FALSE, 
                             headerCallback = JS(
                               "function( thead, data, start, end, display ) {
                                    $(thead).closest('thead').find('th').each(function(){
                                    $(this).css('color', 'grey')});}"),
                             initComplete = JS(
                               "function(settings) {
                                    var table = settings.oInstance.api(); 
                                    $(table.table().node()).removeClass('no-footer');}"),
                             columnDefs = list(list(className = 'dt-center', targets = 0:4))))%>% 
      formatStyle(columns = c('Alternative Ed.', 'Charter School', 'Neighborhood', 'Option School', 'Title I Designated'),
                  backgroundColor = '#EBF1F6')
  } else { #blank display if no school designation
    
  }
})

# Choice Plots ----
### Choice In ----
output$choiceInPlotPie <- renderggiraph({
  
  SchoolNumber <- input$SchoolID
  
  choiceInTheme <- theme(legend.position = 'right', 
                         legend.title = element_text(size = 14),
                         legend.text = element_text(size = 14), 
                         legend.key.size = unit(0.5, 'cm'), 
                         legend.spacing.y = unit(0.5, 'cm'))
  

  
  if (SchoolNumber == 9998) { #### Plot district ----
    ChoiceBySchool <- ChoiceDataBySchool %>% 
      filter(!is.na(SchoolName)) %>% #Because of Warren Tech South 2021-22
      mutate(total = sum(Count)) %>% 
      group_by(Classification, total) %>% 
      summarise(count = sum(Count)) %>% 
      mutate(percent = (count/total)) %>% 
      mutate(legendTitle = case_when(
        Classification == 'EnrolledStudentsChoiceIn'~ "Outside of boundaries", 
        Classification == 'EnrolledStudentsLiveInBoundary' ~"Within the boundaries"), 
        SchoolName = 'District', 
        percent = paste0(round(percent *100), '%'), 
        count = formatC(count, format = 'd', big.mark = ','))
    
    bp <- ggplot(data = ChoiceBySchool, 
                 mapping = aes(x = SchoolName, y = percent, fill = legendTitle, label = percent)) +
      geom_bar_interactive( #
        aes(tooltip = paste0('<b>', legendTitle, '</b>', '<BR>', 'Count of students:','<br>',  count, '<br>','Percent of students:', '<br>', percent)
        ),
                           width = 1, 
                           size = 1, 
                           color = "white", 
                           stat = "identity") +
      ylab (NULL) +
      xlab (NULL)
    
    choiceIn <- bp + 
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("#315683", "#3688c8")) +
      theme_void() +
      labs(fill = "Students who choiced into \nthe school from")+
choiceInTheme
    interactivePlot(choiceIn)
    
  } else if (SchoolNumber == '9234' | SchoolNumber == '9245'| SchoolNumber == '1001') { ### Warren Tech ----
    #Needed for 2021-22 only
    warrenSouth <- data.frame(SchoolName = rep(c('Warren Tech South'), 2), 
                              CDESchoolCode = rep(c('1001'), 2), 
                              Classification = c('EnrolledStudentsChoiceIn', 'EnrolledStudentsLiveInBoundary'), 
                              Count = c(193, 0), 
                              Percent = c('100%', '0%'), 
                              totalStudents = c(193, 0))
    
    ChoiceBySchool <- ChoiceDataBySchool %>% 
      #Needed for 2021-22 only
      rbind(warrenSouth) %>%
      mutate(percent = str_remove(Percent, '%')) %>% 
      mutate(percent = as.numeric(percent)) %>% 
      filter(CDESchoolCode == SchoolNumber) %>% 
      mutate(legendTitle = case_when(
        Classification == 'EnrolledStudentsChoiceIn'~ "Outside of boundaries", 
        Classification == 'EnrolledStudentsLiveInBoundary' ~"Within the boundaries"), 
        totalStudents = formatC(totalStudents, format = 'd', big.mark = ','))
    
    bp <- ggplot(data = ChoiceBySchool, mapping = aes(x = SchoolName, y = percent, fill = legendTitle)) +
      geom_bar_interactive( #_interactive
        aes(tooltip = paste0('<b>', legendTitle, '</b>', '<BR>',
                                                'Count of students:','<br>',  totalStudents, '<br>',
                                                'Percent of students:', '<br>', percent, '%')),
                           color = "white", 
                           stat = "identity") +
      ylab (NULL) +
      xlab (NULL)
    
    choiceIn <- bp + 
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("#315683", "#3688c8")) +
      theme_void() +
      labs(fill = "Students who choiced into \nthe school from") +
choiceInTheme
    
    interactivePlot(choiceIn)
  } else if (SchoolNumber %in% unique(ChoiceDataBySchool$CDESchoolCode)) { #### Schools with attendance boundaries ----
    ChoiceBySchool <- ChoiceDataBySchool %>% 
      mutate(percent = str_remove(Percent, '%')) %>% 
      mutate(percent = as.numeric(percent)) %>% 
      filter(CDESchoolCode == SchoolNumber) %>% 
      mutate(legendTitle = case_when(
        Classification == 'EnrolledStudentsChoiceIn'~ "Outside of boundaries", 
        Classification == 'EnrolledStudentsLiveInBoundary' ~"Within the boundaries"), 
        Count = formatC(Count, format = 'd', big.mark = ','))
    
    bp <- ggplot(data = ChoiceBySchool, mapping = aes(x = SchoolName, y = percent, fill = legendTitle)) +
      geom_bar_interactive( #_interactive
        aes(tooltip = paste0('<b>', legendTitle, '</b>', '<BR>',
                                                'Count of students:','<br>',  Count, '<br>',
                                                'Percent of students:', '<br>', percent, '%')),
                           color = "white", 
                           stat = "identity") +
      ylab (NULL) +
      xlab (NULL)
    
    choiceIn <- bp + 
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("#315683", "#3688c8")) +
      theme_void() +
      labs(fill = "Students who choiced into \nthe school from") +
 choiceInTheme

 interactivePlot(choiceIn)
  } else { #schools with NO attendance boundaries
    ChoiceBySchool <- ChoiceDataBySchool %>% 
      mutate(total = sum(Count)) %>% 
      group_by(Classification, total) %>% 
      summarise(count = sum(Count)) %>% 
      mutate(percent = (count/total)) %>% 
      mutate(legendTitle = case_when(
        Classification == 'EnrolledStudentsChoiceIn'~ "Outside of boundaries", 
        Classification == 'EnrolledStudentsLiveInBoundary' ~"Within the boundaries"), 
        SchoolName = 'District', 
        percent = paste0(round(percent *100), '%'), 
        count = formatC(count, format = 'd', big.mark = ','))
    
    bp <- ggplot(data = ChoiceBySchool, mapping = aes(x = SchoolName, y = percent, fill = legendTitle)) +
      geom_bar_interactive( #_interactive
        aes(tooltip = paste0('<b>', legendTitle, '</b>', '<BR>', 'Count of students:','<br>',  count, '<br>','Percent of students:', '<br>', percent)),
                           width = 1, 
                           size = 1, 
                           color = "white", 
                           stat = "identity") +
      annotate('rect', xmin = .2, xmax = 1, ymin = 0.25, ymax = 0.75, fill = 'white')+
      annotate('text',  y = .1,  x = .7, label  = "No attendance \n boundaries \nfor this site.", size = 6, color = '#515151')+
      ylab (NULL) +
      xlab (NULL)
    
    choiceIn <- bp + 
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("white", "white")) +
      theme_void() +
      labs(fill = "Students who choiced into \nthe school from") +
choiceInTheme +
      theme(legend.title = element_blank(),
            legend.text = element_blank())
    interactivePlot(choiceIn)
  }
})
### Choice Out ----    
output$choiceOutPlotBar <- renderggiraph({
  
  SchoolNumber <- input$SchoolID
  
  AreaChoiceData <- AreaChoiceData %>% 
    filter(Count != 0)
  
  choiceOutTheme <- theme(plot.background = element_rect(fill = 'white'), 
                                panel.background = element_rect(fill = 'white'),
                                axis.title.x = element_blank(), 
                                axis.text.x = element_blank(),
                                axis.ticks.x = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y= element_blank(), 
                                axis.text.y = element_blank(),
                                panel.grid.major = element_blank(), 
                                panel.grid.minor = element_blank(),
                                legend.title = element_blank(),
                                legend.text = element_blank(),
                                legend.key.size = unit(0.5, 'cm'), 
                                legend.spacing.y = unit(0.5, 'cm'),
                                legend.position = "right") 
  
  if (SchoolNumber %in% unique(AreaChoiceData$CDESchoolCode)) { ##### Schools with attendance boundaries ----
    
    ChoiceByArea <- AreaChoiceData %>% 
      filter(CDESchoolCode == SchoolNumber) %>% 
      mutate(legendTitle = case_when(
        Classification == "AreaKidsChoiceOut" ~ 'Choiced out', 
        Classification == "AreaKidsEnrolledAtThisSchool" ~ 'Remained'), 
        Count = formatC(Count, format = 'd', big.mark = ',')) %>% 
      mutate(Percent = str_remove(Percent, '%'), 
             Percent = as.numeric(Percent))
    
    bp <- ggplot(data = ChoiceByArea, 
                 mapping = aes(x = SchoolName, y = Percent, fill = legendTitle)) +
      geom_bar_interactive(stat = 'identity',  #_interactive
                           aes(tooltip = paste0('<b>', legendTitle, '</b>', '<BR>',
                                                'Count of students: ',
                                                '<br>', Count, '<br>',
                                                'Percent of students:', '<br>', Percent, "%"))
                           ) +
      scale_fill_manual(values =  c("#315683", "#3688c8"))+
     choiceOutTheme +
      theme(legend.title = element_text(size = 14),
            legend.text = element_text(size = 14)) +
      labs(fill = "Students with within \nthe boundaries")
    
    choiceOut <- bp + 
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "Students who choiced out \nof the school")+
      theme(legend.position = 'right', 
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14), 
            legend.key.size = unit(0.5, 'cm'), 
            legend.spacing.y = unit(0.5, 'cm'))
    choiceOut
    
    interactivePlot(choiceOut)
    
  } else { ### Schools with NO attendance boundaries ----
    ChoiceByArea <- AreaChoiceData %>% 
      filter(CDESchoolCode == '0030') %>% 
      mutate(legendTitle = case_when(
        Classification == "AreaKidsChoiceOut" ~ 'Choiced out', 
        Classification == "AreaKidsEnrolledAtThisSchool" ~ 'Remained'), 
        Count = formatC(Count, format = 'd', big.mark = ',')) %>% 
      mutate(Percent = str_remove(Percent, '%'), 
             Percent = as.numeric(Percent))
    
    choiceOut <- ggplot(data = ChoiceByArea, 
                        mapping = aes(x = SchoolName, y = Percent, fill = legendTitle)) +
      geom_bar_interactive(stat = 'identity', #_interactive
                           aes(tooltip = paste0('<b>', legendTitle, '</b>', '<BR>',
                                                'Count of students: ',
                                                '<br>', Count, '<br>',
                                                'Percent of students:', '<br>', Percent, "%"))
               ) +
      scale_fill_manual(values = c('white', 'white'))+
      annotate('rect', xmin = 0, xmax = 1, ymin = 0.25, ymax = 0.75, fill = 'lightgrey')+
      annotate('text',  y = 70,  x = 1, label  = "There are no attendance \nboundaries for this site.", size = 7, color = '#515151')+
choiceOutTheme
    
    interactivePlot(choiceOut)
  }
})