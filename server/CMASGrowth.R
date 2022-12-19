#### CMAS 2021 ####
# 
# output$grade4Math <- renderUI({
#   SchoolNumber <- input$SchoolID
#   
#   currentCMAS <-currentCmasValue(.test = "CMAS", .Grade = 'Gr4', .ContentName = "MATH") %>% 
#     filter(CDESchoolNumber == SchoolNumber) %>% 
#     filter(stat == 'percentMet_Exceed')
#   
#   noGrade <- masterSchoolProfileData %>% 
#     filter(grade4count == '-') %>% 
#     distinct(cdeSchoolCode) 
#   
#   nonSufficientdata <- currentCMAS 
#   
#   if (SchoolNumber %in% noGrade$cdeSchoolCode) {
#     #Empty- Show nothing
#   } else if (!SchoolNumber %in% noGrade$cdeSchoolCode & nrow(nonSufficientdata)<1){
#     valueBox(value = ' ', 
#              subtitle =  'Grade 4, Non sufficient data', 
#              width = NULL)
#   } else {
#     valueBox(value = paste0(round(currentCMAS$value), '%'), 
#              subtitle =  HTML('4th Grade <br><i>participation rate: 92% of 341 students</i>'), 
#              width = NULL)
#   }
# })
# 
# output$grade6Math <- renderUI({
#   SchoolNumber <- input$SchoolID
#   
#   currentCMAS <-currentCmasValue(.test = "CMAS", .Grade = 'Gr6', .ContentName = "MATH") %>% 
#     filter(CDESchoolNumber == SchoolNumber) %>% 
#     filter(stat == 'percentMet_Exceed')
#   
#   noGrade <- masterSchoolProfileData %>% 
#     filter(grade6count == '-') %>% 
#     distinct(cdeSchoolCode) 
#   
#   nonSufficientdata <- currentCMAS 
#   
#   if (SchoolNumber %in% noGrade$cdeSchoolCode) {
#     #Empty- Show nothing
#   } else if (!SchoolNumber %in% noGrade$cdeSchoolCode & nrow(nonSufficientdata)<1){
#     valueBox(value = ' ', 
#              subtitle =  'Grade 6, Non sufficient data', 
#              width = NULL)
#   } else {
#     valueBox(value = paste0(round(currentCMAS$value), '%'), 
#              subtitle =  HTML('6th Grade <br><i>participation rate: 92% of 341 students</i>'), 
#              width = NULL)
#   }
# })
# 
# output$grade8Math <- renderUI({
#   SchoolNumber <- input$SchoolID
#   
#   currentCMAS <-currentCmasValue(.test = "CMAS", .Grade = 'Gr8', .ContentName = "MATH") %>% 
#     filter(CDESchoolNumber == SchoolNumber) %>% 
#     filter(stat == 'percentMet_Exceed')
#   
#   noGrade <- masterSchoolProfileData %>% 
#     filter(grade8count == '-') %>% 
#     distinct(cdeSchoolCode) 
#   
#   nonSufficientdata <- currentCMAS 
#   
#   if (SchoolNumber %in% noGrade$cdeSchoolCode) {
#     #Empty- Show nothing
#   } else if (!SchoolNumber %in% noGrade$cdeSchoolCode & nrow(nonSufficientdata)<1){
#     valueBox(value = ' ', 
#              subtitle =  'Grade 8, Non sufficient data', 
#              width = NULL)
#   } else {
#     valueBox(value = paste0(round(currentCMAS$value), '%'), 
#              subtitle = HTML('8th Grade <br><i>participation rate: 92% of 341 students</i>'), 
#              width = NULL)
#   }
# })
# 
# output$psat2021EBRWGrade9 <- renderUI({
#   
#   SchoolNumber <- input$SchoolID
#   
#   currentCMAS <-currentCmasValue(.test = "SAT", .Grade = 'Gr9', .ContentName = "LANGUAGE ARTS") %>% 
#     filter(CDESchoolNumber == SchoolNumber) %>% 
#     filter(stat == 'percentMet_Exceed')
#   
#   noGrade <- masterSchoolProfileData %>% 
#     filter(grade9count == '-') %>% 
#     distinct(cdeSchoolCode) 
#   
#   nonSufficientdata <- currentCMAS 
#   
#   if (SchoolNumber %in% noGrade$cdeSchoolCode) {
#     #Empty- Show nothing
#   } else if (!SchoolNumber %in% noGrade$cdeSchoolCode & nrow(nonSufficientdata)<1){
#     valueBox(value = ' ', 
#              subtitle =  'Grade 9, Non sufficient data', 
#              width = NULL)
#   } else {
#     valueBox(value = paste0(round(currentCMAS$value), '%'), 
#              subtitle = HTML('9th grade students within College & Career Readiness Benchmark<br><i><b>Participation rate: </b><br>92% of 352 students</i>'), 
#              icon = icon("book"),
#              color = 'blue', 
#              width = NULL)
#   }
# })
# 
# output$psat2021MathGrade9 <- renderUI({
#   SchoolNumber <- input$SchoolID
#   
#   currentCMAS <-currentCmasValue(.test = "SAT", .Grade = 'Gr9', .ContentName = "MATH") %>% 
#     filter(CDESchoolNumber == SchoolNumber) %>% 
#     filter(stat == 'percentMet_Exceed')
#   
#   noGrade <- masterSchoolProfileData %>% 
#     filter(grade9count == '-') %>% 
#     distinct(cdeSchoolCode) 
#   
#   nonSufficientdata <- currentCMAS 
#   
#   if (SchoolNumber %in% noGrade$cdeSchoolCode) {
#     #Empty- Show nothing
#   } else if (!SchoolNumber %in% noGrade$cdeSchoolCode & nrow(nonSufficientdata)<1){
#     valueBox(value = ' ', 
#              subtitle =  'Grade 9, Non sufficient data', 
#              width = NULL)
#   } else {
#     valueBox(value = paste0(round(currentCMAS$value), '%'), 
#              subtitle = HTML('9th Grade students within College & Career Readiness Benchmark<br><i><b>Participation rate: </b><br>92% of 352 students</i>'), 
#              icon = icon("chart-bar"),
#              color = 'blue', 
#              width = NULL)
#   }
# })
# 
# 
# output$psat2021EBRWGrade10 <- renderUI({
#   
#   SchoolNumber <- input$SchoolID
#   
#   currentCMAS <-currentCmasValue(.test = "SAT", .Grade = 'Gr10', .ContentName = "LANGUAGE ARTS") %>% 
#     filter(CDESchoolNumber == SchoolNumber) %>% 
#     filter(stat == 'percentMet_Exceed')
#   
#   noGrade <- masterSchoolProfileData %>% 
#     filter(grade10count == '-') %>% 
#     distinct(cdeSchoolCode) 
#   
#   nonSufficientdata <- currentCMAS 
#   
#   if (SchoolNumber %in% noGrade$cdeSchoolCode) {
#     #Empty- Show nothing
#   } else if (!SchoolNumber %in% noGrade$cdeSchoolCode & nrow(nonSufficientdata)<1){
#     valueBox(value = ' ', 
#              subtitle =  'Grade 10, Non sufficient data', 
#              width = NULL)
#   } else {
#     valueBox(value = paste0(round(currentCMAS$value), '%'), 
#              subtitle = HTML('10th Grade <br><i><b>Participation rate: </b><br>90% of 364 students</i>'), 
#              color = 'blue', 
#              icon = icon("book"),
#              width = NULL)
#   }
# })
# 
# output$psat2021MathGrade10 <- renderUI({
#   SchoolNumber <- input$SchoolID
#   
#   currentCMAS <-currentCmasValue(.test = "SAT", .Grade = 'Gr10', .ContentName = "MATH") %>% 
#     filter(CDESchoolNumber == SchoolNumber) %>% 
#     filter(stat == 'percentMet_Exceed')
#   
#   noGrade <- masterSchoolProfileData %>% 
#     filter(grade9count == '-') %>% 
#     distinct(cdeSchoolCode) 
#   
#   nonSufficientdata <- currentCMAS 
#   
#   if (SchoolNumber %in% noGrade$cdeSchoolCode) {
#     #Empty- Show nothing
#   } else if (!SchoolNumber %in% noGrade$cdeSchoolCode & nrow(nonSufficientdata)<1){
#     valueBox(value = ' ', 
#              subtitle =  'Grade 10, Non sufficient data', 
#              width = NULL)
#   } else {
#     valueBox(value = paste0(round(currentCMAS$value), '%'), 
#              subtitle = HTML('10th Grade <br><i><b>Participation rate: </b><br>87% of 364 students</i>'), 
#              color = 'blue', 
#              icon = icon("chart-bar"),
#              width = NULL)
#   }
# })


#### * CMAS Growth Cohort Reading 2021 ####
# output$CMASGrowthPlot2021 <- renderPlot({
#   
#   SchoolNumber <- input$SchoolID
#   
#   CMASvar <- cmasGrowthCombo %>% 
#     mutate(gradeLevel = recode(gradeLevel,
#                                '05' = '2021 Fifth Grade Growth \nfrom Grade 3 to Grade 5', 
#                                '06' = '2021 Sixth Grade Growth \nfrom Grade 4 to Grade 6', 
#                                '07' = '2021 Seventh Grade Growth \nfrom Grade 5 to Grade 7', 
#                                '08' = '2021 Eighth Grade Growth \nfrom Grade 6 to Grade 8')) %>% 
#     mutate(medianSgpCohort = as.numeric(medianSgpCohort)) %>% 
#     mutate(medianSgpCohort = medianSgpCohort /100) %>% 
#     group_by(schNumber, subject, gradeLevel) %>% 
#     summarise(medianSgpCohort = first(medianSgpCohort)) %>% 
#     filter(schNumber == SchoolNumber) %>% 
#     filter(subject == 'ENGLISH LANGUAGE ARTS') %>% 
#     filter(!is.na(medianSgpCohort))
#   
#   if (first(is.na(CMASvar$medianSgpCohort))| nrow(CMASvar) == 0){
#     
#     ggplot() +
#       annotate('text', x = 0, y = 0, 
#                label = 'Insufficient growth data\ndue to small number of students testing', 
#                size = 6, 
#                color = 'grey') +
#       theme_minimal() +
#       baseGrowthTheme +
#       theme(axis.text.x = element_blank(), 
#             axis.text.y = element_blank())
#   } else {
#     
#     ggplot(data = CMASvar) +
#       geom_bar(data = CMASHundredMGP, 
#                mapping = aes(x = group, fill = percent),
#                position="fill",
#                width = 0.15) +
#       geom_point(data = CMASvar, 
#                  mapping = aes(y = medianSgpCohort), 
#                  x = 1,  
#                  size = 8, 
#                  alpha = 0.5,
#                  shape = 18) +
#       geom_text(aes(y = medianSgpCohort, label = ordinal(medianSgpCohort*100)), 
#                 x = 1, 
#                 vjust = -1, 
#                 size = 6) +
#       scale_fill_manual(values = c('#3688c8', '#22a783', '#e2a331', '#d8274a')) +
#       coord_flip()+
#       facet_wrap(~gradeLevel, 
#                  ncol = 2) +
#       ylim(0, 1)+
#       theme(axis.title.x = element_blank(), 
#             axis.title.y = element_blank(), 
#             plot.title = element_text(size = 10, color = 'grey'),
#             axis.text.x = element_blank(), 
#             axis.text.y = element_blank(), 
#             axis.ticks.x = element_blank(), 
#             axis.ticks.y = element_blank(), 
#             panel.background = element_rect(fill = '#EBF1F6', 
#                                             colour = '#EBF1F6'),
#             plot.background = element_rect(fill = '#EBF1F6', 
#                                            colour = '#EBF1F6'),
#             panel.spacing.y=unit(.2, "lines"), 
#             panel.grid.minor = element_blank(), 
#             panel.grid.major = element_blank(), 
#             legend.position = 'none', 
#             strip.text = element_text(size = 16))
#   }
#   
# })

#### * CMAS Growth Baseline Reading 2021 ####

# output$CMASGrowthPlot2021New <- renderPlot({
#   
#   schoolNumber <- input$SchoolID
#   library(cowplot)
#   
#   cmasGrowthBaseline <- cmasGrowthCombo %>% 
#     mutate(gradeLevel = recode(gradeLevel,
#                                '05' = '2021 Fifth Grade Growth \nfrom Grade 3 to Grade 5', 
#                                '06' = '2021 Sixth Grade Growth \nfrom Grade 4 to Grade 6', 
#                                '07' = '2021 Seventh Grade Growth \nfrom Grade 5 to Grade 7', 
#                                '08' = '2021 Eighth Grade Growth \nfrom Grade 6 to Grade 8')) %>% 
#     mutate(medianSgpBaseline = as.numeric(medianSgpBaseline)) %>% 
#     mutate(medianSgpBaseline = medianSgpBaseline /100) %>% 
#     group_by(schNumber, subject, gradeLevel, binN, sgpBaseline) %>% 
#     summarise(medianSgpBaseline = first(medianSgpBaseline)) %>% 
#     filter(schNumber == schoolNumber) %>% 
#     filter(subject == 'ENGLISH LANGUAGE ARTS') %>% 
#     filter(!is.na(medianSgpBaseline))
#   
#   grade3to5 <- cmasGrowthBaseline %>%  
#     filter(gradeLevel == '2021 Fifth Grade Growth \nfrom Grade 3 to Grade 5')
#   
#   grade5to7 <- cmasGrowthBaseline %>% 
#     filter(gradeLevel == '2021 Seventh Grade Growth \nfrom Grade 5 to Grade 7')
#   
#   cmasGrowthBaselineState5 <- cmasGrowthCombo %>% 
#     filter(schNumber == '9999') %>% 
#     filter(subject == 'ENGLISH LANGUAGE ARTS') %>% 
#     filter(gradeLevel == '05') %>% 
#     mutate(medianSgpBaseline = as.numeric(medianSgpBaseline))
#   
#   cmasGrowthBaselineState7 <- cmasGrowthCombo %>% 
#     filter(schNumber == '9999') %>% 
#     filter(subject == 'ENGLISH LANGUAGE ARTS') %>% 
#     filter(gradeLevel == '07') %>% 
#     mutate(medianSgpBaseline = as.numeric(medianSgpBaseline))
#   
#   plot3to5 <-  ggplot(data = grade3to5, 
#                       mapping = aes(x = sgpBaseline, 
#                                     y = binN)) +
#     geom_col(data = grade3to5, 
#              mapping = aes(x = sgpBaseline, 
#                            y = binN),
#              color = '#71797E', 
#              fill = '#71797E', 
#              width = 8) +
#     geom_vline(aes(xintercept = grade3to5$medianSgpBaseline*100, 
#                    color = paste0('School: ', 
#                                   factor(ordinal(round(grade3to5$medianSgpBaseline*100))))), 
#                size = 2, 
#                show.legend = TRUE) +
#     geom_vline(aes(xintercept = cmasGrowthBaselineState5$medianSgpBaseline, 
#                    color = paste0('State: ', ordinal(cmasGrowthBaselineState5$medianSgpBaseline))), 
#                size = 2, 
#                show.legend = TRUE) +
#     facet_wrap(~gradeLevel)+
#     labs(color = ' ') +
#     scale_color_manual(values = c('black', '#3688C8')) +
#     scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                        labels = c(' ',  '10th', '20th', '30th',
#                                   '40th', '50th', '60th', '70th',
#                                   '80th', '90th', '99th')) + 
#     theme_minimal() +
#     baseGrowthTheme +
#     theme(strip.text = element_text(size = 16), 
#           strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
#           panel.background = element_rect(fill = '#EBF1F6', 
#                                           colour = '#EBF1F6'),
#           plot.background = element_rect(fill = '#EBF1F6', 
#                                          colour = '#EBF1F6'))
#   
#   plot5to7 <-  ggplot(data = grade5to7, 
#                       mapping = aes(x = sgpBaseline, 
#                                     y = binN)) +
#     geom_col(color = '#71797E', 
#              fill = '#71797E', 
#              width = 8) +
#     geom_vline(aes(xintercept = grade5to7$medianSgpBaseline*100, 
#                    color = paste0('School: ', 
#                                   factor(ordinal(round(grade5to7$medianSgpBaseline*100))))), 
#                size = 2, 
#                show.legend = TRUE) +
#     scale_color_manual(values = c('black', '#3688C8')) +
#     geom_vline(aes(xintercept = cmasGrowthBaselineState7$medianSgpBaseline, 
#                    color = paste0('State: ', ordinal(cmasGrowthBaselineState7$medianSgpBaseline))), 
#                size = 2, 
#                show.legend = TRUE) +
#     facet_wrap(~gradeLevel)+
#     labs(color = ' ') +
#     scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                        labels = c(' ',  '10th', '20th', '30th',
#                                   '40th', '50th', '60th', '70th',
#                                   '80th', '90th', '99th')) + 
#     theme_minimal() +
#     baseGrowthTheme +
#     theme(strip.text = element_text(size = 16), 
#           strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
#           panel.background = element_rect(fill = '#EBF1F6', 
#                                           colour = '#EBF1F6'),
#           plot.background = element_rect(fill = '#EBF1F6', 
#                                          colour = '#EBF1F6'))
#   
#   plot3to5NoState <-  ggplot(data = grade3to5, 
#                              mapping = aes(x = sgpBaseline, 
#                                            y = binN)) +
#     geom_col(data = grade3to5, 
#              mapping = aes(x = sgpBaseline, 
#                            y = binN),
#              color = '#71797E', 
#              fill = '#71797E', 
#              width = 8) +
#     geom_vline(aes(xintercept = grade3to5$medianSgpBaseline*100, 
#                    color = paste0('School: ', 
#                                   factor(ordinal(round(grade3to5$medianSgpBaseline*100))))), 
#                size = 2, 
#                show.legend = TRUE) +
#     facet_wrap(~gradeLevel)+
#     labs(color = ' ') +
#     scale_color_manual(values = c('black', '#3688C8')) +
#     scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                        labels = c(' ',  '10th', '20th', '30th',
#                                   '40th', '50th', '60th', '70th',
#                                   '80th', '90th', '99th')) + 
#     theme_minimal() +
#     baseGrowthTheme +
#     theme(strip.text = element_text(size = 16), 
#           strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
#           panel.background = element_rect(fill = '#EBF1F6', 
#                                           colour = '#EBF1F6'),
#           plot.background = element_rect(fill = '#EBF1F6', 
#                                          colour = '#EBF1F6'))
#   
#   plot5to7NoState <-  ggplot(data = grade5to7, 
#                              mapping = aes(x = sgpBaseline, 
#                                            y = binN)) +
#     geom_col(color = '#71797E', 
#              fill = '#71797E', 
#              width = 8) +
#     geom_vline(aes(xintercept = grade5to7$medianSgpBaseline*100, 
#                    color = paste0('School: ', 
#                                   factor(ordinal(round(grade5to7$medianSgpBaseline*100))))), 
#                size = 2, 
#                show.legend = TRUE) +
#     facet_wrap(~gradeLevel)+
#     labs(color = ' ') +
#     scale_color_manual(values = c('black', '#3688C8')) +
#     scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                        labels = c(' ',  '10th', '20th', '30th',
#                                   '40th', '50th', '60th', '70th',
#                                   '80th', '90th', '99th')) + 
#     theme_minimal() +
#     baseGrowthTheme +
#     theme(strip.text = element_text(size = 16), 
#           strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
#           panel.background = element_rect(fill = '#EBF1F6', 
#                                           colour = '#EBF1F6'),
#           plot.background = element_rect(fill = '#EBF1F6', 
#                                          colour = '#EBF1F6'))
#   
#   if(input$cmasNewGrowthELA){
#     if (nrow(grade5to7) == 0 & nrow(grade3to5) == 0){
#       
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Insufficient growth data\ndue to small number of students testing', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else if (schoolNumber %in% k8Schools & nrow(grade5to7) >0) { #K8 school with 7th grade growth
#       
#       plot_grid(plot3to5, plot5to7) 
#     } else if (schoolNumber %in% k8Schools & nrow(grade5to7) == 0) { #K8 school with no 7th grade growth
#       
#       plot_grid(plot3to5) 
#     } else if (schoolNumber %in% k5Schools & nrow(grade3to5) > 0){
#       
#       plot_grid(plot3to5) 
#     } else if (schoolNumber %in% middleSchools & nrow(grade5to7) > 0){
#       
#       plot_grid(plot5to7) 
#     } else if (schoolNumber %in% multiLevel & nrow(grade3to5) == 0){
#       
#       plot_grid(plot5to7) 
#     } else if (schoolNumber == '9998') {
#       plot3to5 <-  ggplot(data = grade3to5, 
#                           mapping = aes(x = sgpBaseline, 
#                                         y = binN)) +
#         geom_col(data = grade3to5, 
#                  mapping = aes(x = sgpBaseline, 
#                                y = binN),
#                  color = '#71797E', 
#                  fill = '#71797E', 
#                  width = 8) +
#         geom_vline(aes(xintercept = grade3to5$medianSgpBaseline*100, 
#                        color = paste0('District: ', 
#                                       factor(ordinal(round(grade3to5$medianSgpBaseline*100))))), 
#                    size = 2, 
#                    show.legend = TRUE) +
#         geom_vline(aes(xintercept = cmasGrowthBaselineState5$medianSgpBaseline, 
#                        color = paste0('State: ', ordinal(cmasGrowthBaselineState5$medianSgpBaseline))), 
#                    size = 2, 
#                    show.legend = TRUE) +
#         facet_wrap(~gradeLevel)+
#         labs(color = ' ') +
#         scale_color_manual(values = c('black', '#3688C8')) +
#         scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                            labels = c(' ',  '10th', '20th', '30th',
#                                       '40th', '50th', '60th', '70th',
#                                       '80th', '90th', '99th')) + 
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(strip.text = element_text(size = 16), 
#               strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
#               panel.background = element_rect(fill = '#EBF1F6', 
#                                               colour = '#EBF1F6'),
#               plot.background = element_rect(fill = '#EBF1F6', 
#                                              colour = '#EBF1F6'))
#       
#       plot5to7 <-  ggplot(data = grade5to7, 
#                           mapping = aes(x = sgpBaseline, 
#                                         y = binN)) +
#         geom_col(color = '#71797E', 
#                  fill = '#71797E', 
#                  width = 8) +
#         geom_vline(aes(xintercept = grade5to7$medianSgpBaseline*100, 
#                        color = paste0('District: ', 
#                                       factor(ordinal(round(grade5to7$medianSgpBaseline*100))))), 
#                    size = 2, 
#                    show.legend = TRUE) +
#         scale_color_manual(values = c('black', '#3688C8')) +
#         geom_vline(aes(xintercept = cmasGrowthBaselineState7$medianSgpBaseline, 
#                        color = paste0('State: ', ordinal(cmasGrowthBaselineState7$medianSgpBaseline))), 
#                    size = 2, 
#                    show.legend = TRUE) +
#         facet_wrap(~gradeLevel)+
#         labs(color = ' ') +
#         scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                            labels = c(' ',  '10th', '20th', '30th',
#                                       '40th', '50th', '60th', '70th',
#                                       '80th', '90th', '99th')) + 
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(strip.text = element_text(size = 16), 
#               strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
#               panel.background = element_rect(fill = '#EBF1F6', 
#                                               colour = '#EBF1F6'),
#               plot.background = element_rect(fill = '#EBF1F6', 
#                                              colour = '#EBF1F6'))
#       
#       plot_grid(plot3to5, plot5to7) 
#     } else if (nrow(grade3to5) == 0){
#       
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Insufficient growth data\ndue to small number of students testing', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else if (nrow(grade5to7) == 0){ #No data for school
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Insufficient growth data\ndue to small number of students testing', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else {
#       
#     }
#   } else {
#     if (nrow(grade5to7) == 0 & nrow(grade3to5) == 0){
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Insufficient growth data\ndue to small number of students testing', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else if (schoolNumber %in% k8Schools & nrow(grade5to7) > 0){
#       plot_grid(plot3to5NoState, plot5to7NoState) 
#     } else if (schoolNumber %in% k8Schools & nrow(grade5to7) == 0){
#       plot_grid(plot3to5NoState) 
#     } else if (schoolNumber %in% k5Schools & nrow(grade3to5) > 0){
#       plot_grid(plot3to5NoState) 
#     } else if (schoolNumber %in% middleSchools & nrow(grade5to7) > 0) {
#       plot_grid(plot5to7NoState) 
#     }  else if (schoolNumber %in% c('0965', '1730')){
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Insufficient growth data\ndue to small number of students testing', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else if ((nrow(grade3to5) == 0 & schoolNumber != '8793' & schoolNumber != '7701' & schoolNumber != '4404')| nrow(grade5to7) == 0 ){
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Insufficient growth data\ndue to small number of students testing', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank())
#     } else if (schoolNumber %in% multiLevel & nrow(grade3to5) > 0 & schoolNumber != '9998'){ #collegiate Academy
#       plot_grid(plot3to5NoState) 
#     } else if (nrow(grade3to5) == 0 & schoolNumber %in% c('8793', '7701', '4404')) {
#       plot_grid(plot5to7NoState) 
#     } else if (schoolNumber == '9998') {
#       plot3to5NoState <-  ggplot(data = grade3to5, 
#                                  mapping = aes(x = sgpBaseline, 
#                                                y = binN)) +
#         geom_col(data = grade3to5, 
#                  mapping = aes(x = sgpBaseline, 
#                                y = binN),
#                  color = '#71797E', 
#                  fill = '#71797E', 
#                  width = 8) +
#         geom_vline(aes(xintercept = grade3to5$medianSgpBaseline*100, 
#                        color = paste0('District: ', 
#                                       factor(ordinal(round(grade3to5$medianSgpBaseline*100))))), 
#                    size = 2, 
#                    show.legend = TRUE) +
#         facet_wrap(~gradeLevel)+
#         labs(color = ' ') +
#         scale_color_manual(values = c('black', '#971B72')) +
#         scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                            labels = c(' ',  '10th', '20th', '30th',
#                                       '40th', '50th', '60th', '70th',
#                                       '80th', '90th', '99th')) + 
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(strip.text = element_text(size = 16), 
#               strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
#               panel.background = element_rect(fill = '#EBF1F6', 
#                                               colour = '#EBF1F6'),
#               plot.background = element_rect(fill = '#EBF1F6', 
#                                              colour = '#EBF1F6'))
#       
#       plot5to7NoState <-  ggplot(data = grade5to7, 
#                                  mapping = aes(x = sgpBaseline, 
#                                                y = binN)) +
#         geom_col(color = '#71797E', 
#                  fill = '#71797E', 
#                  width = 8) +
#         geom_vline(aes(xintercept = grade5to7$medianSgpBaseline*100, 
#                        color = paste0('District: ', 
#                                       factor(ordinal(round(grade5to7$medianSgpBaseline*100))))), 
#                    size = 2, 
#                    show.legend = TRUE) +
#         scale_color_manual(values = c('black', '#3688C8')) +
#         facet_wrap(~gradeLevel)+
#         labs(color = ' ') +
#         scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                            labels = c(' ',  '10th', '20th', '30th',
#                                       '40th', '50th', '60th', '70th',
#                                       '80th', '90th', '99th')) + 
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(strip.text = element_text(size = 16), 
#               strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
#               panel.background = element_rect(fill = '#EBF1F6', 
#                                               colour = '#EBF1F6'),
#               plot.background = element_rect(fill = '#EBF1F6', 
#                                              colour = '#EBF1F6'))
#       
#       plot_grid(plot3to5NoState, plot5to7NoState) 
#     } else {
#       
#     }
#   }
# })

#### * CMAS Growth Cohort Math 2021####

# output$CMASGrowthPlotMath2021 <- renderPlot({
#   
#   SchoolNumber <- input$SchoolID
#   
#   CMASvar <- cmasGrowthCombo %>% 
#     mutate(gradeLevel = recode(gradeLevel,
#                                '05' = '2021 Fifth Grade Growth \nfrom Grade 3 to Grade 5', 
#                                '06' = '2021 Sixth Grade Growth \nfrom Grade 4 to Grade 6', 
#                                '07' = '2021 Seventh Grade Growth \nfrom Grade 5 to Grade 7', 
#                                '08' = '2021 Eighth Grade Growth \nfrom Grade 6 to Grade 8')) %>% 
#     mutate(medianSgpCohort = as.numeric(medianSgpCohort)) %>% 
#     mutate(medianSgpCohort = medianSgpCohort /100) %>% 
#     group_by(schNumber, subject, gradeLevel) %>% 
#     summarise(medianSgpCohort = first(medianSgpCohort)) %>% 
#     filter(schNumber == SchoolNumber) %>% 
#     filter(subject == 'MATH') %>% 
#     filter(!is.na(medianSgpCohort)) %>% 
#     mutate(gradeLevel = factor(gradeLevel, 
#                                levels = c('2021 Sixth Grade Growth \nfrom Grade 4 to Grade 6', 
#                                           '2021 Eighth Grade Growth \nfrom Grade 6 to Grade 8'), 
#                                ordered = T))
#   
#   
#   naChecker <- cmasGrowthCombo %>% 
#     filter(schNumber == SchoolNumber) %>% 
#     filter(subject == 'MATH') %>% 
#     summarise(medianSgpCohort = first(medianSgpCohort))
#   
#   if (naChecker$medianSgpCohort %in% c('-')){
#     ggplot() +
#       annotate('text', x = 0, y = 0, 
#                label = 'Insufficient growth data\ndue to small number of students testing', 
#                size = 6, 
#                color = 'grey') +
#       theme_minimal() +
#       baseGrowthTheme +
#       theme(axis.text.x = element_blank(), 
#             axis.text.y = element_blank())
#   } else if (nrow(CMASvar) == 0) {
#     
#     ggplot() +
#       annotate('text', x = 0, y = 0, 
#                label = 'Growth data for grade levels at the selected school\nare not available', 
#                size = 6, 
#                color = 'grey') +
#       theme_minimal() +
#       baseGrowthTheme +
#       theme(axis.text.x = element_blank(), 
#             axis.text.y = element_blank())
#     
#   } else {
#     
#     ggplot(data = CMASvar) +
#       geom_bar(data = CMASHundredMGP, 
#                mapping = aes(x = group, fill = percent),
#                position="fill",
#                width = 0.15) +
#       geom_point(data = CMASvar, 
#                  mapping = aes(y = medianSgpCohort), 
#                  x = 1,  
#                  size = 8, 
#                  alpha = 0.5,
#                  shape = 18) +
#       geom_text(aes(y = medianSgpCohort, label = ordinal(medianSgpCohort*100)), 
#                 x = 1, 
#                 vjust = -1, 
#                 size = 6) +
#       scale_fill_manual(values = c('#3688c8', '#22a783', '#e2a331', '#d8274a')) +
#       coord_flip()+
#       facet_wrap(~gradeLevel, 
#                  ncol = 2) +
#       ylim(0, 1)+
#       theme(axis.title.x = element_blank(), 
#             axis.title.y = element_blank(), 
#             plot.title = element_text(size = 10, color = 'grey'),
#             axis.text.x = element_blank(), 
#             axis.text.y = element_blank(), 
#             axis.ticks.x = element_blank(), 
#             axis.ticks.y = element_blank(), 
#             panel.background = element_rect(fill = '#EBF1F6', 
#                                             colour = '#EBF1F6'),
#             plot.background = element_rect(fill = '#EBF1F6', 
#                                            colour = '#EBF1F6'),
#             panel.spacing.y=unit(.2, "lines"), 
#             panel.grid.minor = element_blank(), 
#             panel.grid.major = element_blank(), 
#             legend.position = 'none', 
#             strip.text = element_text(size = 16))
#     
#   }
#   
# })

######## * CMAS Growth Baseline Math 2021 ########
# output$CMASGrowthPlot2021NewMath <- renderPlot({
#   schoolNumber <- input$SchoolID
#   
#   cmasGrowthBaseline <- cmasGrowthCombo %>% 
#     mutate(gradeLevel = recode(gradeLevel,
#                                '05' = '2021 Fifth Grade Growth \nfrom Grade 3 to Grade 5', 
#                                '06' = '2021 Sixth Grade Growth \nfrom Grade 4 to Grade 6', 
#                                '07' = '2021 Seventh Grade Growth \nfrom Grade 5 to Grade 7', 
#                                '08' = '2021 Eighth Grade Growth \nfrom Grade 6 to Grade 8')) %>% 
#     mutate(medianSgpBaseline = as.numeric(medianSgpBaseline)) %>% 
#     mutate(medianSgpBaseline = medianSgpBaseline /100) %>% 
#     group_by(schNumber, subject, gradeLevel, binN, sgpBaseline) %>% 
#     summarise(medianSgpBaseline = first(medianSgpBaseline)) %>% 
#     filter(schNumber == schoolNumber) %>% 
#     filter(subject == 'MATH') 
#   
#   grade4to6 <- cmasGrowthBaseline %>%  
#     filter(gradeLevel == '2021 Sixth Grade Growth \nfrom Grade 4 to Grade 6')
#   
#   grade6to8 <- cmasGrowthBaseline %>% 
#     filter(gradeLevel == '2021 Eighth Grade Growth \nfrom Grade 6 to Grade 8')
#   
#   cmasGrowthBaselineState6 <- cmasGrowthCombo %>% 
#     filter(schNumber == '9999') %>% 
#     filter(subject == 'MATH') %>% 
#     filter(gradeLevel == '06') %>% 
#     mutate(medianSgpBaseline = as.numeric(medianSgpBaseline))
#   
#   cmasGrowthBaselineState8 <- cmasGrowthCombo %>% 
#     filter(schNumber == '9999') %>% 
#     filter(subject == 'MATH') %>% 
#     filter(gradeLevel == '08') %>% 
#     mutate(medianSgpBaseline = as.numeric(medianSgpBaseline))
#   
#   
#   naChecker4to6 <- cmasGrowthBaseline %>% 
#     filter(schNumber == schoolNumber) %>% 
#     filter(subject == 'MATH') %>% 
#     filter(is.na(medianSgpBaseline) & gradeLevel == 'Growth from\nGrade 4 to Grade 6')
#   
#   naChecker6to8 <- cmasGrowthBaseline %>% 
#     filter(schNumber == schoolNumber) %>% 
#     filter(subject == 'MATH') %>% 
#     filter(is.na(medianSgpBaseline) & gradeLevel == 'Growth from\nGrade 6 to Grade 8')
#   
#   plot4to6 <-  ggplot(data = grade4to6, 
#                       mapping = aes(x = sgpBaseline, 
#                                     y = binN)) +
#     geom_col(data = grade4to6, 
#              mapping = aes(x = sgpBaseline, 
#                            y = binN),
#              color = '#71797E', 
#              fill = '#71797E', 
#              width = 8) +
#     geom_vline(aes(xintercept = grade4to6$medianSgpBaseline*100, 
#                    color = paste0('School: ', 
#                                   factor(ordinal(round(grade4to6$medianSgpBaseline*100))))), 
#                size = 2, 
#                show.legend = TRUE) +
#     geom_vline(aes(xintercept = cmasGrowthBaselineState6$medianSgpBaseline, 
#                    color = paste0('State: ', ordinal(cmasGrowthBaselineState6$medianSgpBaseline))), 
#                size = 2, 
#                show.legend = TRUE) +
#     facet_wrap(~gradeLevel)+
#     labs(color = ' ') +
#     scale_color_manual(values = c('black', '#3688C8')) +
#     scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                        labels = c(' ',  '10th', '20th', '30th',
#                                   '40th', '50th', '60th', '70th',
#                                   '80th', '90th', '99th')) + 
#     theme_minimal() +
#     baseGrowthTheme 
#   
#   plot6to8 <-  ggplot(data = grade6to8, 
#                       mapping = aes(x = sgpBaseline, 
#                                     y = binN)) +
#     geom_col(color = '#71797E', 
#              fill = '#71797E', 
#              width = 8) +
#     geom_vline(aes(xintercept = grade6to8$medianSgpBaseline*100, 
#                    color = paste0('School: ',factor(ordinal(round(grade6to8$medianSgpBaseline*100))))
#     ), 
#     size = 2, 
#     show.legend = TRUE) +
#     scale_color_manual(values = c('black', '#3688C8')) +
#     geom_vline(aes(xintercept = cmasGrowthBaselineState8$medianSgpBaseline, 
#                    color = paste0('State: ', ordinal(cmasGrowthBaselineState8$medianSgpBaseline))), 
#                size = 2, 
#                show.legend = TRUE) +
#     facet_wrap(~gradeLevel)+
#     labs(color = ' ') +
#     scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                        labels = c(' ',  '10th', '20th', '30th',
#                                   '40th', '50th', '60th', '70th',
#                                   '80th', '90th', '99th')) + 
#     theme_minimal() +
#     baseGrowthTheme 
#   
#   # theme(legend.position = 'top', 
#   # legend.background = element_rect(fill = 'lightgrey'))
#   
#   plot4to6NoState <-  ggplot(data = grade4to6, 
#                              mapping = aes(x = sgpBaseline, 
#                                            y = binN)) +
#     geom_col(data = grade4to6, 
#              mapping = aes(x = sgpBaseline, 
#                            y = binN),
#              color = '#71797E', 
#              fill = '#71797E', 
#              width = 8) +
#     geom_vline(aes(xintercept = grade4to6$medianSgpBaseline*100, 
#                    color =   paste0('School: ',factor(ordinal(round(grade4to6$medianSgpBaseline*100))))), 
#                size = 2, 
#                show.legend = TRUE) +
#     facet_wrap(~gradeLevel)+
#     labs(color = ' ') +
#     scale_color_manual(values = c('black', '#3688C8')) +
#     scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                        labels = c(' ',  '10th', '20th', '30th',
#                                   '40th', '50th', '60th', '70th',
#                                   '80th', '90th', '99th')) + 
#     theme_minimal() +
#     baseGrowthTheme
#   
#   plot6to8NoState <-  ggplot(data = grade6to8, 
#                              mapping = aes(x = sgpBaseline, 
#                                            y = binN)) +
#     geom_col(color = '#71797E', 
#              fill = '#71797E', 
#              width = 8) +
#     geom_vline(aes(xintercept = grade6to8$medianSgpBaseline*100, 
#                    color = paste0('School: ', 
#                                   factor(ordinal(round(grade6to8$medianSgpBaseline*100))))), 
#                size = 2, 
#                show.legend = TRUE) +
#     facet_wrap(~gradeLevel)+
#     labs(color = ' ') +
#     scale_color_manual(values = c('black', '#3688C8')) +
#     scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                        labels = c(' ',  '10th', '20th', '30th',
#                                   '40th', '50th', '60th', '70th',
#                                   '80th', '90th', '99th')) + 
#     theme_minimal() +
#     baseGrowthTheme
#   
#   if(input$cmasNewGrowthMath){
#     if ((schoolNumber %in% k6School  &  schoolNumber != '5972')| schoolNumber == 7701){ #K6 schools or collegiate Academy
#       plot_grid(plot4to6)
#     } else if(nrow(grade4to6) < 2 & nrow(cmasGrowthBaseline) > 0 & schoolNumber != '5036' & schoolNumber != '5892' & schoolNumber != '4798'){ #jeffco open
#       plot_grid(plot6to8) 
#     } else if (nrow(grade4to6) < 2){ #No math growth because K-5 schools
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Growth data for grade levels at the selected school\nare not available', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else if ((nrow(grade6to8) == 0 | nrow(naChecker6to8)  > 0 | nrow(naChecker4to6) > 0) & (schoolNumber %in% middleSchools | schoolNumber %in% k6School | schoolNumber %in% multiLevel| schoolNumber %in% k8Schools | schoolNumber %in% k5Schools)) { #middle schools with no data
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Insufficient growth data\ndue to small number of students testing', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else if (is.na(grade4to6$medianSgpBaseline)){
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Insufficient growth data\ndue to small number of students testing', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else if  (schoolNumber %in% k8Schools | schoolNumber %in% middleSchools| schoolNumber %in% multiLevel) { #k8 or middle schools with data
#       plot_grid(plot4to6, plot6to8) 
#     } else if (schoolNumber == '9998') { #district
#       plot4to6 <-  ggplot(data = grade4to6, 
#                           mapping = aes(x = sgpBaseline, 
#                                         y = binN)) +
#         geom_col(data = grade4to6, 
#                  mapping = aes(x = sgpBaseline, 
#                                y = binN),
#                  color = '#71797E', 
#                  fill = '#71797E', 
#                  width = 8) +
#         geom_vline(aes(xintercept = grade4to6$medianSgpBaseline*100, 
#                        color = paste0('District: ',factor(ordinal(round(grade4to6$medianSgpBaseline*100))))), 
#                    size = 2, 
#                    show.legend = TRUE) +
#         geom_vline(aes(xintercept = cmasGrowthBaselineState6$medianSgpBaseline, 
#                        color = paste0('State: ', ordinal(cmasGrowthBaselineState6$medianSgpBaseline))), 
#                    size = 2, 
#                    show.legend = TRUE) +
#         facet_wrap(~gradeLevel)+
#         labs(color = ' ') +
#         scale_color_manual(values = c('black', '#3688C8')) +
#         scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                            labels = c(' ',  '10th', '20th', '30th',
#                                       '40th', '50th', '60th', '70th',
#                                       '80th', '90th', '99th')) + 
#         theme_minimal() +
#         baseGrowthTheme 
#       
#       plot6to8 <-  ggplot(data = grade6to8, 
#                           mapping = aes(x = sgpBaseline, 
#                                         y = binN)) +
#         geom_col(color = '#71797E', 
#                  fill = '#71797E', 
#                  width = 8) +
#         geom_vline(aes(xintercept = grade6to8$medianSgpBaseline*100, 
#                        color = paste0('District: ',factor(ordinal(round(grade6to8$medianSgpBaseline*100))))
#         ), 
#         size = 2, 
#         show.legend = TRUE) +
#         scale_color_manual(values = c('black', '#3688C8')) +
#         geom_vline(aes(xintercept = cmasGrowthBaselineState8$medianSgpBaseline, 
#                        color = paste0('State: ', ordinal(cmasGrowthBaselineState8$medianSgpBaseline))), 
#                    size = 2, 
#                    show.legend = TRUE) +
#         facet_wrap(~gradeLevel)+
#         labs(color = ' ') +
#         scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                            labels = c(' ',  '10th', '20th', '30th',
#                                       '40th', '50th', '60th', '70th',
#                                       '80th', '90th', '99th')) + 
#         theme_minimal() +
#         baseGrowthTheme 
#       
#       plot_grid(plot4to6, plot6to8) 
#     } else {  
#       
#     }
#   } else {
#     if ((schoolNumber %in% k6School  &  schoolNumber != '5972')| schoolNumber == 7701){ #K6 schools or collegiate Academy
#       plot_grid(plot4to6NoState)
#     } else if (nrow(grade4to6) < 2 & nrow(cmasGrowthBaseline) > 0 & schoolNumber != '5036' & schoolNumber != '5892' & schoolNumber != '4798'){ #jeffco open, Jefferson Academy
#       plot_grid(plot6to8NoState) 
#     } else if (nrow(grade4to6) < 2) {
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Growth data for grade levels at the selected school\nare not available', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else if ((nrow(grade6to8) == 0 | nrow(naChecker6to8)  > 0) & (schoolNumber %in% middleSchools | schoolNumber %in% k6School | schoolNumber %in% multiLevel| schoolNumber == '5892'| schoolNumber %in% k8Schools | schoolNumber %in% k5Schools)) {
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Insufficient growth data\ndue to small number of students testing', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else if (schoolNumber %in% k5Schools){
#       plot_grid(plot4to6NoState) 
#     } else if (is.na(grade4to6$medianSgpBaseline)){
#       ggplot() +
#         annotate('text', x = 0, y = 0, 
#                  label = 'Insufficient growth data\ndue to small number of students testing', 
#                  size = 6, 
#                  color = 'grey') +
#         theme_minimal() +
#         baseGrowthTheme +
#         theme(axis.text.x = element_blank(), 
#               axis.text.y = element_blank())
#     } else if (schoolNumber %in% k8Schools | schoolNumber %in% middleSchools){
#       plot_grid(plot4to6NoState, plot6to8NoState) 
#     } else if (schoolNumber == 9998) {
#       plot4to6 <-  ggplot(data = grade4to6, 
#                           mapping = aes(x = sgpBaseline, 
#                                         y = binN)) +
#         geom_col(data = grade4to6, 
#                  mapping = aes(x = sgpBaseline, 
#                                y = binN),
#                  color = '#71797E', 
#                  fill = '#71797E', 
#                  width = 8) +
#         geom_vline(aes(xintercept = grade4to6$medianSgpBaseline*100, 
#                        color = paste0('District: ',factor(ordinal(round(grade4to6$medianSgpBaseline*100))))), 
#                    size = 2, 
#                    show.legend = TRUE) +
#         facet_wrap(~gradeLevel)+
#         labs(color = ' ') +
#         scale_color_manual(values = c('black', '#3688C8')) +
#         scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                            labels = c(' ',  '10th', '20th', '30th',
#                                       '40th', '50th', '60th', '70th',
#                                       '80th', '90th', '99th')) + 
#         theme_minimal() +
#         baseGrowthTheme 
#       
#       plot6to8 <-  ggplot(data = grade6to8, 
#                           mapping = aes(x = sgpBaseline, 
#                                         y = binN)) +
#         geom_col(color = '#71797E', 
#                  fill = '#71797E', 
#                  width = 8) +
#         geom_vline(aes(xintercept = grade6to8$medianSgpBaseline*100, 
#                        color = paste0('District: ',factor(ordinal(round(grade6to8$medianSgpBaseline*100))))
#         ), 
#         size = 2, 
#         show.legend = TRUE) +
#         scale_color_manual(values = c('black', '#3688C8')) +
#         facet_wrap(~gradeLevel)+
#         labs(color = ' ') +
#         scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
#                            labels = c(' ',  '10th', '20th', '30th',
#                                       '40th', '50th', '60th', '70th',
#                                       '80th', '90th', '99th')) + 
#         theme_minimal() +
#         baseGrowthTheme 
#       
#       plot_grid(plot4to6NoState, plot6to8NoState) 
#     } else { #for collegiate Academy - Need a real condition
#       plot_grid(plot4to6NoState) 
#     }
#   }
#   
# })