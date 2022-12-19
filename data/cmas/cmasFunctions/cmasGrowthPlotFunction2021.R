#### 2021 Cohort Growth ####
  CMASvar <- CMASvarA %>% 
    mutate(gradeLevel = recode(gradeLevel,
                               '05' = 'Growth From\nGrade 3 to Grade 5', 
                               '06' = 'Growth From\nGrade 4 to Grade 6', 
                               '07' = 'Growth From\nGrade 5 to Grade 7', 
                               '08' = 'Growth From\nGrade 6 to Grade 8')) %>% 
    mutate(medianSgpCohort = as.numeric(medianSgpCohort)) %>% 
    mutate(medianSgpCohort = medianSgpCohort /100) %>% 
    group_by(schNumber, subject, gradeLevel) %>% 
    summarise(medianSgpCohort = first(medianSgpCohort)) %>% 
    filter(schNumber == schoolNumber) %>% 
    filter(subject == 'ENGLISH LANGUAGE ARTS')
  
 ggplot(data = CMASvar) +
    geom_bar(data = CMASHundredMGP, 
             mapping = aes(x = group, fill = percent),
             position="fill",
             width = 0.15) +
    geom_point(data = CMASvar, 
                           mapping = aes(y = medianSgpCohort), 
                           x = 1,  
                           size = 14, 
                           alpha = 0.5,
                           shape = 18) +
    geom_text(aes(y = medianSgpCohort, label = ordinal(medianSgpCohort*100)), 
              x = 1, 
              vjust = -1, 
              size = 8) +
    scale_fill_manual(values = c('#3688c8', '#22a783', '#e2a331', '#d8274a')) +
    coord_flip()+
    facet_wrap(~gradeLevel, 
               ncol = 1) +
    ylim(0, 1)+
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          plot.title = element_text(size = 10, color = 'grey'),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(), 
          panel.background = element_rect(fill = '#EBF1F6', 
                                          colour = '#EBF1F6'),
          plot.background = element_rect(fill = '#EBF1F6', 
                                         colour = '#EBF1F6'),
          panel.spacing.y=unit(.2, "lines"), 
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          legend.position = 'none', 
          strip.text = element_text(size = 14))


 #### Baseline Growth ####
 
 grade3to5 <- CMASvarA %>% 
   mutate(gradeLevel = recode(gradeLevel,
                              '05' = 'Growth From\nGrade 3 to Grade 5', 
                              '06' = 'Growth From\nGrade 4 to Grade 6', 
                              '07' = 'Growth From\nGrade 5 to Grade 7', 
                              '08' = 'Growth From\nGrade 6 to Grade 8')) %>% 
   mutate(medianSgpBaseline = as.numeric(medianSgpBaseline)) %>% 
   mutate(medianSgpBaseline = medianSgpBaseline /100) %>% 
   group_by(schNumber, subject, gradeLevel, binN, sgpBaseline) %>% 
   summarise(medianSgpBaseline = first(medianSgpBaseline)) %>% 
   filter(schNumber == schoolNumber) %>% 
   filter(subject == 'ENGLISH LANGUAGE ARTS') %>% 
   filter(gradeLevel == 'Growth From\nGrade 3 to Grade 5')
 
 
 plot3to5 <-  ggplot(data = grade3to5, 
                     mapping = aes(x = sgpBaseline, 
                                   y = binN)) +
   geom_col(data = grade3to5, 
            mapping = aes(x = sgpBaseline, 
                          y = binN),
            color = 'grey', 
            fill = 'grey', 
            width = 8) +
   geom_vline(aes(xintercept = grade3to5$medianSgpBaseline*100, 
                  color = paste0('School: ', 
                                 factor(ordinal(round(grade3to5$medianSgpBaseline*100))))
                  ), 
              size = 2, 
              show.legend = TRUE) +
   geom_vline(aes(xintercept = 46, 
                  color = 'State: 46th'), 
              size = 2, 
              show.legend = TRUE) +
   labs(color = 'Median Growth\nPercentile') +
   scale_color_manual(values = c('black', 'blue')) +
   scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                      labels = c(' ',  '10th', '20th', '30th',
                                 '40th', '50th', '60th', '70th',
                                 '80th', '90th', '99th')) + 
   theme_minimal() +
   theme(axis.title = element_blank(), 
         axis.text.y = element_blank(), 
         axis.text.x = element_text(size = 8),
         panel.background = element_rect(fill = 'white',
                                         colour = 'white'),
         panel.grid = element_blank(),
         panel.spacing.y=unit(.2, "lines"),
         legend.position = 'left',
         legend.direction = 'vertical', 
         legend.key.size =  unit(0.5, 'cm'))

grade5to7 <- CMASvarA %>% 
  mutate(gradeLevel = recode(gradeLevel,
                             '05' = 'Growth From\nGrade 3 to Grade 5', 
                             '06' = 'Growth From\nGrade 4 to Grade 6', 
                             '07' = 'Growth From\nGrade 5 to Grade 7', 
                             '08' = 'Growth From\nGrade 6 to Grade 8')) %>% 
  mutate(medianSgpBaseline = as.numeric(medianSgpBaseline)) %>% 
  mutate(medianSgpBaseline = medianSgpBaseline /100) %>% 
  group_by(schNumber, subject, gradeLevel, binN, sgpBaseline) %>% 
  summarise(medianSgpBaseline = first(medianSgpBaseline)) %>% 
  filter(schNumber == schoolNumber) %>% 
  filter(subject == 'ENGLISH LANGUAGE ARTS') %>% 
  filter(gradeLevel == 'Growth From\nGrade 5 to Grade 7')


plot5to7 <-  ggplot(data = grade5to7, 
                    mapping = aes(x = sgpBaseline, 
                                  y = binN)) +
  geom_col(color = 'grey', 
           fill = 'grey', 
           width = 8) +
  geom_vline(aes(xintercept = grade5to7$medianSgpBaseline*100, 
                 color = paste0('School: ', 
                                factor(ordinal(round(grade5to7$medianSgpBaseline*100))))
  ), 
  size = 2, 
  show.legend = TRUE) +
  geom_vline(aes(xintercept = 40, 
                 color = 'State: 40th'), 
             size = 2, 
             show.legend = TRUE) +
  labs(color = 'Median Growth\nPercentile') +
  scale_color_manual(values = c('black', 'blue')) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     labels = c(' ',  '10th', '20th', '30th',
                                '40th', '50th', '60th', '70th',
                                '80th', '90th', '99th')) + 
  theme_minimal() +
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 8),
        panel.background = element_rect(fill = 'white',
                                        colour = 'white'),
        panel.grid = element_blank(),
        panel.spacing.y=unit(.2, "lines"),
        legend.position = 'left',
        legend.direction = 'vertical', 
        legend.key.size =  unit(0.5, 'cm'))

plot_grid(plot3to5, plot5to7, 
          labels = c('Growth From Grade 3 to Grade 5', 'Growth From Grade 5 to Grade 7'), 
          label_size = 12, 
          label_fontface = 'plain',
          hjust = 0,
          vjust = 1.1,
          label_x = 0.01) 
