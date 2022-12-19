

#' Plot Student Demographic Performance on Statewide Assessments
#'
#' @param .school, .grade, .contentName
#' @return plot
#' @examples .school <- '0030' .grade <- 'Gr3' .contentName <- 'MATH' .df <- cmasElaGroups

statewideAssessmentsDemos <- function(.df = cmas, 
                                      # .school, 
                                      # .grade, 
                                      .contentName) {
## Filler for schools with low N in subcategory
  groupLookup <- data.frame(
        category = c(
           rep('iepBin', 2),
           rep('ellBin', 2),
            rep('frlBin', 2),
           rep('raceBin', 2),
             rep('gtBin', 2),
           'all'
          ),
        subcategory = c(
            'IEP',
            'No IEP'  ,
            'English Language Learner',
             'Not ELL'   ,
             'Free or Reduced Lunch Eligible' ,
             'Not Free or Reduced Lunch' ,
             'Ethnic/Racial Minority'  ,
            'Not Ethnic/Racial Minority',
             'GT',
            'Not GT',
             '1'
         )
     )
    
  #set levels and labels of subcategory
  subcategoryLookup <- data.frame(
    levels = c('1', 
               'Free or Reduced Lunch Eligible', 
               'Not Free or Reduced Lunch', 
               'English Language Learner', 
               'Not ELL', 
               'IEP', 
               'No IEP', 
               'Ethnic/Racial Minority', 
               'Not Ethnic/Racial Minority', 
               'GT', 
               'Not GT'), 
    labels = c('All Students - All Grades Combined', 
               'Free or Reduced Lunch Eligible', 
               'Not Free or Reduced Lunch Eligible',
               'English Language Learner', 
               'Not in English Language Learner Program',
               'Individualized Education Program',
               'Not in Individualized Education Program',
               'Students of Color or Hispanic',
               'White Students',
               'Gifted and Talented Program',
               'Not in Gifted and Talented Program'
    )
  )
  
  #set levels and labels of category
  categoryLookup <- data.frame(
    labels = c('All Students', 
               'Free or Reduced Lunch Program Students', 
               'Students in English Language Learners Program',
               'Students with Individualized Education Program', 
               'Students of Color or Hispanic', 
               'Students in Gifted and Talented Program'),  
    levels = c('all', 
               'frlBin',
               'ellBin', 
               'iepBin', 
               'raceBin', 
               'gtBin')
  )
    cmasGroup <- .df %>% 
      filter(
        # cdeSchoolNumber == .school, 
             # grade == .grade, 
             profFlag == 1, 
             profDescription == 'Met'|profDescription == 'Meet or Exceed Benchmark', 
             contentName == .contentName) %>% 
      filter(subcategoryN > 15) %>% 
      full_join(groupLookup, by = 'subcategory') %>%
      ungroup() %>% 
      select(category = category.y, subcategory, pctMeetExceedCategory, 
             profNCategory, contentName, noGradesubcategoryN) %>% 
      mutate(pctMeetExceedLabel = paste0(round(pctMeetExceedCategory*100), '%')) %>% 
      mutate(pctMeetExceedCategory = replace_na(pctMeetExceedCategory, 0.001), # to handle color placement
             profNCategory = replace_na(profNCategory, 0)) %>% 
      mutate(pctMeetExceedLabel = case_when(
        pctMeetExceedLabel == 'NA%' ~ 'N<16 Students', 
        TRUE ~ pctMeetExceedLabel)) %>% 
      mutate(subcategory = factor(subcategory, 
                                  levels = subcategoryLookup$levels, 
                                  labels = subcategoryLookup$labels,
                                  ordered = T)) %>%  
      mutate(category = factor(category, 
                               levels = categoryLookup$levels, 
                               labels = categoryLookup$labels, 
                               ordered = T)) %>% 
      group_by(category,subcategory, contentName) %>% 
      summarise(pctMeetExceedCategory = first(pctMeetExceedCategory), 
                profNCategory = first(profNCategory), 
                noGradesubcategoryN = first(noGradesubcategoryN),
                pctMeetExceedLabel = first(pctMeetExceedLabel))
    
    themeCode <-      
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(), 
            plot.title = element_text(size = 24, 
                                      color = '#315683', 
                                      hjust = 0),
            plot.subtitle = element_text(size = 24, 
                                         color = 'black', 
                                         hjust = 0),
            axis.text.x = element_blank(), 
            axis.text.y.left = element_text(size  = 16, #_interactive
                                                        hjust = 1#,
                                               #          data_id = 'axisYText',
                                               #          tooltip = 'Student Groups include:
                                               # All Students,
                                               # English Language Learners,
                                               # Free or Reduced Lunch Program,
                                               # Gifted and Talented Program,
                                               # Individualized Education Program,
                                               # Students of Color or Hispanic',
                                               # hover_css = "fill:#22a783;stroke:none;"
            ),
            axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank(), 
            strip.text = element_text(size = 18#, #_interactive
                                               #    data_id = 'stripText', 
                                               #    tooltip = 'Student Groups include: 
                                               # All Students,
                                               # English Language Learners, 
                                               # Free or Reduced Lunch Program, 
                                               # Gifted and Talented Program, 
                                               # Individualized Education Program, 
                                               # Students of Color or Hispanic', 
                                               # hjust = 0, 
                                               # face ='bold',
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
    
    
    
    p <-  ggplot(data = cmasGroup, 
                 mapping = aes(x = fct_rev(subcategory), 
                               y = pctMeetExceedCategory*100, 
                               fill = subcategory)) +
      geom_col_interactive(#_interactive
        aes(tooltip =
                                 paste0('<b>Total in Met or Exceed level: </b>',
                                        scales::comma(profNCategory, accuracy = 1),
                                        '<br><b>Group: </b>', subcategory,
                                        '<br><b>Total in group: </b>',  scales::comma(noGradesubcategoryN, accuracy = 1)
                                       ))
                           ) +
      scale_fill_manual(values = c('#315683', #315683
                                   '#315683', 'grey',
                                   '#315683', 'grey',
                                   '#315683', 'grey',
                                   '#315683', 'grey',
                                   '#315683', 'grey') )+
      geom_text(aes(label = pctMeetExceedLabel), 
                hjust = -0.1, 
                size = 8) +
      facet_wrap(~category, 
                 scales = 'free', 
                 ncol = 1) +
      coord_flip(expand = F) +
      ylim(0, 115) +
      labs(title = str_to_title(cmasGroup$contentName)) +
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
}