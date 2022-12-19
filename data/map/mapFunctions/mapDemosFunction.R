#MAP Demos Plots ----



#' Plot NWEA MAP Results with Student Demographics
#'
#' @param .df 
#' @param .school 
#' @param .grade 
#' @param .contentName 
#'
#' @return ggiraph plot
#' @examples .df <- mapDemoData

mapDemosPlotFunction <- function(.df = mapDemoDataFiltered()$mapDemoData, 
                                 .contentName = 'READING') {
  
  #Filler for schools with missing student groups, due to low N in group
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
      'all'
    )
  )
  
  #set levels and labels of subcategory
  subcategoryLookup <- data.frame(
    levels = c('all', 
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
  
  mapDemoPlot <- .df %>% 
    filter(profFlag == 1, 
           profNumeric == 4, #Most schools have level 4 students, not all have level 5 students
           contentName == .contentName) %>% 
    filter(subcategoryN > 15) %>% 
    full_join(groupLookup, by = 'subcategory') %>%
    ungroup() %>% 
    select(category = category.y, subcategory, pctMeetExceed, 
           profN, contentName,  subcategoryN, testingPeriodName) %>% #gradeLevel,
    mutate(pctMeetExceedLabel = paste0(round(pctMeetExceed*100), '%')) %>% 
    mutate(pctMeetExceed = replace_na(pctMeetExceed, 0.001), # to handle color placement
           profN = replace_na(profN, 0)) %>% 
    mutate(pctMeetExceedLabel = case_when(
      pctMeetExceedLabel == 'NA%' ~ 'N <16 Students', 
      TRUE ~ pctMeetExceedLabel)) %>% 
    mutate(subcategory = factor(subcategory, 
                                levels = subcategoryLookup$levels, 
                                labels = subcategoryLookup$labels, 
                                ordered = T)) %>%  
    mutate(category = factor(category, 
                             levels = categoryLookup$levels, 
                             labels = categoryLookup$labels, 
                             ordered = T)) 
  
  themeCode <-      
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          plot.title = element_text(size = 14, 
                                    # face = 'bold',
                                    color = '#315683', 
                                    hjust = -0.4),
          axis.text.x = element_blank(), 
          axis.text.y.left = element_text(size  = 18, #_interactive
                                                      hjust = NULL#,
                                                      # data_id = 'axisYText'#,
                                               #        tooltip = 'Student Groups include:
                                               # All Students,
                                               # English Language Learners,
                                               # Free or Reduced Lunch Program,
                                               # Gifted and Talented Program,
                                               # Individualized Education Program,
                                               # Students of Color or Hispanic',# Use of lookup Table values not working in tooltip
                                               # hover_css = "fill:#22a783;stroke:none;"
                                          ),
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.text = element_text(size = 24, #_interactive
                                                # data_id = 'stripText', 
                                               #  tooltip = 'Student Groups include: 
                                               # All Students,
                                               # English Language Learners, 
                                               # Free or Reduced Lunch Program, 
                                               # Gifted and Talented Program, 
                                               # Individualized Education Program, 
                                               # Students of Color or Hispanic', # Use of lookup Table values not working in tooltip
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

  p <-  ggplot(data = mapDemoPlot, 
               mapping = aes(x = fct_rev(subcategory), 
                             y = pctMeetExceed*100, 
                             fill = subcategory)) +
    geom_col_interactive( #_interactive
      aes(tooltip =
                               paste0('<b>Total in Met or Exceed level: </b>',
                                      scales::comma(profN, accuracy = 1),
                                      '<br>', '<b>Group: </b>', subcategory,
                                      '<br><b>Total students in group: </b>', scales::comma(subcategoryN, accuracy = 1)))
      ) +
    scale_fill_manual(values = c('#315683', 
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
    ylim(0, 110) +
    labs(title = paste0('Assessment Season:  ', unique(mapDemoPlot$testingPeriodName))) +
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
