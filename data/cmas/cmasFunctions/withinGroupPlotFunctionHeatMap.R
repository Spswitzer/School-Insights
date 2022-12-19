#############This function will display Within Group Subgroup data for Statewide assessments Reading & Math#########
## Within group Heat Map Reading ----
## Within group Heat Map Math ----
withinGroupPlotHeat <- function(schoolCode, subject = "English Language Arts"){
  # schoolCode <-  '0776'
  # subject  <-  "English Language Arts"

   m <- withinGroupDataForPlots %>% 
     filter(schoolNumber == schoolCode) %>% 
     select(-subject) %>% 
     filter(subjectArea == subject)
     
   full_df <- expand.grid(demographicCategory = c('FRL', 'Students\nof Color\nor\nHispanic', 'ELL', 'IEP', 'GT'),  
                          subindicator = unique(m$subindicator)
   ) %>%
     mutate(demographicCategory = factor(demographicCategory,
                                         ordered = T)
     )
   
l <- m %>% 
  full_join(full_df, by = c('demographicCategory', 'subindicator')) %>% 
  filter(!is.na(gradeLevel)) %>% 
  mutate(gradeLevel = factor(gradeLevel, 
                             levels = c('Elementary', 'Middle School', 'PSAT 9 & 10', 'Grade 11'), 
                             labels = c('Elementary', 'Middle School', 'PSAT 9 & 10', 'SAT'), 
                             ordered = T)) %>% 
  mutate(demographicCategory = factor(demographicCategory, 
                                      levels = c('FRL', 'Students\nof Color\nor\nHispanic', 'ELL', 'IEP', 'GT'), 
                                      ordered = T))

nasPresent <- l %>% 
  filter(is.na(percentileInGroup))

if (nrow(nasPresent) == 0){
  ggplot()+
    geom_tile(data = l,
              mapping = aes(x = demographicCategory,
                            y = 50,
                            fill = percentileInGroup), 
              color = 'lightgrey',
              width = 0.95)+
    geom_label(data = l,
              mapping = aes(x = demographicCategory,
                            y = 50,
                            label = scales::ordinal(percentileInGroup)),
              color = '#515151',
              fill = 'white',
              size = 6,
              alpha = 0.5,
              label.size = 0)+
    scale_fill_gradientn(colors = c('#9E3D22','#BF4E23','#DE6226', 
                                    '#EB8129', '#F0AB4D',
                                    '#D9D5C9', #0 point on scale
                                    '#95C6E2', '#82A6D1', '#6C97C9', '#5687C0',
                                    '#315683'
    ), 
    limits=c(1, 99), 
    space = "Lab", 
    breaks = waiver(), 
    na.value = "white",
    minor_breaks = 5)+
    labs(fill='Percentile in Subgroup') +
    facet_grid(.~gradeLevel) + #subindicator
    theme_minimal()+
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 14), 
          panel.border = element_rect(fill = NA, 
                                      color = '#D9D5C9'),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = 'bottom' ,
          legend.text = element_text(size = 16, color = '#515151'), 
          legend.title.align = 1, 
          legend.title = element_text(size = 14), 
          legend.key.height = unit(1.5,"mm"), 
          legend.key.size= unit(10,"mm"),
          strip.text = element_text(size = 16), 
          strip.background = element_rect(fill = 'lightgrey', color = 'darkgrey')
         )+
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
           size = guide_legend(title.position="top", title.hjust = 0.5))
} else {
  ggplot()+
    geom_tile(data = l,
              mapping = aes(x = demographicCategory,
                            y = 50,
                            fill = percentileInGroup), 
              color = 'lightgrey',
              width = 0.95)+
    geom_label(data = l,
               mapping = aes(x = demographicCategory,
                             y = 50,
                             label = scales::ordinal(percentileInGroup)), 
               color = '#515151', 
               fill = 'white',
               size = 6, 
               alpha = 0.5, 
               label.size = 0)+
    geom_label(data = filter(l, is.na(percentileInGroup)),
               mapping = aes(x = demographicCategory,
                             y = 50, label = 'Limited\ndata'),
               color = '#515151',
               fill = 'white',
               size = 4,
               alpha = 0.5,
               label.size = 0)+
    labs(caption = 'Limited data means this school has less than 16 students in the group so percentile not reported for student data privacy.')+
    scale_fill_gradientn(colors = c('#9E3D22','#BF4E23','#DE6226', 
                                    '#EB8129', '#F0AB4D',
                                    '#D9D5C9', #0 point on scale
                                    '#95C6E2', '#7FB4D8', '#6A9FCA', '#5A8EBC',
                                    '#4A7CAB'
    ), 
    limits=c(1, 99), 
    space = "Lab", 
    breaks = waiver(), 
    na.value = "white",
    minor_breaks = 5)+
    labs(fill='Percentile in Subgroup') +
    facet_grid(.~gradeLevel) + #subindicator
    theme_minimal()+
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 14), 
          # panel.border = element_rect(fill = NA, 
          #                             color = '#D9D5C9'),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = 'bottom' ,
          plot.caption = element_text(size = 12, color = '#515151'),
          legend.text = element_text(size = 16, color = '#515151'), 
          legend.title.align = 1, 
          legend.title = element_text(size = 14), 
          legend.key.height = unit(1.5,"mm"), 
          legend.key.size= unit(10,"mm"),
          strip.text = element_text(size = 16), 
          strip.background = element_rect(fill = 'lightgrey', color = 'darkgrey')
    )+
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
           size = guide_legend(title.position="top", title.hjust = 0.5))
}

}

noWithinGroupPlot <- function() {
  m <- withinGroupDataForPlots %>% 
    filter(schoolNumber == '0030', 
           subjectArea == 'Math')
  
  # m$subindicator <- fct_rev(m$subindicator)
  
  # broncos <- m %>%
  #   select(testType, demographicCategory, percentileInGroup) 
  
  ggplot()+
    geom_label(data = m,
               mapping = aes(x = 3,
                             y = 50,
                             label = 'This site has \nno data for\nthis measure'), 
               color = '#515151', 
               fill = 'white',
               size = 8, 
               alpha = 0.5, 
               label.size = 0)+
    theme_minimal()+
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(),
          axis.text.x = element_blank(), 
          panel.grid = element_blank(),
          legend.position = 'bottom' ,
          legend.text = element_text(color = '#515151'), 
          legend.title.align = 1, 
          legend.title = element_text(size = 10), 
          legend.key.height = unit(1.5,"mm"), 
          legend.key.size= unit(8,"mm"),
          strip.text = element_text(size = 16), 
          strip.background = element_rect(fill = 'lightgrey', color = 'darkgrey')
    )+
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5),
           size = guide_legend(title.position="top", title.hjust = 0.5))
}



