#This function is used to generate the following plots: MAP: Growth rating, Reading & Math
## Reading Growth Status Plots ----
## Math Growth Status Plots ----

mapGrowthPlot <- function(df = mapGrowthDf(), schoolCode, subject = "READING", q = 'Fall'){
  #df = growth
  # subject <- "READING"
  # schoolCode <- '0030'
  
  
  mapPlot <- df %>% 
    filter(contentName == subject, 
           testingPeriodName == q#, 
           # gradeId != '1st\nGrade'& gradeId != '2nd\nGrade'
           ) %>% # Not very representative in Fall 2022
    mutate(gradeId = factor(subcategory, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8 ,9, 10), 
                            labels = c('1st Grade', '2nd Grade',
                                       '3rd Grade', '4th Grade', '5th Grade', 
                                       '6th Grade' ,'7th Grade','8th Grade', 
                                       '9th Grade','10th Grade')))

  
  interactiveElement <- df %>% 
    filter(cdeSchoolNumber == schoolCode, 
           contentName == subject) %>% 
    mutate(suppress = case_when(
      subcategoryN < 15 ~ 1, 
      TRUE ~ 0 )) %>% 
    ungroup() %>% 
    distinct(suppress) %>% 
    pull()
  
  mapGrow <- ggplot(data = mapPlot, mapping = aes(x = MGP,
                                                  y = contentName))+
    xlim(0, 100)+
    facet_wrap(~gradeId,
               ncol = 1)+
    labs(title = 'National Average 50th percentile',
         # subtitle = 'Growth information for Grades 1 and 2 will be available in the Winter benchmark',
         caption = 'Fall to Fall (2021-2022) comparision')

if (interactiveElement == 0 & nrow(mapPlot)>5) {
mapGrow <- mapGrow + 
  geom_label_interactive(aes(label = ordinal(MGP)),
                         size = 10,
                         color = '#ffffff', 
                         fill = '#315683') +
  theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          title = element_text(size = 16, color = '#515151'),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_rect(fill = '#EBF1F6',
                                          colour = '#EBF1F6'),
          plot.background = element_rect(fill = '#EBF1F6',
                                         colour = '#EBF1F6'),
          plot.caption = element_text(size = 20),
          plot.subtitle = element_text(size = 20),
          panel.spacing.y=unit(.2, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = "#bcc0c4", size = 2),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 25, hjust = 0),
          legend.position = 'none')
  
    ggiraph(code = print(mapGrow),
            tooltip_opacity = 0.7,
            tooltip_offx = 50,
            tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
            hover_css = "cursor:pointer;stroke:#EA9563;",
            width = 1,
            width_svg = 10,
            height_svg = 9
            )
} else if (interactiveElement == 0 & nrow(mapPlot) < 5) {
  
mapGrow <-   mapGrow +
  geom_label_interactive(aes(label = ordinal(MGP)),
                         size = 10,
                         color = '#ffffff', 
                         fill = '#315683') +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          title = element_text(size = 16, color = '#515151'),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.background = element_rect(fill = '#EBF1F6',
                                          colour = '#EBF1F6'),
          plot.background = element_rect(fill = '#EBF1F6',
                                         colour = '#EBF1F6'),
          plot.caption = element_text(size = 20),
          plot.subtitle = element_text(size = 20),
          panel.spacing.y=unit(.2, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = "#bcc0c4", size = 2),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 20, hjust = 0),
          legend.position = 'none')
  
  ggiraph(code = print(mapGrow),
          tooltip_opacity = 0.7,
          tooltip_offx = 50,
          tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
          hover_css = "cursor:pointer;stroke:#EA9563;",
          width = 1,
          width_svg = 10,
          height_svg = 5

  )

  } else {

    p <- ggplot()+
      annotate('text', x = 2017, y = 50,
               label = 'Results for this\nassessment are not reported \ndue to small testing population.', size = 20,
               color = 'darkgrey')+
      ylim(c(0,100))+
      scale_fill_manual(values = c("lightblue", "lightgrey"))+
      theme_minimal()+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            legend.position = 'bottom',
            legend.title = element_blank(),
            legend.key.size = unit(2, "mm"),
            legend.spacing.x = unit(1.0, 'mm'),
            legend.background = element_rect(fill = "#EBF1F6", color = "#515151"),
            panel.background = element_rect(fill = '#ffffff',
                                            colour = '#ffffff'),
            plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt"),
            plot.background = element_rect(fill = '#ffffff',
                                           colour = '#ffffff'),
            plot.title = element_text(size = 12, color = 'grey'),
            plot.subtitle = element_text(size = 12, color = 'grey'),
            panel.spacing.y=unit(.2, "lines"),
            panel.grid.major = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid = element_blank())

    ggiraph(print(p),
            width_svg = 15,
            height_svg = 5)
 }
}

