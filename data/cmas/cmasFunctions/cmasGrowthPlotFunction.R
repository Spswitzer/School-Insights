## CMAS 2019 Growth Reading ----
## CMAS 2019 Growth Math ----
CMASGrowthPlot <- function(df = CMASvar, subject = 'GRO_MEDIAN_SGP'){

  subjectSym <- sym(subject)

  CMASvar <- df %>% 
    mutate(EMH_CODE = recode(EMH_CODE, Middle = 'Middle School'))
  
  p <-  ggplot(data = CMASvar) +
    geom_bar(data = CMASHundredMGP, 
             mapping = aes(x = group, fill = percent),
             position="fill",
             width = 0.15) +
    geom_point_interactive(data = CMASvar, 
                           mapping = aes(y = PctPts, 
                                                         tooltip = paste0('<b>School Level: </b>', 
                                                                          '<br>', EMH_CODE, '<br>',
                                                                          '<b>Growth Percentile: </b>', '<br>',
                                                                          !!subjectSym, '<br>',
                                                                          '<b>Number of Students: </b><br>', GRO_N_VALID
                                                                          )), 
                           x = 1,  
                           size = 40, 
                           alpha = 0.5,
                           shape = 17)+
    geom_text(aes(y = PctPts, label = ordinal(!!subjectSym)), 
              x = 1, 
              vjust = -1, 
              size = 35)+
    scale_fill_manual(values = c('#3688c8', '#22a783', '#e2a331', '#d8274a'))+
    facet_wrap(~EMH_CODE, 
               ncol = 1)+
    coord_flip()+
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
          strip.text = element_text(size = 110))
  
  ggiraph(code = print(p), 
          tooltip_opacity = 0.9, 
          tooltip_offx = 50, 
          tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
          hover_css = "cursor:pointer;stroke:#EA9563;",
          width = 1,
          width_svg = 30,
          height_svg = 12
  )
}
