## PSAT 2019 Ach Percentile EBRW ----
## PSAT 2019 Ach Percentile Math ----
SATAchPlotFunction <- function(){
  p <-    ggplot() +
    geom_bar(data= CMASHundred, aes(x = group, fill = percent),
             position="fill",
             width = 0.15) +
    geom_point_interactive(data = SATvar, mapping = aes(y = PctPts, 
                                                        tooltip = paste0('<b>Mean scale score: </b>', 
                                                                         ACH_MEAN_SS)), 
                           x = 1,  
                           size = 15, 
                           alpha = 0.5,
                           shape = 17)+
    geom_text(aes(y = SATvar$PctPts, label = ordinal(SATvar$ACH_PERCENTILE)), 
              x = 1, 
              vjust = -2, 
              size = 15)+
    scale_fill_manual(values = c('#3688c8', '#22a783', '#e2a331', '#d8274a'))+
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
          legend.position = 'none')
  
  ggiraph(code = print(p), 
          tooltip_opacity = 0.9, 
          tooltip_offx = 50, 
          tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
          hover_css = "cursor:pointer;stroke:#EA9563;",
          width = 1, 
          width_svg = 12, 
          height_svg = 3)
}



noSatHereFunction <- function(){
  p <-  ggplot() +
    geom_text(aes(y = 0.35, 
                  x = 1.2, 
                  label = ' '),
              size = 15)+
    scale_fill_manual(values = c('#3688c8', '#22a783', '#e2a331', '#d8274a'))+
    coord_flip()+
    ylim(0, 1)+
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          plot.title = element_text(size = 10, color = 'grey'),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(), 
          panel.background = element_rect(fill = '#ffffff', 
                                          colour = '#ffffff'),
          plot.background = element_rect(fill = '#ffffff', 
                                         colour = '#ffffff'),
          panel.spacing.y=unit(.2, "lines"), 
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          legend.position = 'none')
  
  ggiraph(code = print(p), 
          tooltip_opacity = 0.3, 
          tooltip_offx = 50, 
          hover_css = "cursor:pointer;stroke:#EA9563;",
          width = 1, 
          width_svg = 12, 
          height_svg = 3)
}