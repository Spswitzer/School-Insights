## CMAS 2019 ACH Percentile Reading ----
## CMAS 2019 ACH Percentile Math ----
CMASAchPlotFunction <- function(){
  
  CMASvar <- CMASvar %>% 
    mutate(EMH_CODE = recode(EMH_CODE, Middle = 'Middle School'))

p <-  ggplot(data = CMASvar) +
  geom_bar(data= CMASHundred, aes(x = group, fill = percent),
           position="fill",
           width = 0.15) +
  geom_point_interactive(data = CMASvar, mapping = aes(y = PctPts, 
                                                       tooltip = paste0('<b>School Level: </b>', '<br>', EMH_CODE,
                                                                        '<br>',
                                                                        '<b>Mean Scale Score: </b>', '<br>', 
                                                                        ACH_MEAN_SS, '<br>', 
                                                                        '<b>Number of Students: </b>', '<br>', 
                                                                        ACH_N_VALID)), 
                         x = 1,  
                         size = 40, 
                         alpha = 0.5,
                         shape = 17)+
  geom_text(aes(y = PctPts, label = ordinal(ACH_PERCENTILE)), 
            x = 1, 
            vjust = -1, 
            size = 35)+
  scale_fill_manual(values = c('#3688c8', '#22a783', '#e2a331', '#d8274a'))+
  facet_wrap(~EMH_CODE,                     ncol = 1)+
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
        height_svg = 12)

}

noCMASfunction <- function(){
  p <-  ggplot() +
    geom_text(aes(y = 0.35, 
                  x = 1.2, 
                  label = 'See PSAT tab for High Schools'),
              size = 25)+
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
          width_svg = 20, 
          height_svg = 6)
}

