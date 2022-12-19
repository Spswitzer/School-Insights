#MAP Alluvial Plot ----

mapCohortAlluvialPlotBin <- function(.df = mapAlluvialBinaryLevelsAll, .content =  'MATH', .school = SchoolNumber) {
 
   plotData <- .df%>% 
    filter(contentGroupName == .content) %>% 
    filter(CDESchoolNumber == .school)

  trendTheme <- theme_minimal()+
    theme(axis.text.y = element_blank(), 
          axis.title.y = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          legend.position = 'top',
          legend.text = element_text_interactive(size = 12, 
                                                 data_id = 'legend.text', 
                                                 tooltip = 'At or Above | Below or Well Below', 
                                                 hover_css = "fill:#22a783;stroke:none;"),
          legend.title = element_text_interactive(size = 14, 
                                                  data_id = 'legend.title', 
                                                  tooltip = 'Performance Level',
                                                  hover_css = "fill:#22a783;stroke:none;"),
          axis.text.x = element_text_interactive(size = 14,   
                                                 data_id = "axis.text.x",
                                                 tooltip = "Beginning of Year | End of Year",
                                                 hover_css = "fill:#22a783;stroke:none;"), 
          strip.text = element_text(size = 18), 
          strip.background = element_rect(fill = 'lightgrey', color = 'lightgrey'), 
          plot.caption = element_text_interactive(hjust = -.001, vjust = 1, size = 12, color = '#808080', 
                                                  data_id = 'plot.caption', 
                                                  tooltip = '*2020 unavailable due to shift to remote learning in March 2020 \n**Results for students without scores in Beginning of Year and End of Year are withheld', 
                                                  
                                                  
                                                  hover_css = "fill:#22a783;stroke:none;"), 
          plot.title = element_text_interactive(size = 18, 
                                                tooltip = 'Testing subject')
    )
  
  .fontSize <- 4
  
  
  plotData <- plotData %>% 
    mutate(profBOYL= as.character(profBOYLabel), 
           profEOYL = as.character(profEOYLabel))
  
  
 p <-  ggplot(plotData,
         aes(axis1 = fct_rev(profBOYL),
             axis2 = fct_rev(profEOYL),
             y = sum,
             fill = profBOYL)) +
    geom_alluvium(aes(fill = profBOY)) +
    geom_flow(color = '#e57a3c', curve_type = 'quintic') +
    scale_x_discrete(limits = c("Beginning \nof Year", "End \nof Year")
                      , expand = c(.2, .05)
                     ) +
    scale_fill_manual(breaks = unique(plotData$profBOYL),
                      values = c("#315683","#6c7070", 'red', 'green'),
                      na.value = NA,
                      label = unique(plotData$profBOYL),
                      guide = guide_legend(reverse = F)
                      ) +
    geom_stratum(aes(fill = profEOYL), color = 'grey', width = 2/3) +
    geom_stratum(aes(fill = profBOYL), color = 'grey', width =2/3) +
    geom_text(stat = 'stratum', aes(label = percentInBoyPB), vjust = 1, size = 6, color = 'white') +
    geom_text(stat = 'stratum', aes(label =  paste0('n = ', totalInBoyPB)),  vjust = -.2, size = .fontSize, color = 'white', lineheight = .75) +
    geom_text(stat = 'stratum', aes(label = percentInEoyPB), vjust = 1, size = 6, color = 'white')+
    geom_text(stat = 'stratum', aes(label =  paste0('n = ', totalInEoyPB)),  vjust = -.2, size = .fontSize, color = 'white', lineheight = .75) +
    labs(fill = 'Performance Level',
         title = unique(plotData$contentGroupName),
         caption= '*2020 unavailable due to shift to remote learning in March 2020 \n**Results for students without scores in Beginning of Year and End of Year are withheld')+
    facet_wrap(vars(factor(EndYear)),
               nrow = 1,
               scales = 'free_y')+
    trendTheme

 ggiraph(code = print(p),
         tooltip_opacity = 1,
         tooltip_offx = 5,
         width_svg = 10,
         height_svg = 7,
         tooltip_extra_css = 'color:#333333;stroke:#e26d28;background:white;border:1px solid darkgrey;font-size:10px',
         hover_css = "cursor:pointer;stroke:#EA9563;",
         width = 1
 )
  
}
  
