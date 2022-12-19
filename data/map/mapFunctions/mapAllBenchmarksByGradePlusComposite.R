## Map Achievement Plots ----
### Reading ----
### Math ----
# 
# library(ggh4x)
mapAchPlotBenchmarksByGradeWithComposite <- function(df, subject, schoolCode) {
  # > df= m2
  # > schoolCode = '0030'
  # > subject = 'READING'
  
  #Reshape data from long to wide
  plotData <- df %>% 
    filter(contentName == subject) #%>% 
  
  #Calculate number of grades in the school to refine what is displayed in a manner that is customized to each school configuration
  gradesInSchool <- unique(df$subcategory)
  
  #Apply suppression rules at grade level
  mapForPlot <- plotData %>% 
    mutate(pctMeetExceed = case_when(
      subcategoryN < 16 ~ 0,
      TRUE ~ pctMeetExceed)) %>% #plot small N's as )% Favorable so values are not plotted when they should be suppressed
    mutate(subcategoryN = formatC(subcategoryN, format = 'd', big.mark = ','))
  
  #Apply suppression rules at school level
  interactiveElement <- df %>% 
    filter(contentName == subject) %>% 
    mutate(suppress = case_when(
      subcategoryN < 15 ~ 1, 
      TRUE ~ 0))
  
  textColor <-  list(element_text(colour = "darkgrey"), 
                     element_text(colour = "#315683"), 
                     element_text(colour = "#315683"), 
                     element_text(colour = "#315683"), 
                     element_text(colour = "#315683"), 
                     element_text(colour = "#315683"), 
                     element_text(colour = "#315683"), 
                     element_text(colour = "#315683"), 
                     element_text(colour = "#315683"), 
                     element_text(colour = "#315683"), 
                     element_text(colour = "#315683"))
  
  geomBarCode <- 
    geom_bar_interactive(aes(fill = subcategory, #_interactive
                             alpha = school, 
                             tooltip =
                               paste0('<b>Time period: </b>', testingPeriodName,
                                      '<br>', '<b>Group: </b>', school, ' - ', subcategory,
                                      '<br>', "<b>Total students tested: </b>", subcategoryN)
                 ), 
                         stat = 'identity', 
                         position = position_dodge(width = 0.8),
                         width = 0.7)
  
  facetCode <- 
    facet_grid2(rows = vars(subcategory),
                cols = vars(testingPeriodName), 
                switch = 'y',
                strip = strip_nested(bleed = TRUE, 
                                     size = 'variable', 
                                     text_y = textColor)) 

  
  textCode <- 
    geom_text(aes(label = percentAtAboveSuppressed),
              hjust = -0.1, 
              color = '#515151', 
              size = 10)
  
  
  themeCode <-      
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          plot.title = element_text(size = 1, color = 'white'),
          axis.text.x = element_blank(), 
          axis.text.y = element_text(size  = 30#, #_interactive
                                                 # data_id = 'axisYText', 
                                                 # tooltip = 'Available reporting sites: <br>School <br>District'#, 
                                                 # hover_css = "fill:#22a783;stroke:none;"
                                     ),
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.text = element_text(size = 28#, #_interactive
                                                # data_id = 'stripText', 
                                                # tooltip = 'Available reporting periods: <br>Beginning of Year <br>Middle of Year<br>End of Year', 
                                                # hover_css = "fill:#22a783;stroke:none;"
                                    ),
          strip.text.y.left = element_text(angle = 0, 
                                           hjust = 0.5, 
                                           vjust = 0.5,
                                           face = 'bold',
                                           size = 30), 
          strip.placement = 'outside',
          strip.background = element_rect(fill = 'white', color = 'white'),
          panel.background = element_rect(fill = '#EBF1F6',
                                          colour = '#EBF1F6'),
          panel.spacing.y=unit(.2, "lines"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          plot.caption = element_text(size = 30),
          legend.position = 'none')

  
  ggiraphFunction <- function(){
    ggiraph(
      code = print(MapAchPlot),
      tooltip_opacity = 1,
      tooltip_offx = 50,
      tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
      hover_css = "cursor:pointer;stroke:#EA9563;",
      width = 1,
      width_svg = 12,
      height_svg = 10)
  }
  
  if (schoolCode == '9998'){
    MapAchPlot <- ggplot(data = filter(mapForPlot, school == 'District'),  
                         mapping = aes(x = school, 
                                       y = pctMeetExceed)) +
      geomBarCode +
      scale_alpha_manual(values = c(0.5, 1))+
      scale_fill_manual(name = 'subcategory',
                        values=c('darkgrey', rep("#315683", 10)))+
      textCode +
      ylim(0,1.20)+
      facetCode+
      coord_flip() +
      theme_minimal() +
      themeCode
    
    ggiraphFunction()
    
  } else if(first(interactiveElement$suppress == 0)){
    
    MapAchPlot <- ggplot(data = mapForPlot, 
                         mapping = aes(x = school, 
                                       y = pctMeetExceed))+
      geomBarCode +
      scale_alpha_manual(values = c(0.5, 1))+
      scale_fill_manual(name = 'subcategory',
                        values=c('darkgrey', rep("#315683", length(gradesInSchool)-1)))+
      textCode +
      ylim(0,1.20) +
      facetCode +
      coord_flip() +
      theme_minimal() +
      themeCode
    
    ggiraphFunction()

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
            plot.title = element_text(size = 14, color = 'grey'), 
            plot.subtitle = element_text(size = 14, color = 'grey'),
            panel.spacing.y=unit(.2, "lines"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid = element_blank())
    
    ggiraph(print(p), 
            width_svg = 15,
            height_svg = 5)
  }
}


