###This function is the following displays: DIBELS status plot
### DIBELS Plots ----

dibelsPlotting <- function(){
  
  textColor <-  
    list(element_text(colour = "darkgrey"), 
         element_text(colour = "#315683"), 
         element_text(colour = "#315683"), 
         element_text(colour = "#315683"), 
         element_text(colour = "#315683"), 
         element_text(colour = "#315683"), 
         element_text(colour = "#315683"), 
         element_text(colour = "#315683"), 
         element_text(colour = "#315683"))
  
  geomBarCode <-     
    geom_bar_interactive(aes(fill = category, #_interactive
                             alpha = school,
                             tooltip =
                               paste0('<b>Time period: </b>', testingPeriodName,
                                      '<br>', '<b>Group: </b>', school, ' - ', subcategory, 
                                      '<br>', "<b>Total tested: </b>", scales::comma(subcategoryN, accuracy = 1))
                 ), 
                         stat = 'identity', 
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
                                   # tooltip = 'Available reporting sites: <br>School <br>District', 
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
                                                       # hjust = 0.5, 
                                                       vjust = 0.5,
                                                       face = 'bold',
                                                       size = 35), 
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
      code = print(dibelsPlot),
      tooltip_opacity = 1,
      tooltip_offx = 50,
      tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
      hover_css = "cursor:pointer;stroke:#EA9563;",
      width = 1,
      width_svg = 14,
      height_svg = 9)
  }
  
  if(input$SchoolID == '9998'){

    dibelsByDistrict <-  gradeDibels %>%#dibelsData()
      filter(cdeSchoolNumber == '9998')

    dibelsPlot <- ggplot(data = dibelsByDistrict,
                         mapping = aes(x = school,
                                       y = pctMeetExceed))+
      geomBarCode +
      scale_alpha_manual(values = c(0.5, 1))+
      scale_fill_manual(name = 'category',
                        values=c('darkgrey', rep("#315683", 4)))+
      textCode +
      ylim(0, 1.4)+
      facetCode +
      coord_flip() +
      theme_minimal()+
      themeCode

    ggiraphFunction()
  } else {
    dibelsPlot <- ggplot(data = dibelsBySchool, #dibelsBySchool, 
                         mapping = aes(x = school, 
                                       y = pctMeetExceed))+
      geomBarCode +
      scale_alpha_manual(values = c(0.5, 1))+
      scale_fill_manual(name = 'category',
                        values=c('darkgrey', rep("#315683", length(gradesInSchool))))+
      textCode +
      ylim(0, 1.4)+
      facetCode +
      coord_flip() +
      theme_minimal()+
      themeCode
    
    ggiraphFunction()
  }
}
