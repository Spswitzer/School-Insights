###This function is the following displays: PSAT Reading Meet/Exceeds- Reading & Math
## PSAT 2019 Meets/Exceeds Reading ----
## PSAT 2019 Meets/Exceeds Math ----
# psatSchool <- 
  # readRDS(file = "data/sat/psatSchool.rds")

psatPerformanceLevelPlot <-
  function(schoolCode,
           subject = "LANGUAGE ARTS",
           bindDF) {

    psatPlot <- psatSchool %>%
      filter(CDESchoolNumber == schoolCode) %>%
      filter(ContentName == subject) %>%
       mutate(TestedAtSchool = "School")%>%
      bind_rows(bindDF)
    
 p <-    ggplot(
      data = psatPlot,
      mapping = aes(x = TestedAtSchool, y = value, fill = TestedAtSchool)
    ) +
      geom_col_interactive(width = 0.5, 
                           tooltip = paste0('<b>Group: </b>', psatPlot$TestedAtSchool)) +
      scale_fill_manual(values = c('#6c7070', "#315683")) +
      geom_text(label = paste0(round(psatPlot$value, 0), "%"),
                hjust = -0.1,
                color = '#515151', 
                size = 27) +
      ylim(0, 105) +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20, color = 'grey'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 75),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = '#EBF1F6',
                                        colour = '#EBF1F6'),
        plot.background = element_rect(fill = '#EBF1F6',
                                       colour = '#EBF1F6'),
        panel.spacing.y = unit(.2, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = 'none'
      )
 ggiraph(print(p), 
         width_svg = 18, 
         tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
         hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
    
  }

# psatPerformanceLevelPlot(schoolCode = '0109', subject = "MATH", bindDF = cmasOverallMathAch)
# schoolCode <- '9998'
