## CMAS 2016-2019 Trend Reading M/E ----
## CMAS 2016-2019 Trend Math M/E ----
cmasTrendFunct <-  function(school, content){
 # school <- '0030'
 # content <- "MATH"
  
   cmasTrendSchool <- cmasTrend %>%
    filter(CDESchoolNumber == school) %>%
    filter(ContentName == content,
           EndYear != '2015',
           stat == "percentMet_Exceed")

  cmasDistrictTrend <- cmasDistrictTrend %>%
    filter(ContentName == content, 
           EndYear != '2015', 
           stat == "percentMet_Exceed") %>% 
    mutate(CDESchoolNumber = '9998')
  
  cmasCombined <- cmasTrendSchool %>% 
    rbind(cmasDistrictTrend) %>% 
    mutate(site = case_when(
      CDESchoolNumber == '9998' ~ "District", 
      TRUE ~ "School"
    )) %>% 
    mutate(site = factor(site, 
                         levels = c("School", "District"), 
                         labels = c("School", "District"), 
                         ordered = TRUE)) %>% 
    arrange(site)
  
  ggplot(data = cmasCombined, 
         mapping = aes(x = EndYear, 
                       y = value, 
                       fill = site))+
    geom_bar(stat = "identity", 
             position = 'dodge', 
             color = 'darkgrey')+
    geom_text(data = cmasCombined, 
              aes(x = EndYear, 
                  y= value, 
                  label = paste0(round(value), '%')), 
              position = position_dodge(width = 1), 
              vjust = 1.2, 
              size = 6, 
              color = 'white')+
    ylim(c(-5, 115))+
    scale_fill_manual(values = c("#315683", "#6c7070"))+
    labs(caption = 'CMAS tested in grades 3-8 only since 2017')+
    theme_minimal()+
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 16),
          legend.direction = 'horizontal',
          legend.position = c(0.1, 0.90),
          legend.text= element_text(size = 12),
          legend.title = element_blank(),
          legend.key.size = unit(3, "mm"),
          legend.spacing.x = unit(1.0, 'mm'),
          legend.background = element_rect(fill = "white", color = "darkgrey"),
          panel.background = element_rect(fill = '#bcc0c4', 
                                          colour = '#bcc0c4'),
          # plot.background = element_rect(fill = '#EBF1F6', 
          #                                colour = '#EBF1F6'),
          plot.caption = element_text(size = 12),
          panel.spacing.y=unit(.2, "lines"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid = element_blank())
}
# cmasTrendFunction('0109', "MATH")
