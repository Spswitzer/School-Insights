# CMAS 2022 Diverging Plots ---

cmasDiverging <- function(.df = stateTestData()$statewideData, #.school, 
                          .content = 'LANGUAGE ARTS'){
 
   df <- .df %>%
    # filter(cdeSchoolNumber == .school) %>%
    filter(contentName == .content) %>% 
    filter(category == 'all',
    grade %in% c('Gr3', 'Gr4', 
                 'Gr5', 'Gr6', 
                 'Gr7', 'Gr8')) %>% 
     mutate(profDescription = factor(profDescription, 
                                     levels = c("Did Not Yet Meet", "Partially Met",
                                                "Approached", "Met", "Exceeded" ), 
                                     ordered = T))

   dfPositive <- df %>%
     filter(profFlag ==1)
   
   dfPositiveSummary <- dfPositive %>%
     distinct(grade, .keep_all = T)
   
   dfNegative <- df %>%
     filter(profFlag ==0)
   
   dfNegativeSummary <- dfNegative %>%
     distinct(grade, .keep_all = T) %>%
     mutate(percentageDiv = -pctBinPerf)
   
   #Plot components
 sfm <-   scale_fill_manual(values =  c('#d8274a', # red
                                 '#e57a3c',  #orange
                                 '#e2a331', #yellow
                        
                                 '#3688c8', #blue
                                 '#22a783' #green
   ), #colors
   breaks = c("Did Not Yet Meet", "Partially Met", "Approached","Exceeded","Met"),
   labels = c("Did Not Yet Meet", "Partially Met", "Approached","Exceeded", "Met"),
   guide = guide_legend(reverse = F)) 
  
 syc <-    scale_y_continuous(limits = c(-1.20, 1.20),
                        breaks =  c(-1.00, -.75, -.50, -.25,
                                    0,
                                    .25, .50, .75, .100),
                        labels = c('100%', '75%', '50%', '25%', '0', '25%', '50%', '75%', '100%'))
l <-  labs(fill = 'Performance Levels', 
      caption = 'Results with less than 85% participation rate are faded out') 

fw <-    facet_wrap(~gradeLevel, 
                    ncol = 1) 
gh <-    geom_hline(yintercept = 0,
              color = 'grey',
              linetype = 'solid') 
   
gli <-    geom_label(data = dfPositiveSummary, #_interactive
                          mapping = aes(x = contentName,
                                        y = pctBinPerf + 0.1,
                                        label = pctBinPerfChar#,
                                        # tooltip = paste0('<b>Percentage exceeded or met expectations: </b>', pctBinPerfChar)
                                        ),
                          size = 10,
                          label.size = 0.1,
                          label.padding = unit(0.1, "lines"),
                          color = '#333333') 
  gli2 <-    geom_label(data = dfNegativeSummary, #_interactive
                          mapping = aes(x = contentName,
                                        y = percentageDiv-0.10,
                                        label = pctBinPerfChar#,
                                        # tooltip = paste0('<br><b>Percentage not exceeded or met expectations: </b>', pctBinPerfChar)
                                        ),
                          size = 10,
                          label.size = 0.1,
                          label.padding = unit(0.1, "lines"),
                          color = '#333333')

tm <-    theme_minimal() 
library(ggtext)
t <-    theme(
     axis.title = element_blank(), #remove titles from x and y axes
     axis.text.x = element_blank(),
     axis.text.y = element_blank(),
     legend.box.background = element_rect(color = "lightgrey"),
     legend.key.size = unit(.25, 'cm'), #change legend key size
     legend.position = 'top',
     legend.text = element_text(size = 26),
     legend.title = element_blank(),
     panel.background = element_rect(fill = '#f9f9f9', color = 'white'),
     panel.grid = element_blank(), #remove grid lines
     plot.title = element_text(hjust = 0),
     plot.caption = element_text(size = 22),
     strip.text = element_textbox(
       size = 26,
       color = "black", 
       fill = "#e0e0e0", 
       box.color = "#e0e0e0",
       halign = 0,
       linetype = 1, 
       r = unit(2, "pt"), 
       width = unit(1, "npc"),
       padding = margin(2, 0, 1, 0), 
       margin = margin(3, 3, 3, 3)
     )
   )
   
   if(max(df$participationRate < 0.85, na.rm = T)) {
   p <- ggplot() +
     geom_bar_interactive(data = dfPositive, #create one geom for high PL 
                          mapping = aes(x = contentName,
                                        y = divergPercent,
                                        fill = fct_rev(profDescription),
                                        alpha = participationThreshold,
                                        tooltip = paste0('<b>Performance level: </b>', profDescription,
                                                         '<br><b>Total in level: </b>', nInLevel,
                                                         '<br><b>Total tested: </b>', totNumOverall,
                                                         '<br><b>Percentage in level: </b>', round(pctPerfLevel*100), "%",
                                                         '<br><b>Participation rate: </b>', round(participationRate*100), '%'
                                        )
                          ),
                          stat="identity",
                          width = .5,
                          color = 'white') +
     geom_bar_interactive(data = dfNegative, #create another geom for low PL _interactive
                          mapping = aes(x = contentName,
                                        y = divergPercent,
                                        fill = profDescription,
                                        alpha = participationThreshold,
                                        tooltip = paste0('<b>Performance level: </b>', profDescription,
                                                         '<br><b>Total in level: </b>', nInLevel,
                                                         '<br><b>Total tested: </b>', totNumOverall,
                                                         '<br><b>Percentage in level: </b>', round(pctPerfLevel*100), "%",
                                                         '<br><b>Participation rate: </b>', round(participationRate*100), '%'
                                        )
                          ),
                          stat="identity",
                          width = .5,
                          color = 'white') +
     sfm +
     syc +
     scale_alpha(guide = "none",
                range = c(0.2, 1)
                ) +
     l+
     fw +
     gh +
     gli+
     gli2+
     coord_flip()+
     tm +
     t 
   
   ggiraph(code = print(p),
           tooltip_opacity = 1,
           tooltip_offx = 5,
           width_svg = 15,
           height_svg = 10,
           tooltip_extra_css = 'color:#333333;stroke:#e26d28;background:white;border:1px solid darkgrey;font-size:16px',
           hover_css = "cursor:pointer;stroke:#EA9563;",
           width = 1
   )
   } else {
     p <- ggplot() +
       geom_bar_interactive(data = dfPositive, #create one geom for high PL _interactive
                            mapping = aes(x = contentName,
                                          y = divergPercent,
                                          fill = fct_rev(profDescription),
                                          tooltip = paste0('<b>Performance level: </b>', profDescription,
                                                           '<br><b>Total in level: </b>', nInLevel,
                                                           '<br><b>Total tested: </b>', totNumOverall,
                                                           '<br><b>Percentage in level: </b>', round(pctPerfLevel*100), "%",
                                                           '<br><b>Participation rate: </b>', round(participationRate*100), '%'
                                          )
                            ),
                            stat="identity",
                            width = .5,
                            color = 'white') +
       geom_bar_interactive(data = dfNegative, #create another geom for low PL _interactive
                            mapping = aes(x = contentName,
                                          y = divergPercent,
                                          fill = profDescription,
                                          tooltip = paste0('<b>Performance level: </b>', profDescription,
                                                           '<br><b>Total in level: </b>', nInLevel,
                                                           '<br><b>Total tested: </b>', totNumOverall,
                                                           '<br><b>Percentage in level: </b>', round(pctPerfLevel*100), "%",
                                                           '<br><b>Participation rate: </b>', round(participationRate*100), '%'
                                          )
                            ),
                            stat="identity",
                            width = .5,
                            color = 'white') +
       sfm +
       syc +
       l+
       fw +
       gh +
       gli+
       gli2+
       coord_flip()+
       tm +
       t 
     ggiraph(code = print(p),
             tooltip_opacity = 1,
             tooltip_offx = 5,
             width_svg = 15,
             height_svg = 10,
             tooltip_extra_css = 'color:#333333;stroke:#e26d28;background:white;border:1px solid darkgrey;font-size:16px',
             hover_css = "cursor:pointer;stroke:#EA9563;",
             width = 1
     )
   }  
}
  
  