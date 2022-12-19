#PSAT Diverging Bars ----

satDiverging <- function(.df = stateTestData()$statewideData, 
                         # .school, 
                         .content = 'LANGUAGE ARTS'){

df <- .df %>%
  filter(
    # cdeSchoolNumber == .school,
         contentName == .content, 
         category == 'all',
         grade %in% c('Gr10', 'Gr9')) %>% 
  mutate(profDescription = factor(profDescription, 
                                  levels = c("Need to Strengthen Skills", 
                                             "Approaching Benchmark", 
                                             "Meet or Exceed Benchmark" ), 
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



## Plot components

sfm <- scale_fill_manual(values =  c('#d8274a', # red
                              # '#e57a3c',  #orange
                              '#e2a331', #yellow
                              '#22a783'#, #green
                              # '#3688c8' #blue
), #colors
breaks = c("Need to Strengthen Skills","Approaching Benchmark","Meet or Exceed Benchmark"),
labels = c("Need to \nStrengthen Skills","Approaching \nBenchmark","Meet or Exceed \nBenchmark"),
guide = guide_legend(reverse = F)
)

syc <-   scale_y_continuous(limits = c(-1.2, 1.2),
                     breaks =  c(-1, -0.75, -0.50, -0.25,
                                 0,
                                 0.25, 0.50, 0.75, 1),
                     labels = c('100%', '75%', '50%', '25%', '0', '25%', '50%', '75%', '100%')) 
sad <-   scale_alpha_continuous(guide = "none",
                       range = c(0.2, 1))

fw <-   facet_wrap(~gradeLevel,
             ncol = 1)

gh <-   geom_hline(yintercept = 0,
             color = 'grey',
             linetype = 'solid') 
gli1 <-   geom_label(data = dfPositiveSummary, #_interactive
                         mapping = aes(x = contentName,
                                       y = pctBinPerf + 0.1,
                                       label = paste0(round(pctBinPerf*100), '%')#,
                                       # tooltip = paste0('<b>Percentage exceeded or met expectations: </b>', round(pctBinPerf*100), "%"
                                       # )
                     ),
                         size = 10,
                         label.size = 0.1,
                         label.padding = unit(0.1, "lines"),
                         color = '#333333') 
gli2 <-  geom_label(data = dfNegativeSummary, #_interactive
                         mapping = aes(x =contentName,
                                       y = divergPercent - 0.3,
                                       label = paste0(round(pctBinPerf*100), '%')#,
                                       # tooltip = paste0('<br><b>Percentage not exceeded or met expectations: </b>', round(pctBinPerf*100), "%"
                                       # )
                    ),
                         size = 8,
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
  legend.text = element_text(size = 22),
  legend.title = element_blank(),
  panel.background = element_rect(fill = '#f9f9f9', color = 'white'),
  panel.grid = element_blank(), #remove grid lines
  plot.title = element_text(hjust = 0),
  plot.caption = element_text(size = 18),
  # strip.background = element_rect(colour = 'grey'),
  strip.text = element_textbox(
    size = 30,
    color = "black", 
    fill = "#e0e0e0", 
    box.color = "#e0e0e0",
    halign = 0,
    # hjust = 0,
    linetype = 1, 
    r = unit(2, "pt"), 
    width = unit(1, "npc"),
    padding = margin(2, 0, 1, 0), 
    margin = margin(3, 3, 3, 3)
  )
)

# No coord flip _____________________________________________________
if(max(df$participationRate < 0.85, na.rm = T)) {
p <- ggplot() +
  geom_bar_interactive(data = dfPositive, #create one geom for high PL #_interactive
                       mapping = aes(x = contentName,
                                     y = divergPercent,
                                     fill = profDescription,
                                     alpha = participationThreshold,
                                     tooltip = paste0('<b>Performance level: </b>', profDescription,
                                                      '<br><b>Total tested: </b>', totNumOverall,
                                                      '<br><b>Percentage in level: </b>', round(pctPerfLevel*100), "%",
                                                      '<br><b>Participation rate: </b>', round(participationRate*100), '%'
                                     )
                                     ),
                       stat="identity",
                       width = .5,
                       color = 'white') +
  geom_bar_interactive(data = dfNegative, #create another geom for low PL #_interactive
                       mapping = aes(x = contentName,
                                     y = divergPercent,
                                     fill = profDescription,
                                     alpha = participationThreshold,
                                     tooltip = paste0('<b>Performance level: </b>', profDescription,
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
sad +
fw+
gh +
gli1 +
gli2 +
  coord_flip()+
  tm+
  t


ggiraph(code = print(p),
        tooltip_opacity = 1,
        tooltip_offx = 5,
        width_svg = 15,
        height_svg = 5,
        tooltip_extra_css = 'color:#333333;stroke:#e26d28;background:white;border:1px solid darkgrey;font-size:16px',
        hover_css = "cursor:pointer;stroke:#EA9563;",
        width = 1
) 
} else {
  p <- ggplot() +
    geom_bar_interactive(data = dfPositive, #create one geom for high PL _interactive
                         mapping = aes(x = contentName,
                                       y = divergPercent,
                                       fill = profDescription,
                                       tooltip = paste0('<b>Performance level: </b>', profDescription,
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
    # sad +
    fw+
    gh +
    gli1 +
    gli2 +
    coord_flip()+
    tm+
    t
  
  
  ggiraph(code = print(p),
          tooltip_opacity = 1,
          tooltip_offx = 5,
          width_svg = 15,
          height_svg = 5,
          tooltip_extra_css = 'color:#333333;stroke:#e26d28;background:white;border:1px solid darkgrey;font-size:16px',
          hover_css = "cursor:pointer;stroke:#EA9563;",
          width = 1
  )
}
}
