## MAP Trends ----
### Reading ----
### Math ----

# school <- '0030'
# content <- 'READING'

mapTrendAllPlsPlotFlip <- function(school, content){
  
  df <- allMapAllLevels %>%  
    filter(TestedAtSchoolNumber == school, 
           ContentGroupName == content) %>% 
    mutate(SchoolN = scales::comma(SchoolN))
  
  dfPositive <- df %>% 
    filter(str_detect(ProficiencyLongDescription, 'High')) 
  
  dfPositiveSummary <- dfPositive %>% 
    distinct(percentageInLevel, .keep_all = T)
  
  dfNegative <- df %>% 
    filter(str_detect(ProficiencyLongDescription, 'High', negate = T))
  
  dfNegativeSummary <- dfNegative %>% 
    distinct(percentageInLevel, .keep_all = T) %>% 
    mutate(percentageDiv = -percentageInLevel)
  
  p <- ggplot() +
    geom_bar(data = dfPositive, #create one geom for high PL _interactive
                         mapping = aes(x = quarter,
                                       y = divergPercent,
                                       fill = fct_rev(ProficiencyLongDescription)#,
                                       # tooltip = paste0('<b>Year: </b>', EndYear,
                                       #                  '<br><b>Testing benchmark: </b>', quarter,
                                       #                  '<br><b>Total students tested: </b>', SchoolN,
                                       #                  '<br><b>Percentage in level: </b>', round(PctInLevel*100), "%"
                                       #                  # ,
                                       #                  # '<br><b>Participation rate: </b>', 'placeholder'
                                       # )
                                       ),
                         stat="identity",
                         width = .5,
                         color = 'white') +
    geom_bar(data = dfNegative, #create another geom for low PL _interactive
                         mapping = aes(x = quarter,
                                       y = divergPercent,
                                       fill = fct_rev(ProficiencyLongDescription)#,
                                       # tooltip = paste0('<b>Year: </b>', EndYear,
                                       #                  '<br><b>Testing benchmark: </b>', quarter,
                                       #                  '<br><b>Total students tested: </b>', SchoolN,
                                       #                  '<br><b>Percentage in level: </b>', round(PctInLevel*100), "%"
                                       # )
                                       ),
                         stat="identity",
                         width = .5,
                         color = 'white') +
    scale_fill_manual(values =  c('#d8274a', # red
                                  '#e57a3c',  #orange
                                  '#e2a331', #yellow
                                  '#22a783', #green
                                  '#3688c8' #blue
    ), #colors
    breaks =  c("Low", "Low Average", "Average",  "High Average", "High"),
    labels =  c("Low", "Low Average", "Average",  "High Average", "High"), 
    guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(limits = c(-1.2, 1.2),
                       breaks =  c(-1, -0.75, -0.50, -0.25,
                                   0,
                                   0.25, 0.50, 0.75, 1),
                       labels = c('100%', '75%', '50%', '25%', '0', '25%', '50%', '75%', '100%')) +
    scale_alpha_discrete(guide = "none",
                         range = c(0.2, 1))+
    labs(caption = 'Beginning in 2022-2023, results include students in Grade 1-10',
         fill = 'Performance Levels',
         alpha = 'Participation Rate'
    ) +
    geom_hline(yintercept = 0,
               color = 'grey',
               linetype = 'solid') +
    geom_label(data = dfPositiveSummary, #_interactive
                           mapping = aes(x = fct_rev(quarter),
                                         y = percentageInLevel + 0.1,
                                         label = paste0(round(percentageInLevel*100), '%')#,
                                         # tooltip = paste0('<b>Year: </b>', EndYear,
                                         #                  '<br><b>Testing benchmark: </b>', quarter,
                                         #                  '<br><b>Percentage in High Average or High levels: </b>', round(percentageInLevel*100), "%"
                                         # )
                                         ),
                           size = 4,
                           label.size = 0.1,
                           label.padding = unit(0.1, "lines"),
                           color = '#333333')+
    geom_label(data = dfNegativeSummary, #_interactive
                           mapping = aes(x = fct_rev(quarter),
                                         y = percentageDiv - 0.1,
                                         label = paste0(round(percentageInLevel*100), '%')#,
                                         # tooltip = paste0('<b>Year: </b>', EndYear,
                                         #                  '<br><b>Testing benchmark: </b>', quarter,
                                         #                  '<br><b>Percentage in Average or Low levels: </b>', round(percentageInLevel*100), "%"
                                         # )
                                         ),
                           size = 4,
                           label.size = 0.1,
                           label.padding = unit(0.1, "lines"),
                           color = '#333333')+
    facet_grid(cols = vars(factor(EndYear)), 
               shrink = T) +
    theme_minimal() +
    theme(
      axis.title = element_blank(), #remove titles from x and y axes
      axis.text.x = element_text(size = 12), #remove axis text from X axis
      axis.text.y = element_blank(),
      legend.box.background = element_rect(color = "lightgrey"),
      legend.key.size = unit(.25, 'cm'), #change legend key size
      legend.position = 'top',
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      panel.background = element_rect(fill = '#f9f9f9', color = 'white'),
      panel.grid = element_blank(), #remove grid lines
      plot.title = element_text(hjust = 0),
      plot.caption = element_text(size = 10, hjust = 0),
      strip.background = element_rect(colour = 'grey'),
      strip.text = element_text(size = 14)# move title over y axis text
    )
  
  ggiraph(code = print(p),
          tooltip_opacity = 1,
          tooltip_offx = 5,
          tooltip_extra_css = 'color:#333333;stroke:#e26d28;background:white;border:1px solid darkgrey;font-size:10px',
          hover_css = "cursor:pointer;stroke:#EA9563;",
          width = 1
  )
  
} #close function


# tables ---------

mapTrendTable <- function(school, content){
  
  dfWide <- allMapAllLevelsWide %>% 
    filter(testedAtSchoolNumber == school,
           contentGroupName == content) %>% 
    select(-testedAtSchoolNumber, -contentGroupName) %>% 
    arrange(endYear)
  
  gt(dfWide) %>% 
    cols_label(quarter = '', 
               low = md('Low'), 
               lowAverage = md('Low<br>Average'),
               average = md('Average'), 
               highAverage = md('High<br>Average'), 
               high = md('High')) %>% 
    cols_align(align = c('left'), 
               columns = quarter) %>% 
    cols_align(align = c('center'), 
               columns = low:high) %>% 
    cols_width(
      everything() ~ px(60)
    ) %>% 
    tab_spanner(label = md('**Performance Levels**<BR><i>Percentage of students in level</i>'), 
                columns = c(low:high)) %>% 
        tab_options(source_notes.font.size = 14, 
                data_row.padding = px(5), 
                column_labels.padding = px(5), 
                heading.padding = px(5), 
                table.font.size = 14, 
                row_group.font.weight = 'bold',
                row_group.font.size = 14, 
                row_group.background.color = 'grey'
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("left"),
        color = "lightgrey",
        weight = px(1.5),
        style = "solid"
      ),
      locations = cells_body(
        columns = highAverage,
        rows = everything()
      )
    ) %>% 
    fmt_markdown(columns = everything()) %>% 
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    )
  
}

