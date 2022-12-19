
#' SPF Overall Rating
#'
#' @param .hjust 
#'
#' @return ggiraph plot

spfOverallPlot <- function(.hjust = -0.1) {

p <- ggplot(data = SPFOverall, 
            mapping = aes(x = PctPts, 
                          y = Indicator))+
  geom_col(fill = SPFOverall$color)+
  geom_text_interactive(aes(label = Rating, 
                            tooltip = Rating,
                            x = 0), 
                        vjust = 0.2, 
                        hjust = .hjust,
                        lineheight = .6,
                        size = 28, 
                        color = 'white') +
  geom_text_interactive(aes(label = paste0(PctPts, '% of points earned'), 
                            tooltip = paste0(PctPts, '% of points earned'), 
                            x = 0), 
                        vjust = 1.5, 
                        hjust = .hjust,
                        lineheight = .3,
                        size = 26, color = 'white')+
  scale_color_manual(values = SPFOverall$color)+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        plot.title = element_text(size = 16, color = '#515151'),
        plot.subtitle = element_text(size = 12, color = '#515151'),
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
ggiraph(print(p), 
        width_svg = 18, 
        tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
        hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
}