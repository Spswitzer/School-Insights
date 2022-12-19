#' Plot alluvials for DIBELS three-year trend
#'
#' @param .fontSize 
#'
#' @return ggiraph interative plot
#' @export
#'
#' @examples 5, 4
#' 
#' 
#' 
dibelsTrendPlot <- function( df = dibelsTrendCombined, .fontSize = 5) {
  
  p <- ggplot(df,
              aes(y = sum,
                  axis1 = str_wrap(profBOY, 10),
                  axis2 = str_wrap(profEOY, 10),
                  fill = profBOY
              )) +
    geom_flow(color = '#e57a3c', curve_type = 'quintic') +
    scale_x_discrete(limits = c("Beginning \nof Year", "End \nof Year")) +
    scale_fill_manual(values = c("#315683","#6c7070", 'red'), 
                      na.value = NA) +
    geom_stratum(aes(fill = profEOY), color = 'grey', width = 2/3) +
    geom_stratum(aes(fill = profBOY), color = 'grey', width = 2/3) +
    geom_text(stat = 'stratum', aes(label = paste0(round(percentInBoyPB), '%')), vjust = 1, size = 6, color = 'white')+
    geom_text(            stat = 'stratum', 
                          mapping = aes(label = paste0('n = ', totalInBoyPB)), 
                          vjust = -.7, 
                          size = .fontSize, 
                          color = 'white'#, 
                          # tooltip = paste0(df$sum, ' students moving from <br><b>', df$profBOY, '</b> to <b>', profEOY, '</b>')
                          )+
    geom_text(stat = 'stratum', aes(label = paste0(round(percentInEoyPB), '%')), vjust = 1, size = 6, color = 'white')+
    geom_text(stat = 'stratum', aes(label = paste0('n = ', totalInEoyPB)), vjust = -.3, size = .fontSize, color = 'white')+
    labs(fill = 'Performance Level', 
         caption= '*2019-2020 unavailable due to shift to remote learning in March 2020\n**Results for students without scores in Beginning of Year and End of Year are withheld')+
    facet_wrap(vars(factor(EndYear)), 
               nrow = 1, 
               scales = 'free_y')+
    dibelsTrendTheme
  
  ggiraph(code = print(p), 
          width_svg = 12, 
          width = 1,
          tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
          hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;")
}