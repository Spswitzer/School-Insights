interactivePlot <- function(plotName = choiceIn){
  
  ggiraph(code = print(plotName), 
          tooltip_opacity = 0.9, 
          tooltip_offx = 50, 
          height_svg = 2, 
          width_svg = 5,
          tooltip_extra_css = 'color:#315683;stroke:#e26d28;background:white;border: 1px solid darkgrey;',
          hover_css = "cursor:pointer;stroke:#e26d28;border-width:thick;",
          selection_type = 'none')
}