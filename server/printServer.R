#### (5) Print Summary Server #####
output$pdfFrame <- renderUI({
  
  validate(need(!is.na(input$SchoolID), " "))
  site <<- paste0(input$SchoolID, "_SchoolSummary.pdf")
  my_site <- tags$iframe(class = "iframe-placeholder", src = site, height = 800, width = 900)
  my_site 
})