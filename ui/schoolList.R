schoolListModuleUI <- dropDownModuleUi <- function(id) {
  
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = "SchoolID",
      label = "Select a School",
      choices = list(
        'District Summary'= 9998,
        'Elementary' = elemNames,
        'Middle' = midNames,
        'High' = highNames,
        'Multi Level' = multiNames),
      selected = 9998,
      tags$select(
        class = "dropdown",
      ), 
      multiple = F
    )
  )
}

schoolListModuleServer <- dropDownModuleServer <- function(id) {
  moduleServer(
    id, function(input, output, session){
    }
  )
}