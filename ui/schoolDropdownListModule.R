# School Dropdown List Module ----
dropDownModuleUi <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = "SchoolID",
      label = "Select a School",
      choices = list(
        'District Summary'= 9998,
        'Multi Level' = multiNames,
        'Elementary' = elemNames,
        'Middle' = midNames,
        'High' = highNames),
      selected = 9998,
      tags$select(
        class = "dropdown"), 
      multiple = F))
}

dropDownModuleServer <- function(id) {
  moduleServer(
    id, function(input, output, session){
    }
  )
}