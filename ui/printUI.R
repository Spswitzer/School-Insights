#### (5) Print Summary Tab #####
uiPrint <-  tabPanel(value = 'main-Print',
                     title = div(
                       img(src = "print-icon.png",
                           alt = "Print",
                           title = 'print')
                     ),
                     fluidRow(
                       column(
                         width = 10,
                         shinydashboard::box(height = '801px', width = 550,
                                             htmlOutput("pdfFrame")
                         )#close box
                       ) #close column
                     ) #close fluidRow
)  # Closes tabPanel