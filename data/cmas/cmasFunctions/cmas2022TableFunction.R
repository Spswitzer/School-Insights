#' Create Table for CMAS 2021 Achievement Results
#' @param .id 
#' @param .subject 
#' @param .image 
#' @param .navPillsId 
#' @param .navPillsSubject 
#'
#' @return userbox with gt table

cmas2022Table <- function(
                 .df = cmas,
                 .id = 'cmasEla', 
                 .subject = 'Language Arts', 
                 .subtitle = 'Colorado Measures of Academic Success',
                 .image = 'bookBlank.png', 
                 .navPillsId = 'cmasElaPills', 
                 .navPillsSubject = 'CMAS Reading and Writing 2022') {
  
userBox(
  id = .id, 
  title = userDescription(span(HTML(.subject, "<br>Achievement and Growth")), 
                          subtitle = HTML(.subtitle), 
                          type = 2, 
                          image = .image
                          ), 
  gradient = FALSE,
  background = "blue",
  collapsible = F,
  boxToolSize = "xs",
  width = NULL, 
  status = 'primary', 
  HTML( .navPillsSubject),
  .df %>% gt(
      rowname_col = "grade",
              auto_align = F) %>% 
    cols_label(grade = 'Grade',
               participationRateChar = html('Participation<br>Rate'),
               totNumOverall = html('Total<br>Students'),
               participationThreshold = ' ',
               overallMeanSsco = html('Mean<br>Scale Score'),
               pctBinPerfChar = html('Percent<br> Meets/ Exceeds'), 
               medianSgpCohort = html('Median<br>Growth Percentile'), 
               nCountSgpCohort = html("Total<br>Students")) %>%
    tab_spanner(label = 'Achievement',
                columns = c('totNumOverall','participationRateChar', 'participationThreshold','overallMeanSsco', 'pctBinPerfChar')) %>%
    tab_spanner(label = 'Growth', 
                columns = c('nCountSgpCohort', 'medianSgpCohort')) %>% 
    text_transform(locations = cells_body(columns = 'participationThreshold',
                                          rows = participationRate < .85),
                   fn = f) %>%
    cols_hide(c(participationRate, cdeSchoolNumber)) %>% 
    opt_row_striping(row_striping = TRUE) %>% 
    tab_style(
      style = list(
        cell_text(color = "white")
      ),
      locations = cells_body(
        columns = participationThreshold
              )
    ) %>% 
    tab_options(table.font.size = 14)
  
) #close userBox
  
}
