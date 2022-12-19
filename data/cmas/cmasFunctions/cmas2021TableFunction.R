#' Create Table for CMAS 2021 Achievement Results
#' @param .id 
#' @param .subject 
#' @param .image 
#' @param .navPillsId 
#' @param .navPillsSubject 
#'
#' @return userbox with gt table

cmas2021Table <- function(
                 .df = cmas,
                 .id = 'cmasEla', 
                 .subject = 'English Language Arts', 
                 .subtitle = 'Colorado Measures of Academic Success',
                 .image = 'bookBlank.png', 
                 .navPillsId = 'cmasElaPills', 
                 .navPillsSubject = 'CMAS Reading and Writing 2021') {
  
userBox(
  id = .id, 
  title = userDescription(span(HTML(.subject, "<br>Achievement")), 
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
  .df %>% gt(rowname_col = "grade",
              auto_align = F) %>% 
    cols_label(grade = 'Grade',
               participationRateChar = 'Rate',
               numberOfValidScores = html('Total<br>Students'),
               flag = ' ',
               meanScaleScore = html('Mean<br>Scale Score'),
               percentMetOrExceededChar = html('Percent<br> Meets/Exceeds')) %>% 
    tab_spanner(label =  "Participation",
                columns = c('participationRateChar', 'numberOfValidScores', 'flag')) %>% 
    tab_spanner(label = 'Achievement', 
                columns = c('meanScaleScore', 'percentMetOrExceededChar')) %>% 
    text_transform(locations = cells_body(columns = 'flag',
                                          rows = as.numeric(participationRate) < 95),
                   fn = f) %>%
    cols_hide(percentMetOrExceededExpectations) %>% 
    cols_hide(participationRate) 
) #close userBox
  
}