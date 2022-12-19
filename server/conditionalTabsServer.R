#### Conditionals to control which tabs are visible, based on data availability ####
# * Remove academic tabs for Warren Tech due to low primary Enrollment ----
observeEvent(input$SchoolID,{
  if(input$SchoolID == '9234'| input$SchoolID == '9245' |input$SchoolID == '1001'){   #Warren Tech Central and North South
    hideTab(inputId = "tabs", target = "main-DistrictTests")
    hideTab(inputId = "tabs", target = "main-StateAcademic")
    hideTab(inputId = 'tabs', target = 'main-Print')
  } else {
    showTab(inputId = "tabs", target = "main-DistrictTests")
    showTab(inputId = "tabs", target = "main-StateAcademic")
    showTab(inputId = 'tabs', target = 'main-Print')
  }
})

observeEvent(input$SchoolID,{
  if(input$SchoolID == '4408'){   #JVA
    hideTab(inputId = 'tabs', target = 'main-Print')
  } else {
    showTab(inputId = 'tabs', target = 'main-Print')
  }
})

#* High Schools with no Middle School ----
highSchools <- masterSchoolProfileData %>%
  filter(emhLevel == 'High'| cdeSchoolCode == '0965')

observeEvent(input$SchoolID,{
  if(input$SchoolID  %in% unique(highSchools$cdeSchoolCode)){
    hideTab(inputId = "cmasTabBox", target = "sub-StatewideTestsCmas2022")
    hideTab(inputId = 'districtTestsTabBox', target = 'sub-DistrictTestsEarlyLit')
    hideTab(inputId = 'districtTestsTabBox', target = 'sub-DistrictTestsEarlyLitTrend')
       showTab(inputId = "cmasTabBox", target = "sub-StatewideTestsPSAT")
  } else {
    showTab(inputId = "cmasTabBox", target = "sub-StatewideTestsCmas2022")
    showTab(inputId = 'districtTestsTabBox', target = 'sub-DistrictTestsEarlyLit')
    showTab(inputId = 'districtTestsTabBox', target = 'sub-DistrictTestsEarlyLitTrend')
    hideTab(inputId = "cmasTabBox", target = 'sub-StatewideTestsPSAT')
   }
})
# * High School or Middle School ----
highOrMidddle <- masterSchoolProfileData %>%
  select(cdeSchoolCode, emhLevel) %>%
  filter(emhLevel == 'High'| emhLevel == 'Jr/Sr'| emhLevel == 'Middle')

observeEvent(input$SchoolID,{
  if(input$SchoolID  %in% unique(highOrMidddle$cdeSchoolCode)){
    hideTab(inputId = 'districtTestsTabBox', target = 'sub-DistrictTestsEarlyLit')
    hideTab(inputId = 'districtTestsTabBox', target = 'sub-DistrictTestsEarlyLitTrend')
  } else {
    showTab(inputId = 'districtTestsTabBox', target = 'sub-DistrictTestsEarlyLit')
    showTab(inputId = 'districtTestsTabBox', target = 'sub-DistrictTestsEarlyLitTrend')
  }
})
# * Not High School ----
notHighSchools <- masterSchoolProfileData %>%
  select(cdeSchoolCode, emhLevel) %>%
  filter(emhLevel  != 'Jr/Sr' & emhLevel != 'High' & emhLevel != 'K-12')

observeEvent(input$SchoolID,{
  if(input$SchoolID  %in% unique(notHighSchools$cdeSchoolCode)){
     hideTab(inputId = "cmasTabBox", target = "sub-StatewideTestsPSAT")
    hideTab(inputId = 'cmasTabBox', target = 'sub-pwr')
  } else {
     showTab(inputId = "cmasTabBox", target = "sub-StatewideTestsPSAT")
     showTab(inputId = 'cmasTabBox', target = 'sub-pwr')
  }
})

# ACCESS for ELLs ----
observeEvent(input$SchoolID, {
  if(input$SchoolID %in% unique(accessComplete$schNumber)){
    showTab(inputId = 'cmasTabBox', target = 'sub-access')
  } else {
    hideTab(inputId = 'cmasTabBox', target = 'sub-access')
  }
})
