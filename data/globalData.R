#################### GLOBAL DATA  ######################

# SCHOOL BASICS ----
## Enrollment & Demographic Data ----
masterSchoolProfileData <- 
  readRDS('data/masterSchoolData.rds') %>% 
  filter(cdeSchoolCode != '0148') %>%  #Allendale School Closed - will be updated with 2021 Oct Count
  filter(cdeSchoolCode != '9805') %>%  #MountView School - Low N
  filter(cdeSchoolCode != '2946') %>%  #Fitzmorris School Closed- will be updated with 2022 Oct Count
  mutate(veryAbbreviatedName = case_when( # waiting for requested training from Steve
    cdeSchoolCode == '6470' ~ 'Oberon Middle', 
    cdeSchoolCode == '6848' ~ "Peak Expeditionary",
    cdeSchoolCode == '9234' ~ "Warren Tech Central",
    cdeSchoolCode == '5994' ~ 'Peaks Acad.', 
    cdeSchoolCode == '4408' ~ 'Jeffco Virtual Academy',
    TRUE ~ veryAbbreviatedName)) %>% 
  mutate(emhLevel = case_when( #force level change for Warren Tech Central and North and South
    cdeSchoolCode == '9234' ~ "High", 
    cdeSchoolCode == '9245' ~ "High",
    cdeSchoolCode == '1001' ~ "High",
    cdeSchoolCode == '4798' ~ 'Multi-Level', #Connections
    TRUE ~ emhLevel))
# Warren Tech Special Request See ...noUploadToShinyIO\enrollmentTab\enrollment_demo_schoolType\secondaryEnrollments.R with SelfieSummaryDataHandling.R octCountNoHomeBased object from data handling
secondaryEnrollment <- 
  readRDS(file = 'data/secondaryEnrollment/secondaryEnrollment.rds')

## JVA/ JRLP 2021-2022 Split InsightsRedux\noUploadToShinyIO\basicsTab\enrollment_demo_schoolType
# masterSchoolDataProfileJVA %>% 
#   readRDS(file = 'data/masterSchoolDataProfileJVA.rds')
masterSchoolDataProfileJVA <-  read_csv('masterSchoolDataProfileJVA.csv') %>% 
  mutate(cdeSchoolCode = as.character(cdeSchoolCode))
## Master  Time Period----
masterTimePeriod <-  masterSchoolProfileData %>% 
  select(endYear) %>% 
  mutate(endYear = paste(endYear)) %>% 
  distinct() %>% 
  pull()

## Attendance Data  ----
Attendance <- 
  readRDS('data/attendance/attendance.rds')

attendanceTrend <- 
  readRDS(file = 'data/attendance/attendanceTrend.rds')

## Attendance heading ----
attendanceTimePeriod <- Attendance %>% 
  select(endYear) %>% 
  distinct() %>% 
  pull()

## Teacher to Student Ratio Data ----
ratios <- 
  readRDS('data/ratios/ratios.rds') %>% 
  add_row(cdeSchoolCode = '9998', ratio = 18) # Ratio for district not calculated by state

## List to populate drop down menu and conditionals ----
### Elementary ----
elem <- masterSchoolProfileData %>% 
  filter(emhLevel == 'Elementary') %>% 
  arrange(veryAbbreviatedName)
elemNames <- unique(elem$cdeSchoolCode)
names(elemNames) <- unique(elem$veryAbbreviatedName)

### Middle ----
middle <-  masterSchoolProfileData %>% 
  filter(emhLevel == 'Middle') %>% 
  arrange(veryAbbreviatedName)
midNames <- unique(middle$cdeSchoolCode)
names(midNames) <- unique(middle$veryAbbreviatedName)

### High ----
high <-  masterSchoolProfileData %>% 
  filter(emhLevel == 'High') %>% 
  arrange(veryAbbreviatedName)
highNumbers <- unique(high$cdeSchoolCode) #needed for validation statement - CMAS Trends
highNames <- unique(high$cdeSchoolCode)
names(highNames) <- unique(high$veryAbbreviatedName)

### Multi Level ----
multiLevelSchools <-  masterSchoolProfileData %>% 
  mutate(emhLevel = case_when(
    emhLevel == 'Jr/Sr' | emhLevel == "K-8"| emhLevel == "K-12" ~ 'Multi-Level', 
    TRUE ~ emhLevel
  )) %>% 
  filter(emhLevel == 'Multi-Level') %>% 
  arrange(veryAbbreviatedName) %>% 
  filter(cdeSchoolCode != '9998')
multiNames <- unique(multiLevelSchools$cdeSchoolCode)
names(multiNames) <- unique(multiLevelSchools$veryAbbreviatedName)

### K8 schools ----
kEight <- masterSchoolProfileData %>% 
  filter(emhLevel == 'K-8')
kEightNames <- unique(kEight$cdeSchoolCode)
names(kEightNames) <- unique(kEight$veryAbbreviatedName)

### Junior / Senior High ----
juniorSenior <- masterSchoolProfileData %>% 
  filter(emhLevel == 'Jr/Sr' )
juniorSeniorNames <- unique(juniorSenior$cdeSchoolCode)
names(juniorSeniorNames) <- unique(juniorSenior$veryAbbreviatedName)

### K-12 ----
kTwelve <-  masterSchoolProfileData %>% 
  filter(emhLevel == 'K-12' )
kTwelveNames <- unique(kTwelve$cdeSchoolCode)[-10]
names(kTwelveNames) <- unique(kTwelve$veryAbbreviatedName)[-10]

SchoolNames <- unique(masterSchoolProfileData$cdeSchoolCode)[-164]
names(SchoolNames) <- unique(masterSchoolProfileData$veryAbbreviatedName)[-164]

## Choice Data ----
secondaryEnrollmentForChoicePlots <- secondaryEnrollment %>% 
  select(cdeSchoolNumber, totalStudents) %>% 
  ungroup() %>% 
  group_by(cdeSchoolNumber, totalStudents) %>% 
  summarise()
ChoiceDataBySchool <- 
  readRDS('data/schoolChoiceEnrollment/choiceDataBySchool.rds') %>% 
  full_join(secondaryEnrollmentForChoicePlots, by = c('CDESchoolCode' = 'cdeSchoolNumber')) %>% 
  mutate(totalStudents = as.numeric(totalStudents)) %>% 
  mutate(Count = as.numeric(Count))
AreaChoiceData <- 
  readRDS('data/schoolChoiceEnrollment/areaChoiceData.rds') %>% 
  mutate(Count = as.numeric(Count))
source('data/choice/interactivePlotFunction.R', local = TRUE)

## GeoLocation and Website and Profile Data ----
JeffcoLatLng <-   #Longitude and Latitude data for Schools 
  readRDS(file = "data/mapping/jeffcoLatLng.rds")
schoolDistrict <-  # Shape of District
  readRDS(file ='data/mapping/schoolDistrict.rds')

## Shapes by School Level ----
JeffcoMapElem <- 
  readRDS(file = 'data/mapping/jeffcoElemMap.rds')   #### Access Map of Jeffco Elementary School boundaries ----
JeffcoMapMiddle <-  
  readRDS(file = 'data/mapping/jeffcoMiddleMap.rds')   #### Access Map of Jeffco Elementary School boundaries ----
JeffcoMapHigh <-  
  readRDS(file = 'data/mapping/jeffcoHighMap.rds')   ### Access Map of Jeffco Elementary School boundaries ----
schoolsInElemShape <- 
  unique(JeffcoMapElem@data$ES_CampCod)   #Create lists for conditional 
schoolsInMiddleShape <- 
  unique(JeffcoMapMiddle@data$MS_CampCod)
schoolsInHighShape <- 
  unique(JeffcoMapHigh@data$HS_CampCod)

## List of AECs for conditionals ----
aecList <- masterSchoolProfileData %>% 
  filter(alternateEducationCampus == "TRUE") %>% 
  distinct(cdeSchoolCode) 

nonAec <-  masterSchoolProfileData %>% 
  filter(alternateEducationCampus != "TRUE") %>% 
  filter( cdeSchoolCode != '0776') %>%  #remove Bergen Meadow because there are not any state measures
  distinct(cdeSchoolCode) 

# SCHOOL CULTURE ----

## FSP Survey Data ----
fspOverall <-
  readRDS("data/fsp/fspOverallComplete.rds") #used for overall favorability
fspAllYearsQuartiled <-
  readRDS(file = 'data/fsp/fspQuart.rds')

## MYVH Data ----
# MYVHQuartiles <- 
#   readRDS('data/myvh/myvhQuartiles.rds')
myvhTableAllYears <- 
  readRDS(file = 'data/myvh/MYVHTableAllYears2022.rds')
MYVHResponseRates <- 
  readRDS(file = 'data/myvh/myvhResponseRates.rds')
myvhOverall <- 
  readRDS('data/myvh/myvhOverall.rds')
myvhTrendYear <- 
  readRDS('data/myvh/myvhTrendYear.rds')
myvhTrendDistrictYear<- 
  readRDS(file = 'data/myvh/myvhTrendDistrictYear.rds')

## TLCC Data ----
TLCCQuartiles <- 
  readRDS('data/tlcc/tlccQuartiles.rds') %>% 
  mutate(ClassificationName = recode(ClassificationName, 
                                     `General Reflection` = 'Overall Reflection', 
                                     `Staff Leadership` = "Teacher Leadership", 
                                     `New Staff Questions` = 'New Teacher Questions'))
tlccTrend <- 
  readRDS('data/tlcc/tlccTrend.rds')

# STATEWIDE TESTS & Accountability ----
## CMAS, PSAT, SAT Data ----
# icons created with https://www.flaticon.com/ and saved at Google Drive
f <- function(x) web_image(url = "https://docs.google.com/drawings/d/e/2PACX-1vSwcINF7qJB5OvEHwoDCb1zLT0IUdxpU8bJVowClxgZHuCqnJVo-9fhURCtE12eKpHjouAZKSR_hcGL/pub?w=281&h=274", height = 20)

#2022 data ----
statewideAllLevelsComplete <- 
  readRDS('data/cmas/statewideAllLevelsComplete.rds')



# cmasAllLevelsComplete <- 
#   readRDS('data/cmas/cmasAllLevelsComplete.rds')

source('data/cmas/cmasFunctions/cmasDivergingFunction.R')


#Statewide Testing Within Group Achievement Percentiles ----
withinGroupDataForPlots <- 
  readRDS('data/cmas/cmasWthinGroup.rds') 

source('data/cmas/cmasFunctions/withinGroupPlotFunctionHeatMap.R', local = TRUE)

#2022 SAT & PSAT
# satAllLevelsComplete <- 
#   readRDS('data/sat/satAllLevelsComplete.rds') %>% 
#   mutate(participationRate = participationRate/100) %>% 
#   select(-participationRateChar) %>% 
#   mutate(participationRateChar = paste0(round(participationRate*100), '%'))



## PWR Data - PWR diverged with SPF pause, due to pandemic ----
PWRData <- 
  readRDS("data/pwr/pwrData.rds")

pwr2021 <-
  readRDS(file = 'data/pwr/pwr2021DataNoSPF.rds')

## ACCESS for ELLs -----
accessComplete <-
  read_rds('data/access/accessComplete.rds')

# DISTRICT TESTS ----
## NWEA MAP Data ----
# mapBySchoolJva <- #No longer needed
#   readRDS(file = 'data/map/mapBySchoolJva.rds')

# mapBySchoolbyGrade <- 
#   readRDS(file = 'data/map/mapBySchoolbyGradeWide.rds')

mapTestPeriod <- 
  readRDS( 'data/map/mapTestPeriod.rds')

source('data/map/mapFunctions/mapAllBenchmarksByGradePlusComposite.R')
source('data/map/mapFunctions/mapGrowthPlotFunction.R')

## Map Performance LevelTrends ----
# Created by mapAllPLs.R
allMapAllLevels <- 
  readRDS(file = 'data/map/allMapAllLevels.rds') %>% 
  filter(TestedAtSchoolNumber != '0965', 
         TestedAtSchoolNumber != '4408')

allMapAllLevelsWide <- 
  readRDS(file = 'data/map/allMapAllLevelsWide.rds') %>% 
  filter(testedAtSchoolNumber != '0965', 
         testedAtSchoolNumber != '4408')

# Data for Alluvial Plots ---- 
#Only update in Spring
mapAlluvialBinaryLevels <- 
readRDS( file = 'data/map/mapAlluvialBinaryLevels.rds') %>% 
  mutate(EndYear = as.character(EndYear)) %>% 
  mutate(EndYear = recode(EndYear, '2019' = '2018-2019', 
                                   '2021' = '2020-2021', 
                                   '2022' = '2021-2022'))

# Map with Demographics -----
#created by mapAllGradesDemoswithbySchoolByGrade.R
mapDemos <- 
  readRDS(file = 'data/map/mapDemos.rds')

source('data/map/mapFunctions/mapCohortAlluvialPlotFunction.R', local = TRUE)
# source('data/map/mapFunctions/mapTrendPerformanceLevelsFunction.R', local =TRUE)
source('data/map/mapFunctions/mapTrendAllPLs.R', local =TRUE) # This files contain functions for plots and tables

# DIBELS ----
dibelsBySchoolAllGradesAndCombo <-
readRDS(file = "data/dibels/dibelsBySchoolAllGradesAndCombo.rds") %>%
  mutate(schoolN = formatC(schoolN,  big.mark = ',')) %>%
  filter(testedAtSchoolNumber != '4408') #JVA combined with JRLP

## Dibels Demos ----

# Created by wrangleDibelsTrend2022Demos.R
dibelsDemos <- 
  readRDS('data/dibels/dibelsDemos.rds') %>% 
  mutate(category = recode(category,
                           "all" = "All Students", 
                           "frlBin" = "Free or Reduced Lunch Eligible Students", 
                           "ellBin" = "Students in Enlish Langauge Learner Program", 
                           "iepBin" = "Students with Individual Education Program", 
                           "raceBin" = "Student Race or Ethnicity", 
                           "gtBin" = "Students in Gifted and Talented Program")) %>% 
  mutate(subcategoryN = as.numeric(subcategoryN)) %>% 
  mutate(subcategoryNLabel = case_when(
   subcategoryN < 16 ~ '< 16 students', 
    TRUE ~ as.character(subcategoryN)
  ))


dibelsTrend <- 
  readRDS(file = "data/dibels/dibelsTrend.rds")

acadienceYears <- unique(dibelsTrend$EndYear)

## NWEA MAP Heading----
mapTestPeriod <- mapTestPeriod %>% 
  pull()

# App Title ----
title <- tags$a(href = 'https://www.jeffcopublicschools.org/community_portal/research_surveys',#tags$a adds an anchor element
                tags$img(src = 'jeffcoTree_sm_white.png', 
                         alt= 'Jeffco Public Schools',
                         title= 'Jeffco Public Schools',
                         height = '25', 
                         width = '25')) 

# Heading Fonts ----
headingFont <- "color:#315683; font-weight:bold; font-size:14px"

subHeadingFont <- "color:#315683; font-weight:italic; font-size:12px"

captionFont <- "color:#6c7070; font-weight:italic; font-size:10px"

cultureHeading <- "color:#315683; font-weight:bold; font-size:18px"

cultureSubHeading <- "color:#315683; font-size:16px"

covidContent <- "color:#e57a3c;font-size:80%"
