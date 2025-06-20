rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)



csv_file <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Patient_Master.csv"
# T1csv <- "C:/Users/tedal/OneDrive/Desktop/T1_rawppmi_4_10_2024.csv"
# T1forsecsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/T1forse.csv"
# GMcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/CODICI/Grey_Matter_Volume.csv"
SDMTcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Symbol_Digit_Modalities_Test.csv"
JOLOcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Benton_Judgement_of_Line_Orientation.csv"
HVLTcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Hopkins_Verbal_Learning_Test_-_Revised.csv"
SFTcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Modified_Semantic_Fluency.csv"
Deprcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Non-motor_Assessments/Geriatric_Depression_Scale__Short_Version__19Apr2024.csv"
SCOPcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Non-motor_Assessments/SCOPA-AUT_19Apr2024.csv"
STAIcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Non-motor_Assessments/State-Trait_Anxiety_Inventory_19Apr2024.csv"
EPwhcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Non-motor_Assessments/Epworth_Sleepiness_Scale_19Apr2024.csv"

# Read the CSV file into a data frame
data <- read.csv(csv_file)
# GM <- read.csv(GMcsv) 
# T1 <- read.csv(T1forsecsv)
# # Extract rows where COHORT is 'sWEDD'
# T1 <- distinct(T1, Subject.ID)
SDMT <- read.csv(SDMTcsv)
JOLO <- read.csv(JOLOcsv)
HVLT <- read.csv(HVLTcsv)
SFT <- read.csv(SFTcsv)
Depr <- read.csv(Deprcsv)
GDI <- read.csv(SCOPcsv)
STAI <- read.csv(STAIcsv)
EPwh <- read.csv(EPwhcsv)


SDMTBL <- SDMT %>%
  filter(SDMT$EVENT_ID == 'BL')
SDMTBL <- SDMTBL %>%
  select(PATNO, SDMTOTAL)
JOLOBL <- JOLO %>%
  filter(JOLO$EVENT_ID == 'BL')
JOLOBL <- JOLOBL %>%
  select(PATNO, JLO_TOTCALC)
HVLTBL <- HVLT %>%
  filter(HVLT$EVENT_ID == 'BL')
HVLTBL <- HVLTBL %>%
  select(PATNO, DVT_DELAYED_RECALL, DVT_TOTAL_RECALL)
SFTBL <- SFT %>%
  filter(SFT$EVENT_ID == 'BL')
SFTBL <- SFTBL %>%
  select(PATNO, DVS_SFTANIM)
GDIBL <- GDI %>% 
  filter(GDI$EVENT_ID == 'BL')
GDIBL <- GDIBL %>%
  mutate(GDI = (GDIBL$SCAU5 + GDIBL$SCAU6 + GDIBL$SCAU7))
GDIBL$Urine <- rowSums(GDIBL[, 8:13], na.rm = TRUE)
GDIBL <- GDIBL %>%
  select(PATNO,GDI,Urine)
DeprBL <- Depr %>%
  filter(Depr$EVENT_ID == 'BL')
DeprBL$DEPR <- rowSums(DeprBL[, 6:20], na.rm = TRUE)
DeprBL <- DeprBL %>%
  select(PATNO,DEPR)
STAIBL <- STAI %>%
  filter(STAI$EVENT_ID == 'BL')
STAIBL$STAI_Y1 <- rowSums(STAIBL[, 6:25], na.rm = TRUE)
STAIBL$STAI_Y2 <- rowSums(STAIBL[, 26:45], na.rm = TRUE)
STAIBL <- STAIBL %>%
  select(PATNO,STAI_Y1,STAI_Y2)
EPwhBL <- EPwh %>%
  filter(EPwh$EVENT_ID == 'BL')
EPwhBL$SleepSC <- rowSums(EPwhBL[, 7:14], na.rm = TRUE)
EPwhBL <- EPwhBL %>%
  select(PATNO,SleepSC)




newdata <- left_join(data,SDMTBL, by = 'PATNO')
newdata <- left_join(newdata,JOLOBL, by = 'PATNO')
newdata <- left_join(newdata,HVLTBL, by = 'PATNO')
newdata <- left_join(newdata,SFTBL, by = 'PATNO')
newdata <- left_join(newdata,GDIBL, by = 'PATNO')
newdata <- left_join(newdata,DeprBL, by = 'PATNO')
newdata <- left_join(newdata,STAIBL, by = 'PATNO')
newdata <- left_join(newdata,EPwhBL, by = 'PATNO')




write.csv(newdata, "C:/Users/tedal/OneDrive/Desktop/thesis/Data/dataCog.csv", row.names = FALSE)

# SubjectwithMRI <- inner_join(data,T1, by = c('PATNO' = 'Subject.ID'))
 
# MRI_patients_DAT <- data[data$MRI_SEQ_T1_WEIGHTED == 'Yes', c("DATSCAN_CAUDATE_L", "DATSCAN_CAUDATE_R", "DATSCAN_PUTAMEN_L", "DATSCAN_PUTAMEN_R")]
# MRI_patients_AV133 <- data[data$MRI_SEQ_T1_WEIGHTED == 'Yes', c("AV133_RCAUD_S", "AV133_LCAUD_S", "AV133_RPUTANT_S", "AV133_LPUTANT_S")]
# MRI_patients_AV133_DAT <- data[data$MRI_SEQ_T1_WEIGHTED == 'Yes', c("AV133_RCAUD_S", "AV133_LCAUD_S", "AV133_RPUTANT_S", "AV133_LPUTANT_S","DATSCAN_CAUDATE_L", "DATSCAN_CAUDATE_R", "DATSCAN_PUTAMEN_L", "DATSCAN_PUTAMEN_R")]


