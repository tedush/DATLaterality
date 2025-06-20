rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)
library(nortest)
library(writexl)
library(emmeans)

csv_file <- "C:/Users/tedal/OneDrive/Desktop/Paper/Analisi_Paper/NMLateralityoutput.csv"
data <- read.csv(csv_file)


data$DATSCAN_Caudate_mean <- (data$DATSCAN_CAUDATE_R + data$DATSCAN_CAUDATE_L)/2
data$DATSCAN_Putamen_mean <- (data$DATSCAN_PUTAMEN_R + data$DATSCAN_PUTAMEN_L)/2

data$AI_Caudate <- ((data$DATSCAN_CAUDATE_R - data$DATSCAN_CAUDATE_L)/(data$DATSCAN_CAUDATE_R + data$DATSCAN_CAUDATE_L))

data$AI_Caudate_abs <- abs(data$AI_Caudate)



data$Z_absvalue <- abs(data$Z_lx_putamen)

#max = 3.262

motorAI <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Patient_Master.csv"
newdata <- read.csv(motorAI)



#aggiungo TRMAI e BRDAI
# #5) TremorAssymetryIndex
newdata <- newdata %>%
  mutate(TRMRAI = (newdata$NP3PTRMR + newdata$NP3KTRMR + newdata$NP3RTARU + newdata$NP3RTARL))
newdata <- newdata %>%
  mutate(TRMLAI = (newdata$NP3PTRML + newdata$NP3KTRML + newdata$NP3RTALU + newdata$NP3RTALL))
newdata <- newdata %>%
  mutate(TRMAI = (newdata$TRMRAI - newdata$TRMLAI)/(newdata$TRMRAI + newdata$TRMLAI))
newdata <- newdata %>%
  mutate(TRMAI = round(newdata$TRMAI, digits = 2))

# 6) Brady_Rig_AssymetryIndex
newdata <- newdata %>%
  mutate(BradyRig_AIR = (newdata$NP3RIGRU + newdata$NP3RIGRL + newdata$NP3FTAPR + newdata$NP3HMOVR
                         + newdata$NP3PRSPR + newdata$NP3TTAPR + newdata$NP3LGAGR))
newdata <- newdata %>%
  mutate(BradyRig_AIL = (newdata$NP3RIGLU + newdata$NP3RIGLL + newdata$NP3FTAPL + newdata$NP3HMOVL
                         + newdata$NP3PRSPL + newdata$NP3TTAPL + newdata$NP3LGAGL))
newdata <- newdata %>%
  mutate(BradyRig_AI = (newdata$BradyRig_AIR - newdata$BradyRig_AIL)/(newdata$BradyRig_AIR + newdata$BradyRig_AIL))
newdata <- newdata %>%
  mutate(BradyRig_AI = round(newdata$BradyRig_AI, digits = 2))


motorAI <- subset(newdata, select = c(PATNO,TRMAI,BradyRig_AI))

remove(newdata)
data <- left_join(data,motorAI, by = 'PATNO')



  








data$Z_Asymmetry <- ifelse(data$Z_lx_putamen > 2.787, "EL",
                              ifelse(data$Z_lx_putamen <= -3.262, "EL", "NL"))

data$Z_Directionality <- ifelse(data$Z_lx_putamen > 2.787, "ELdx",
                           ifelse(data$Z_lx_putamen <= -3.262, "ELsx", "NL"))


data$Zabs_asymmetry <- ifelse(data$Z_absvalue > 3.262, "EL",
                              ifelse(data$Z_absvalue <= 3.262, "NL", NA))



data %>%
  group_by(Z_Directionality) %>%
  summarise(
    count_TRMAI_pos = sum(TRMAI > 0, na.rm = TRUE),
    count_TRMAI_neg = sum(TRMAI < 0, na.rm = TRUE)
  )

data %>%
  group_by(Z_Directionality) %>%
  summarise(
    count_TRMAI_pos = sum(BradyRig_AI > 0, na.rm = TRUE),
    count_TRMAI_neg = sum(BradyRig_AI < 0, na.rm = TRUE)
  )






HC <- data %>%
  filter(COHORT == 'HC')


PD <- data %>%
  filter(COHORT == 'PD')

PD %>%
  group_by(Z_Asymmetry) %>%
  count(NHY)


PD %>%
  count(Zabs_asymmetry)


PD %>%
  count(Z_Asymmetry)

HC$GENDER_BINARY <- ifelse(HC$SEX == "Male", 0, 1)

cor_test_result <- cor.test(HC$LI_Put, HC$GENDER_BINARY, method = "spearman")
print(cor_test_result)


write.csv(data,"C:/Users/tedal/OneDrive/Desktop/Paper/Analisi_Paper/NMHCPD.csv")
write.csv(PD,"C:/Users/tedal/OneDrive/Desktop/Paper/Analisi_Paper/NMPD.csv")
write.csv(HC,"C:/Users/tedal/OneDrive/Desktop/Paper/Analisi_Paper/NMHC.csv")


Direction <- PD %>%
  filter(Z_Directionality != 'NL')

write.csv(Direction,"C:/Users/tedal/OneDrive/Desktop/Paper/Analisi_Paper/Direction.csv")


################################################################################
########### V06 V10 V13
################################################################################

Laterality_data <- subset(PD, select = c(PATNO,Z_lx_putamen, Z_Asymmetry,Zabs_asymmetry , NP1BL,NP2BL,NP3BL,MCABL,SDMTBL,GDIBL,UrineBL,DeprBL,SleepSC))


rm(PD)
rm(data)




M3csv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Visit10/MDS-UPDRS_Part_III_21Apr2024.csv"
M3 <- read.csv(M3csv)

M3V10 <- M3 %>%
  filter(M3$EVENT_ID == 'V10' & M3$PAG_NAME != 'NUPDRS3A'& M3$PAG_NAME != 'NUPDR3ON' )
M3V10 <- M3V10 %>%
  group_by(PATNO) %>%
  filter(!(any(PDSTATE == "OFF") & PDSTATE == "ON")) %>%
  ungroup()

M3V10$NP3V10 <- M3V10$NP3TOT
M3V10 <- subset(M3V10, select = c(PATNO,NP3V10))



M3V6 <- M3 %>%
  filter(M3$EVENT_ID == 'V06' & M3$PAG_NAME != 'NUPDRS3A'& M3$PAG_NAME != 'NUPDR3ON' )
M3V6 <- M3V6 %>%
  group_by(PATNO) %>%
  filter(!(any(PDSTATE == "OFF") & PDSTATE == "ON")) %>%
  ungroup()

M3V6$NP3V06 <- M3V6$NP3TOT
M3V6 <- subset(M3V6, select = c(PATNO,NP3V06))



M3V13 <- M3 %>%
  filter(M3$EVENT_ID == 'V13' & M3$PAG_NAME != 'NUPDRS3A'& M3$PAG_NAME != 'NUPDR3ON' )
M3V13 <- M3V13 %>%
  group_by(PATNO) %>%
  filter(!(any(PDSTATE == "OFF") & PDSTATE == "ON")) %>%
  ungroup()

M3V13$NP3V13 <- M3V13$NP3TOT
M3V13 <- subset(M3V13, select = c(PATNO,NP3V13))




M2csv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Visit10/MDS_UPDRS_Part_II__Patient_Questionnaire_21Apr2024.csv"
M2 <- read.csv(M2csv)

M2V10 <- M2 %>%
  filter(M2$EVENT_ID == 'V10')
M2V10$NP2V10 <- M2V10$NP2PTOT
M2V10 <- subset(M2V10, select = c(PATNO, NP2V10))

M2V6 <- M2 %>%
  filter(M2$EVENT_ID == 'V06')
M2V6$NP2V06 <- M2V6$NP2PTOT
M2V6 <- subset(M2V6, select = c(PATNO, NP2V06))

M2V13 <- M2 %>%
  filter(M2$EVENT_ID == 'V13')
M2V13$NP2V13 <- M2V13$NP2PTOT
M2V13 <- subset(M2V13, select = c(PATNO, NP2V13))





M1csv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Visit10/MDS-UPDRS_Part_I_21Apr2024.csv"
M1 <- read.csv(M1csv)

M1V10 <- M1 %>%
  filter(M1$EVENT_ID == 'V10')
M1V10$NP1V10 <- M1V10$NP1RTOT
M1V10 <- subset(M1V10, select = c(PATNO, NP1V10))

M1V6 <- M1 %>%
  filter(M1$EVENT_ID == 'V06')
M1V6$NP1V06 <- M1V6$NP1RTOT
M1V6 <- subset(M1V6, select = c(PATNO, NP1V06))

M1V13 <- M1 %>%
  filter(M1$EVENT_ID == 'V13')
M1V13$NP1V13 <- M1V13$NP1RTOT
M1V13 <- subset(M1V13, select = c(PATNO, NP1V13))





MoCAcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Visit10/Montreal_Cognitive_Assessment__MoCA__21Apr2024.csv"
MoCA <- read.csv(MoCAcsv)


MoCA10 <- MoCA %>%
  filter(MoCA$EVENT_ID == 'V10')
MoCA10$MCAV10 <- MoCA10$MCATOT
MoCA10 <- subset(MoCA10, select = c(PATNO, MCAV10))

MoCA6 <- MoCA %>%
  filter(MoCA$EVENT_ID == 'V06')
MoCA6$MCAV06 <- MoCA6$MCATOT
MoCA6 <- subset(MoCA6, select = c(PATNO, MCAV06))

MoCA13 <- MoCA %>%
  filter(MoCA$EVENT_ID == 'V13')
MoCA13$MCAV13 <- MoCA13$MCATOT
MoCA13 <- subset(MoCA13, select = c(PATNO, MCAV13))






SDMTcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Symbol_Digit_Modalities_Test.csv"
SDMT <- read.csv(SDMTcsv)

SDMTV10 <- SDMT %>%
  filter(SDMT$EVENT_ID == 'V10')
SDMTV10$SDMTOTALV10 <- SDMTV10$SDMTOTAL
SDMTV10 <- subset(SDMTV10, select = c(PATNO, SDMTOTALV10))

SDMTV6 <- SDMT %>%
  filter(SDMT$EVENT_ID == 'V06')
SDMTV6$SDMTOTALV06 <- SDMTV6$SDMTOTAL
SDMTV6 <- subset(SDMTV6, select = c(PATNO, SDMTOTALV06))

SDMTV13 <- SDMT %>%
  filter(SDMT$EVENT_ID == 'V13')
SDMTV13$SDMTOTALV13 <- SDMTV13$SDMTOTAL
SDMTV13 <- subset(SDMTV13, select = c(PATNO, SDMTOTALV13))







SCOPcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Non-motor_Assessments/SCOPA-AUT_19Apr2024.csv"
GDI <- read.csv(SCOPcsv)

GDIV10 <- GDI %>% 
  filter(GDI$EVENT_ID == 'V10')
GDIV10 <- GDIV10 %>%
  mutate(GDIV10score = (GDIV10$SCAU5 + GDIV10$SCAU6 + GDIV10$SCAU7))
GDIV10$UrineV10score <- rowSums(GDIV10[, 8:13], na.rm = TRUE)
GDIV10 <- subset(GDIV10, select = c(PATNO,GDIV10score,UrineV10score))


GDIV6 <- GDI %>% 
  filter(GDI$EVENT_ID == 'V06')
GDIV6 <- GDIV6 %>%
  mutate(GDIV06score = (GDIV6$SCAU5 + GDIV6$SCAU6 + GDIV6$SCAU7))
GDIV6$UrineV06 <- rowSums(GDIV6[, 8:13], na.rm = TRUE)
GDIV6 <- subset(GDIV6, select = c(PATNO,GDIV06score,UrineV06))


GDIV13 <- GDI %>% 
  filter(GDI$EVENT_ID == 'V13')
GDIV13 <- GDIV13 %>%
  mutate(GDIV13 = (GDIV13$SCAU5 + GDIV13$SCAU6 + GDIV13$SCAU7))
GDIV13$UrineV13 <- rowSums(GDIV13[, 8:13], na.rm = TRUE)
GDIV13 <- subset(GDIV13, select = c(PATNO,GDIV13,UrineV13))



Deprcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Non-motor_Assessments/Geriatric_Depression_Scale__Short_Version__19Apr2024.csv"
Depr <- read.csv(Deprcsv)

DeprBL <- Depr %>%
  filter(Depr$EVENT_ID == 'V10')
DeprBL$DEPRV10 <- rowSums(DeprBL[, 6:20], na.rm = TRUE)
DeprV10 <- subset(DeprBL, select = c(PATNO,DEPRV10))

DeprBL <- Depr %>%
  filter(Depr$EVENT_ID == 'V06')
DeprBL$DEPRV06 <- rowSums(DeprBL[, 6:20], na.rm = TRUE)
DeprV6 <- subset(DeprBL, select = c(PATNO,DEPRV06))

DeprBL <- Depr %>%
  filter(Depr$EVENT_ID == 'V13')
DeprBL$DEPRV13 <- rowSums(DeprBL[, 6:20], na.rm = TRUE)
DeprV13 <- subset(DeprBL, select = c(PATNO,DEPRV13))





EPwhcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Non-motor_Assessments/Epworth_Sleepiness_Scale_19Apr2024.csv"
EPwh <- read.csv(EPwhcsv)

EPwhV10 <- EPwh %>%
  filter(EPwh$EVENT_ID == 'V10')
EPwhV10$SleepSCV10 <- rowSums(EPwhV10[, 7:14], na.rm = TRUE)
EPwhV10 <- subset(EPwhV10, select = c(PATNO,SleepSCV10))


EPwhV6 <- EPwh %>%
  filter(EPwh$EVENT_ID == 'V06')
EPwhV6$SleepSCV06 <- rowSums(EPwhV6[, 7:14], na.rm = TRUE)
EPwhV6 <- subset(EPwhV6, select = c(PATNO,SleepSCV06))


EPwhV13 <- EPwh %>%
  filter(EPwh$EVENT_ID == 'V13')
EPwhV13$SleepSCV13 <- rowSums(EPwhV13[, 7:14], na.rm = TRUE)
EPwhV13 <- subset(EPwhV13, select = c(PATNO,SleepSCV13))




newdata <- left_join(Laterality_data,M1V6, by = 'PATNO')
newdata <- left_join(newdata,M1V10, by = 'PATNO')
newdata <- left_join(newdata,M1V13, by = 'PATNO')

newdata <- left_join(newdata,M2V6, by = 'PATNO')
newdata <- left_join(newdata,M2V10, by = 'PATNO')
newdata <- left_join(newdata,M2V13, by = 'PATNO')

newdata <- left_join(newdata,M3V6, by = 'PATNO')
newdata <- left_join(newdata,M3V10, by = 'PATNO')
newdata <- left_join(newdata,M3V13, by = 'PATNO')

newdata <- left_join(newdata,MoCA6, by = 'PATNO')
newdata <- left_join(newdata,MoCA10, by = 'PATNO')
newdata <- left_join(newdata,MoCA13, by = 'PATNO')

newdata <- left_join(newdata,SDMTV6, by = 'PATNO')
newdata <- left_join(newdata,SDMTV10, by = 'PATNO')
newdata <- left_join(newdata,SDMTV13, by = 'PATNO')

newdata <- left_join(newdata,GDIV6, by = 'PATNO')
newdata <- left_join(newdata,GDIV10, by = 'PATNO')
newdata <- left_join(newdata,GDIV13, by = 'PATNO')

newdata <- left_join(newdata,DeprV6, by = 'PATNO')
newdata <- left_join(newdata,DeprV10, by = 'PATNO')
newdata <- left_join(newdata,DeprV13, by = 'PATNO')


duplicate_rows <- newdata[duplicated(newdata$PATNO), ]
newdata <- newdata[!duplicated(newdata$PATNO), ]

newdata <- left_join(newdata,EPwhV6, by = 'PATNO')
newdata <- left_join(newdata,EPwhV10, by = 'PATNO')
newdata <- left_join(newdata,EPwhV13, by = 'PATNO')



write.csv(newdata,"C:/Users/tedal/OneDrive/Desktop/Paper/Analisi_Paper/NM_V06_10_V13.csv")
