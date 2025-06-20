rm(list = ls())

library(readxl)
library(dplyr)
library(nortest)
library(zoo)
library(multcomp)
library(tidyr)



csv_file <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/dataCog.csv"


#########################################################################
## CREAZIONE COVARIATE


newdata <- read.csv(csv_file)



#1) MeanDAT
# newdata <- data %>%
#   mutate(MeanDAT_PUT_L = (data$DATSCAN_PUTAMEN_L + data$DATSCAN_PUTAMEN_L_ANT)/2)
# newdata <- newdata %>%
#   mutate(MeanDAT_PUT_R = (data$DATSCAN_PUTAMEN_R + data$DATSCAN_PUTAMEN_R_ANT)/2)




# # 2) DATbindingAssymetryindex
# newdata <- newdata %>%
#   mutate(DATbindingassymetryindex = (newdata$MeanDAT_PUT_R - newdata$MeanDAT_PUT_L) / (newdata$MeanDAT_PUT_R + newdata$MeanDAT_PUT_L))
# 
# newdata <- newdata %>%
#   mutate(DATbindingassymetryindex = round(newdata$DATbindingassymetryindex, digits = 4))



# 3) CREO IL DOMSIDE dei motorsimptoms
# newdata <- newdata %>%
#   mutate(RMS = (newdata$NP3RIGRU + newdata$NP3RIGRL + newdata$NP3FTAPR + newdata$NP3HMOVR +
#                   newdata$NP3PRSPR + newdata$NP3TTAPR + newdata$NP3LGAGR + newdata$NP3PTRMR +
#                 newdata$NP3KTRMR + newdata$NP3RTARU))
# newdata <- newdata %>%
#   mutate(LMS = (newdata$NP3RIGLU + newdata$NP3RIGLL + newdata$NP3FTAPL + newdata$NP3HMOVL +
#                   newdata$NP3PRSPL + newdata$NP3TTAPL + newdata$NP3LGAGL + newdata$NP3PTRML +
#                   newdata$NP3KTRML + newdata$NP3RTALU))
# 
# newdata <- newdata %>% 
#   mutate(DOMSIDEvalue = (newdata$RMS - newdata$LMS)/(newdata$RMS + newdata$LMS))



# 4) Calcolo la disease duration:
# Converti le colonne di char in oggetti di tipo 'anno-mese'
newdata$DATSCAN_DATE <- as.Date(paste0("01/", newdata$DATSCAN_DATE), format = "%d/%m/%Y")
newdata$SXDT <- as.Date(paste0("01/", newdata$SXDT), format = "%d/%m/%Y")
newdata$PDDXDT <- as.Date(paste0("01/", newdata$PDDXDT), format = "%d/%m/%Y")



# Calcola la differenza tra le date

# Calculate the disease duration in years
newdata$DISEASEDURATIONsymptom <- as.numeric(difftime(newdata$DATSCAN_DATE, newdata$SXDT, units = "days")) / 365.25
newdata$DISEASEDURATIONdiagnosis <- as.numeric(difftime(newdata$DATSCAN_DATE, newdata$PDDXDT, units = "days")) / 365.25

#newdata$DiseaseDuration <- as.numeric(newdata$DATSCAN_DATE - newdata$PDDXDT)

newdata <- newdata %>%
  mutate(DISEASEDURATIONsymptom = round(newdata$DISEASEDURATIONsymptom, digits = 1))
newdata <- newdata %>%
  mutate(DISEASEDURATIONdiagnosis = round(newdata$DISEASEDURATIONdiagnosis, digits = 1))

# #5) TremorAssymetryIndex
# newdata <- newdata %>%
#   mutate(TRMRAI = (newdata$NP3PTRMR + newdata$NP3KTRMR + newdata$NP3RTARU + newdata$NP3RTARL))
# newdata <- newdata %>%
#   mutate(TRMLAI = (newdata$NP3PTRML + newdata$NP3KTRML + newdata$NP3RTALU + newdata$NP3RTALL))
# newdata <- newdata %>%
#   mutate(TRMAI = (newdata$TRMRAI - newdata$TRMLAI)/(newdata$TRMRAI + newdata$TRMLAI))
# newdata <- newdata %>%
#   mutate(TRMAI = round(newdata$TRMAI, digits = 2))
# 
# # 6) Brady_Rig_AssymetryIndex
# newdata <- newdata %>%
#   mutate(BradyRig_AIR = (newdata$NP3RIGRU + newdata$NP3RIGRL + newdata$NP3FTAPR + newdata$NP3HMOVR
#                          + newdata$NP3PRSPR + newdata$NP3TTAPR + newdata$NP3LGAGR))
# newdata <- newdata %>%
#   mutate(BradyRig_AIL = (newdata$NP3RIGLU + newdata$NP3RIGLL + newdata$NP3FTAPL + newdata$NP3HMOVL
#                          + newdata$NP3PRSPL + newdata$NP3TTAPL + newdata$NP3LGAGL))
# newdata <- newdata %>%
#   mutate(BradyRig_AI = (newdata$BradyRig_AIR - newdata$BradyRig_AIL)/(newdata$BradyRig_AIR + newdata$BradyRig_AIL))
# newdata <- newdata %>%
#   mutate(BradyRig_AI = round(newdata$BradyRig_AI, digits = 2))
# 
# # 7) WOFF 
# newdata <- newdata %>%
#   mutate(WOFF = (newdata$NP4OFF + newdata$NP4FLCTI + newdata$NP4FLCTX)/3)
# 
# newdata <- newdata %>%
#   mutate(WOFF = round(newdata$WOFF, digits = 2))


# 8) MotorSubtype

# newdata <- newdata %>%
#   mutate(TR = (newdata$NP2TRMR + newdata$NP3PTRMR + newdata$NP3PTRML + newdata$NP3KTRMR + newdata$NP3KTRML + 
#            newdata$NP3RTARU + newdata$NP3RTARL + newdata$NP3RTALU + newdata$NP3RTALL + newdata$NP3RTALJ + 
#            newdata$NP3RTCON)/11)
# newdata <- newdata %>%
#   mutate(AR = (newdata$NP3FACXP + newdata$NP3RIGN + newdata$NP3RIGRU + newdata$NP3RIGLU + newdata$NP3RIGRL + 
#                  newdata$NP3RIGLL + newdata$NP3FTAPR + newdata$NP3FTAPL + newdata$NP3PRSPR + newdata$NP3PRSPL + 
#                  newdata$NP3TTAPR + newdata$NP3TTAPL + newdata$NP3LGAGR + newdata$NP3LGAGL + newdata$NP3BRADY)/15)
# 
# newdata$Msubtype <- ifelse(newdata$TR / newdata$AR >= 0.82, "TD",
#                             ifelse(newdata$TR / newdata$AR <= 0.71, "AR", "Mixed"))
# 
# newdata$Msubtype <- ifelse(newdata$COHORT == 'HC', 'HC', newdata$Msubtype)

# # 9) AI
# 
# newdata <- newdata %>%
#   mutate(AI = case_when(
#     DATDOMSIDE == 'Left' ~ ((MeanDAT_PUT_L - MeanDAT_PUT_R) / (MeanDAT_PUT_L + MeanDAT_PUT_R) / 2) * 100,
#     DATDOMSIDE == 'Right' ~ ((MeanDAT_PUT_R - MeanDAT_PUT_L) / (MeanDAT_PUT_R + MeanDAT_PUT_L) / 2) * 100,
#     DATDOMSIDE == 'Symmetric' & MeanDAT_PUT_R >= MeanDAT_PUT_L ~ ((MeanDAT_PUT_R - MeanDAT_PUT_L) / (MeanDAT_PUT_R + MeanDAT_PUT_L) / 2) * 100,
#     DATDOMSIDE == 'Symmetric' & MeanDAT_PUT_R < MeanDAT_PUT_L ~ ((MeanDAT_PUT_L - MeanDAT_PUT_R) / (MeanDAT_PUT_L + MeanDAT_PUT_R) / 2) * 100,
#     TRUE ~ 0
#   ))
# 
# 
# newdata <- newdata %>%
#   mutate(AI = round(newdata$AI, digits = 2))




newdata$LI_Put <- (newdata$DATSCAN_PUTAMEN_R - newdata$DATSCAN_PUTAMEN_L)/(newdata$DATSCAN_PUTAMEN_R + newdata$DATSCAN_PUTAMEN_L)
newdata <- newdata %>%
  mutate(LI_Put = round(newdata$LI_Put, digits = 6))
newdata$absLI_Put <- abs(newdata$LI_Put)



newdata$PutAsymmetry <- ifelse(newdata$absLI_Put > 0.273, "EL",
                            ifelse(newdata$absLI_Put <= 0.273, "NL", NA))



#################################################################
## PULIZIA E NaN in 0

#Justifying the removal:
# - AV too few samples (27)
# - Birthdt we have age, newdiagexp
# - Remove the others with other Neuro 
# - primdiag because everyone is 1 = idiopathic, eventid everyone is SC
# - factors present at the moment at the diagnosis
# - Keep NP1TOT e NP2TOT and something about constipation
# - DBSYN solo 3 (60)

# elimino i pazienti con 'OTHERNEUROLOGICALDISORDER'
newdata <- newdata %>%
  filter(is.na(OTHNEURO) | OTHNEURO == "" )
newlan <- newdata %>%
  filter(newdata$PDTRTMNT != 1)



newdataminusAv <- subset(newdata, select = c(2:4,6:9,12,17,25,34,55:56,94,97,104:108,114,120,153:154,156:180))

newdataminusAv %>%
  group_by(COHORT)%>%
  count(PDTRTMNT)

newdataminusAv$PutLat <- ifelse(newdataminusAv$LI_Put > 0.273, "EL Dx",
                      ifelse(newdataminusAv$LI_Put < -0.247, "EL Sx", "NL"))


#####################################################################
## DIVISIONE PAZIENTI


HC <- newdataminusAv %>%
  filter(newdataminusAv$COHORT == 'HC')
# View the resulting dataframe

PD <- newdataminusAv %>%
  filter(newdataminusAv$COHORT == 'PD')

SWEDD <- newdataminusAv %>%
  filter(newdataminusAv$COHORT == 'SWEDD')


#Creo tabella per avere solo idiopathic parkinsons
idiopathicPD <- PD %>%
  filter(PD$PRIMDIAG == 1)
HC <- HC %>%
  filter(HC$PRIMDIAG != 7, HC$PRIMDIAG != 97)

datamin <- bind_rows(idiopathicPD, HC)

# PERDO = pazienti 63 che hanno una bad quality datscan
datamin <- datamin %>%
  filter(datamin$DATSCAN_QUALITY_RATING != 3)



dataminR <- datamin %>%
  filter(datamin$HANDED == 'Right')

dataminL <- datamin %>%
  filter(datamin$HANDED == 'Left')


#write.csv(datamin,"C:/Users/tedal/OneDrive/Desktop/thesis/Data/Data.csv", row.names = FALSE)



##################################################################
#### RENAME
##################################################################


rm(SWEDD)
rm(PD)
rm(newlan)
rm(datamin)
rm(HC)
rm(idiopathicPD)
rm(newdata)
rm(newdataminusAv)

newdata <- rename(dataminR, NP1BL = NP1PTOT)
newdata <- rename(newdata, NP2BL = NP2PTOT)
newdata <- rename(newdata, NP3BL = NP3TOT)
newdata <- rename(newdata, MCABL = MCATOT)
newdata <- rename(newdata, SDMTBL = SDMTOTAL)
newdata <- rename(newdata, UrineBL = Urine)
newdata <- rename(newdata, GDIBL = GDI)
newdata <- rename(newdata, DeprBL = DEPR)


newdata$BIRTHDT <- as.Date(paste0("01/", newdata$BIRTHDT), format = "%d/%m/%Y")
newdata$AGE <- as.numeric(difftime(newdata$DATSCAN_DATE, newdata$BIRTHDT, units = "days")) / 365.25
newdata <- newdata %>%
  mutate(AGE = round(newdata$AGE, digits = 1))


write.csv(newdata,"C:/Users/tedal/OneDrive/Desktop/thesis/Data/Data_RH.csv", row.names = FALSE)



################################################################################
## DATI SOLO RIGHT
################################################################################



M3csv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Visit10/MDS-UPDRS_Part_III_21Apr2024.csv"
M3 <- read.csv(M3csv)

M3V13 <- M3 %>%
  filter(M3$EVENT_ID == 'V13' & M3$PAG_NAME != 'NUPDRS3A'& M3$PAG_NAME != 'NUPDR3ON' )
M3V13 <- M3V13 %>%
  group_by(PATNO) %>%
  filter(!(any(PDSTATE == "OFF") & PDSTATE == "ON")) %>%
  ungroup()

M3V13$NP3V13 <- M3V13$NP3TOT
M3V13 <- subset(M3V13, select = c(PATNO,NP3V13))



M3V8 <- M3 %>%
  filter(M3$EVENT_ID == 'V08' & M3$PAG_NAME != 'NUPDRS3A'& M3$PAG_NAME != 'NUPDR3ON' )
M3V8 <- M3V8 %>%
  group_by(PATNO) %>%
  filter(!(any(PDSTATE == "OFF") & PDSTATE == "ON")) %>%
  ungroup()

M3V8$NP3V08 <- M3V8$NP3TOT
M3V8 <- subset(M3V8, select = c(PATNO,NP3V08))







M2csv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Visit10/MDS_UPDRS_Part_II__Patient_Questionnaire_21Apr2024.csv"
M2 <- read.csv(M2csv)

M2V13 <- M2 %>%
  filter(M2$EVENT_ID == 'V13')
M2V13$NP2V13 <- M2V13$NP2PTOT
M2V13 <- subset(M2V13, select = c(PATNO, NP2V13))

M2V8 <- M2 %>%
  filter(M2$EVENT_ID == 'V08')
M2V8$NP2V08 <- M2V8$NP2PTOT
M2V8 <- subset(M2V8, select = c(PATNO, NP2V08))






M1csv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Visit10/MDS-UPDRS_Part_I_21Apr2024.csv"
M1 <- read.csv(M1csv)

M1V13 <- M1 %>%
  filter(M1$EVENT_ID == 'V13')
M1V13$NP1V13 <- M1V13$NP1RTOT
M1V13 <- subset(M1V13, select = c(PATNO, NP1V13))

M1V8 <- M1 %>%
  filter(M1$EVENT_ID == 'V08')
M1V8$NP1V08 <- M1V8$NP1RTOT
M1V8 <- subset(M1V8, select = c(PATNO, NP1V08))






MoCAcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Visit10/Montreal_Cognitive_Assessment__MoCA__21Apr2024.csv"
MoCA <- read.csv(MoCAcsv)


MoCA13 <- MoCA %>%
  filter(MoCA$EVENT_ID == 'V13')
MoCA13$MCAV13 <- MoCA13$MCATOT
MoCA13 <- subset(MoCA13, select = c(PATNO, MCAV13))

MoCA8 <- MoCA %>%
  filter(MoCA$EVENT_ID == 'V08')
MoCA8$MCAV08 <- MoCA8$MCATOT
MoCA8 <- subset(MoCA8, select = c(PATNO, MCAV08))








SDMTcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Symbol_Digit_Modalities_Test.csv"
SDMT <- read.csv(SDMTcsv)

SDMTV13 <- SDMT %>%
  filter(SDMT$EVENT_ID == 'V13')
SDMTV13$SDMTOTALV13 <- SDMTV13$SDMTOTAL
SDMTV13 <- subset(SDMTV13, select = c(PATNO, SDMTOTALV13))

SDMTV8 <- SDMT %>%
  filter(SDMT$EVENT_ID == 'V08')
SDMTV8$SDMTOTALV08 <- SDMTV8$SDMTOTAL
SDMTV8 <- subset(SDMTV8, select = c(PATNO, SDMTOTALV08))







SCOPcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Non-motor_Assessments/SCOPA-AUT_19Apr2024.csv"
GDI <- read.csv(SCOPcsv)

GDIV13 <- GDI %>% 
  filter(GDI$EVENT_ID == 'V13')
GDIV13 <- GDIV13 %>%
  mutate(GDIV13score = (GDIV13$SCAU5 + GDIV13$SCAU6 + GDIV13$SCAU7))
#GDIBL <- GDIBL %>%
#  mutate(SCOPATOT = (GDIBL$SCAU1 + GDIBL$SCAU2 + GDIBL$SCAU3 + GDIBL$SCAU4 + GDIBL$SCAU5 + GDIBL$SCAU6 + GDIBL$SCAU7 + GDIBL$SCAU8 + GDIBL$SCAU9 + GDIBL$SCAU10 + GDIBL$SCAU11 + GDIBL$SCAU12 + GDIBL$SCAU13 + GDIBL$SCAU14 + GDIBL$SCAU15 + GDIBL$SCAU16 + GDIBL$SCAU17 + GDIBL$SCAU18 + GDIBL$SCAU19 + GDIBL$SCAU20 + GDIBL$SCAU21 + GDIBL$SCAU22 + GDIBL$SCAU23 + GDIBL$SCAU24 + GDIBL$SCAU25))
GDIV13$UrineV13score <- rowSums(GDIV13[, 8:13], na.rm = TRUE)
GDIV13 <- subset(GDIV13, select = c(PATNO,GDIV13score,UrineV13score))


GDIV8 <- GDI %>% 
  filter(GDI$EVENT_ID == 'V08')
GDIV8 <- GDIV8 %>%
  mutate(GDIV08score = (GDIV8$SCAU5 + GDIV8$SCAU6 + GDIV8$SCAU7))
GDIV8$UrineV08 <- rowSums(GDIV8[, 8:13], na.rm = TRUE)
GDIV8 <- subset(GDIV8, select = c(PATNO,GDIV08score,UrineV08))




Deprcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Non-motor_Assessments/Geriatric_Depression_Scale__Short_Version__19Apr2024.csv"
Depr <- read.csv(Deprcsv)

DeprBL <- Depr %>%
  filter(Depr$EVENT_ID == 'V13')
DeprBL$DEPRV13 <- rowSums(DeprBL[, 6:20], na.rm = TRUE)
DeprV13 <- subset(DeprBL, select = c(PATNO,DEPRV13))

DeprBL <- Depr %>%
  filter(Depr$EVENT_ID == 'V08')
DeprBL$DEPRV08 <- rowSums(DeprBL[, 6:20], na.rm = TRUE)
DeprV8 <- subset(DeprBL, select = c(PATNO,DEPRV08))






EPwhcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Non-motor_Assessments/Epworth_Sleepiness_Scale_19Apr2024.csv"
EPwh <- read.csv(EPwhcsv)

EPwhV13 <- EPwh %>%
  filter(EPwh$EVENT_ID == 'V13')
EPwhV13$SleepSCV13 <- rowSums(EPwhV13[, 7:14], na.rm = TRUE)
EPwhV13 <- subset(EPwhV13, select = c(PATNO,SleepSCV13))


EPwhV8 <- EPwh %>%
  filter(EPwh$EVENT_ID == 'V08')
EPwhV8$SleepSCV08 <- rowSums(EPwhV8[, 7:14], na.rm = TRUE)
EPwhV8 <- subset(EPwhV8, select = c(PATNO,SleepSCV08))



# JOLOcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Benton_Judgement_of_Line_Orientation.csv"
# JOLO <- read.csv(JOLOcsv)
# JOLOBL <- JOLO %>%
#   filter(JOLO$EVENT_ID == 'V13')
# JOLOBL$JOLOBLV13 <- JOLOBL$JLO_TOTCALC
# JOLOV13 <- subset(JOLOBL, select = c(PATNO, JOLOBLV13))


# SFTcsv <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Cognitive/Modified_Semantic_Fluency.csv"
# SFT <- read.csv(SFTcsv)
# SFTBL <- SFT %>%
#   filter(SFT$EVENT_ID == 'V13')
# SFTBL$SFTV13 <- SFTBL$DVS_SFTANIM
# SFTV13 <- subset(SFTBL, select = c(PATNO, SFTV13))




newdata <- left_join(dataminR,M1V8, by = 'PATNO')
newdata <- left_join(newdata,M1V13, by = 'PATNO')

newdata <- left_join(newdata,M2V8, by = 'PATNO')
newdata <- left_join(newdata,M2V13, by = 'PATNO')

newdata <- left_join(newdata,M3V8, by = 'PATNO')
newdata <- left_join(newdata,M3V13, by = 'PATNO')

newdata <- left_join(newdata,MoCA8, by = 'PATNO')
newdata <- left_join(newdata,MoCA13, by = 'PATNO')

newdata <- left_join(newdata,SDMTV8, by = 'PATNO')
newdata <- left_join(newdata,SDMTV13, by = 'PATNO')

newdata <- left_join(newdata,GDIV8, by = 'PATNO')
newdata <- left_join(newdata,GDIV13, by = 'PATNO')

newdata <- left_join(newdata,DeprV8, by = 'PATNO')
newdata <- left_join(newdata,DeprV13, by = 'PATNO')

duplicate_rows <- newdata[duplicated(newdata$PATNO), ]
newdata <- newdata[!duplicated(newdata$PATNO), ]

newdata <- left_join(newdata,EPwhV8, by = 'PATNO')
newdata <- left_join(newdata,EPwhV13, by = 'PATNO')


# newdata <- left_join(newdata,JOLOV13, by = 'PATNO')
# newdata <- left_join(newdata,SFTV13, by = 'PATNO')





# newdata <- left_join(newdata,STAIV13, by = 'PATNO')
# newdata <- left_join(newdata,DATSCANV13, by = 'PATNO')



################################################################################
newdata$DATSCAN_sumPUTAMEN <- (newdata$DATSCAN_PUTAMEN_R + newdata$DATSCAN_PUTAMEN_L + newdata$DATSCAN_PUTAMEN_R_ANT + newdata$DATSCAN_PUTAMEN_L_ANT)
newdata$DATSCAN_sumCAUDATE <- (newdata$DATSCAN_CAUDATE_R + newdata$DATSCAN_CAUDATE_L )





write.csv(newdata,"C:/Users/tedal/OneDrive/Desktop/thesis/Data/Data_Visit13.csv", row.names = FALSE)

 
 
# write.csv(Leftdata,"C:/Users/tedal/OneDrive/Desktop/thesis/Data/LeftdataM3.csv", row.names = FALSE)
# 
# ################################################################################
# ################################################################################
# 
# 
# 
# 
# 
# 
# ################################################################################
# ## DATI TUTTI
# ################################################################################
# 
# newdata <- left_join(datamin,M1V13, by = 'PATNO')
# newdata <- left_join(newdata,M2V13, by = 'PATNO')
# newdata <- left_join(newdata,M3V13, by = 'PATNO')
# 
# duplicate_rows <- newdata[duplicated(newdata$PATNO), ]
# newdata <- newdata[!duplicated(newdata$PATNO), ]
# 
# # newdata <- left_join(newdata,M4V13, by = 'PATNO')
# newdata <- left_join(newdata,MoCA13, by = 'PATNO')
# newdata <- left_join(newdata,SDMTV13, by = 'PATNO')
# 
# 
# newdata <- left_join(newdata,GDIV13, by = 'PATNO')
# newdata <- left_join(newdata,DeprV13, by = 'PATNO')
# newdata <- left_join(newdata,EPwhV13, by = 'PATNO')
# 
# newdata <- rename(newdata, NP1BL = NP1PTOT)
# newdata <- rename(newdata, NP2BL = NP2PTOT)
# newdata <- rename(newdata, NP3BL = NP3TOT)
# newdata <- rename(newdata, MCABL = MCATOT)
# newdata <- rename(newdata, SDMTBL = SDMTOTAL)
# newdata <- rename(newdata, UrineBL = Urine)
# newdata <- rename(newdata, GDIBL = GDI)
# alldata <- rename(newdata, DeprBL = DEPR)
# 
# 
# write.csv(alldata,"C:/Users/tedal/OneDrive/Desktop/thesis/Data/ALLData_Visit13.csv", row.names = FALSE)







