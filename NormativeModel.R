# Normative Modelling of PPMI DATScan
# Author: Alessio Giacomel
# Date:   1/05/2023
# Desc: The goal of the work is to estimate a normative model (NM) to 
# quantitatively assess potential DATScan abnormalities.


rm(list = ls())


{
  # General data science libraries
  require(tidyverse)
  require(readxl)
  require(lubridate)
  require(brms) # used for Bayesian modelling
  require(gtsummary) # used for tables
  require(BayesFactor) # used for Bayesian statistical tests 
  require(bayestestR) # used for Bayesian statistical tests
  require(projpred)
  require(marginaleffects)
}


# Global settings
set.seed(2023)

# load("C:/Users/tedal/OneDrive/Desktop/Subjects_data.RData")
csv_file <- "C:/Users/tedal/OneDrive/Desktop/thesis/Data/Data_RH.csv"
data <- read.csv(csv_file)

data$HTm <- data$HTCM/100
data$BMI <- data$WGTKG/(data$HTm*data$HTm)
data <- data %>%
  mutate(BMI = round(data$BMI, digits = 1))

hc_meta <- data %>%
  filter(COHORT == 'HC')

ppmi_pat <- data %>%
  filter(COHORT == 'PD')


#data 1038, hc 191, pd 847

data$SEX <- as.factor(data$SEX)
data$ETHNICITY <- as.factor(data$ETHNICITY)

data %>%
count(!is.na(SEX))
data %>%
  count(!is.na(AGE))
data %>%
  count(!is.na(BMI))
data %>%
  count(!is.na(ETHNICITY))
data %>%
  count(!is.na(LI_Put))

data <- data %>%
  filter(!is.na(SEX)) %>%
  filter(!is.na(AGE)) %>%
  filter(!is.na(BMI)) %>%
  filter(!is.na(ETHNICITY)) %>%
  filter(!is.na(LI_Put))


hc_meta <- data %>%
  filter(COHORT == 'HC')

ppmi_pat <- data %>%
  filter(COHORT == 'PD')

ppmi_pat <- ppmi_pat %>%
  filter(NHY == 1 | NHY == 2)


#data 984, hc 178, pd 793






# ------------------------------------------------------------------
# Model of laterality
putamen_lx_model <- brms::brm(
  bf(LI_Put ~ SEX + AGE + BMI + (1|ETHNICITY)),
  data = hc_meta,
  family = student(),
  warmup = 5000, iter = 30000, cores = 8, 
  save_pars = save_pars(all = TRUE),
  backend = "cmdstanr")
  control = list(adapt_delta = 0.95)
bayes_R2(putamen_lx_model)


hc_kfold_lx <- brms::kfold(putamen_lx_model, K = 5, save_fits = TRUE)
kfp <- kfold_predict(hc_kfold_lx)



#### --------------------------------------------------
# k-fold Cross validation Z-score prediction for HC


Z_kfold <- function(y, yrep){
  yrep_mean <- colMeans(yrep)
  yrep_std <- apply(yrep, 2, sd)
  
  ((y - yrep_mean) / (yrep_std))
}
#### --------------------------------------------------



Z_lx_putamen <- Z_kfold(kfp$y, kfp$yrep)


#### --------------------------------------------------

#  Z-score prediction function
Z_pred <- function(model, pred_var, orig_data){
  tmp <- brms::posterior_predict(model, newdata=pred_var, 
                                 allow_new_levels = TRUE)
  pred_m <- colMeans(tmp)
  pred_std <- apply(tmp, 2, sd)
  ((orig_data - pred_m) / pred_std)
}


# Predict Z-scores of HC using K-fold CV (data are already included in the model)



pred_variables <- ppmi_pat[,c("AGE", "SEX", "BMI","ETHNICITY")]


Z_lx_putamen_pat <- Z_pred(putamen_lx_model, 
                           pred_var = pred_variables,
                           orig_data = ppmi_pat$LI_Put)





Hc_Z_lx <- data.frame(
  PATNO = hc_meta$PATNO,
  Cohort = hc_meta$COHORT,
  Age = hc_meta$AGE,
  Sex = hc_meta$SEX,
  Z_lx_putamen = Z_lx_putamen
)


Pat_Z_lx <- data.frame(
  PATNO = ppmi_pat$PATNO,
  Cohort = ppmi_pat$COHORT,
  Age = ppmi_pat$AGE,
  Sex = ppmi_pat$SEX,
  Z_lx_putamen = Z_lx_putamen_pat
)


Laterality_Z <- rbind(Hc_Z_lx,Pat_Z_lx)
Zvalues <- subset(Laterality_Z,select = c(PATNO,Z_lx_putamen))
# max = 2.787, min = -3.262 || PD max = 9.213, min = -7.262

newdata <- rbind(hc_meta,ppmi_pat)
newdata <- left_join(newdata,Zvalues, by = 'PATNO')



write.csv(newdata,"C:/Users/tedal/OneDrive/Desktop/Paper/Analisi_Paper/NMLateralityoutput.csv")





Laterality_Z |>
  subset(Cohort %in% c("HC", "PD")) |>
  pROC::roc(Cohort, Z_lx_putamen, levels = c("HC", "PD")) 







ggviolin(data=Laterality_Z, y='Z_lx_putamen', x='Cohort',add = 'boxplot', add.params = list(fill = "white"),fill = 'Cohort', alpha=.4) +
  theme_Publication() + 
  theme(legend.position = 'none') +
  ylab("Putamen Laterality") +
  stat_compare_means(label.y = 11, label.x =1.8, method='anova')








auc_data3 <- data.frame(
  name = c("HC v. PD"),
  AUC = c(pROC::auc(d1), pROC::auc(d2), pROC::auc(d3))
)




# Create a custom facet label function that includes AUC values
facet_label_func3 <- function(variable) {
  auc_value <- auc_data3$AUC[auc_data3$name == variable]
  paste(variable, " ", round(auc_value, 3))
}


# Create the ROC curve plot with custom facet labels
pROC::ggroc(list(HC_PD = d1, HC_SWEDD = d2, PD_SWEDD = d3)) +
  facet_wrap(~name, labeller = labeller(name = facet_label_func3)) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color = "darkgrey", linetype = "dashed") +
  theme_Publication() +
  theme(legend.position = "none")
