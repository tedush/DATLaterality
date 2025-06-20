# DATLaterality
Research on Dopamine Laterality in PD


Steps to understand the code

1) Patient_Master.csv (first dataset)
  R.file = DataCog_creation --> where clinical tests at baseline level where added as a result
2) DataCog.csv (second dataset)
  R.file = MasterPatient_V08V13 --> inclusion/exclusion criteria
3) Data_RH.csv (dataset ready for Normative Model)
   R.file = NormativeModel
4) NMLateralityoutput.csv
   R.file = Z_analysis --> here the division in two the subgroups based on AI and all the longitudinal data were added
             Here the creation of the csv files that will be statistically analysed in JASP
              - NMHCPD.csv (PD vs HC)
              - NMPD.csv (EL vs NL)
              - Direction.csv (ELdx vs ELsx)
              - NM_V06_10_V13.csv (Longitudinal Analysis)

