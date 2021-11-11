## Wrote an R function for FDR Corrections because there are many to run. First it reads in the mplus out file. 
## Then it pulls the pvalues for the various networks for that metric and performs an FDR Correction on them
## Then it saves them into an excel based on the four dimensions. 
## NOTE This script labels the networks in this order: SC, Motor, MF, FP, DM, V2, V1, VA - so if your MPlus script labels the networks in this order, then it will be consistent, if not then you will have to change the MPlus script or this script to reflect the order of your networks 

## Run this script to save the function. When it runs, FDR_correct_shen(output) will be the function name/ syntax.
## 'output' should be replaced with the MPlus out file name without the .out (ex. if the MPlus script is called  Mid10_ASP.out, output should be set to "Mid10_ASP" WITH quotation marks)

FDR_correct_shen <- function(output) {
  
  model = paste(output,".out",sep="")
  model
  modelResults <- readModels(model)
  
  ######### read standardized results ############
  standardizedResults <- modelResults[["parameters"]][["stdyx.standardized"]]
  
  # read the index of regression results (can find in "paramHeader" column ending in ".ON")
  RegI=grep(".ON", standardizedResults$paramHeader)
  
  # read the index of regression results with "GENERAL" factor (can find in "param" column)
  GenI=grep("GENERAL", standardizedResults$param)
  # read the index of regression results with "EXT" factor
  ExtI=grep("EXT", standardizedResults$param)
  # read the index of regression results with "INT" factor
  IntI=grep("INT", standardizedResults$param)
  # read the index of regression results with "ADHD" factor
  ADHDI=grep("ADHD", standardizedResults$param)
  
  # Find the index of regression results for each factor
  RegGen=intersect(RegI, GenI)
  RegExt=intersect(RegI, ExtI)
  RegInt=intersect(RegI, IntI)
  RegADHD=intersect(RegI, ADHDI)
  
  # Find the p value of regression results for each factor
  Gen_p=standardizedResults$pval[RegGen]
  Ext_p=standardizedResults$pval[RegExt]
  Int_p=standardizedResults$pval[RegInt]
  ADHD_p=standardizedResults$pval[RegADHD]
  
  # Adjust length if needed by adding -# at the end if you want to cut off some numbers
  Gen_p <- Gen_p[1:(length(Gen_p))]
  Ext_p <- Ext_p[1:(length(Ext_p))]
  Int_p <- Int_p[1:(length(Int_p))]
  ADHD_p <- ADHD_p[1:(length(ADHD_p))]
  
  # Perform fdr
  Gen_p_fdr <- p.adjust(Gen_p, method="fdr")
  Ext_p_fdr <- p.adjust(Ext_p, method="fdr")
  Int_p_fdr <- p.adjust(Int_p, method="fdr")
  Adhd_p_fdr <- p.adjust(ADHD_p, method="fdr")
  
  #Convert to data frame
  Gen_p_fdr <- as.data.frame(Gen_p_fdr)
  Ext_p_fdr <- as.data.frame(Ext_p_fdr)
  Int_p_fdr <- as.data.frame(Int_p_fdr)
  Adhd_p_fdr <- as.data.frame(Adhd_p_fdr)
  
  #Print fdr-corrected p-values to three decimal places
  Gen_p_fdr_round <- round(Gen_p_fdr,3)
  Ext_p_fdr_round <- round(Ext_p_fdr,3)
  Int_p_fdr_round <- round(Int_p_fdr,3)
  Adhd_p_fdr_round <- round(Adhd_p_fdr,3)
  
  #create dataframe to store Gen data
  Gen <- data.frame(
    Factor = "Gen",
    est = standardizedResults$est[RegGen],
    se = standardizedResults$se[RegGen],
    est_se = standardizedResults$est_se[RegGen],
    pval = standardizedResults$pval[RegGen],
    FDR_pval = Gen_p_fdr_round,
    stringsAsFactors = FALSE
  )
  Gen[1,1] = "SC"
  Gen[2,1] = "Motor"
  Gen[3,1] = "MF"
  Gen[4,1] = "FP"
  Gen[5,1] = "DM"
  Gen[6,1] = "V2"
  Gen[7,1] = "V1"
  Gen[8,1] = "VA"
  
  
  #create dataframe to store Ext data
  Ext <- data.frame(
    Factor = "Ext",
    est = standardizedResults$est[RegExt],
    se = standardizedResults$se[RegExt],
    est_se = standardizedResults$est_se[RegExt],
    pval = standardizedResults$pval[RegExt],
    FDR_pval = Ext_p_fdr_round,
    stringsAsFactors = FALSE
  )
  Ext[1,1] = "SC"
  Ext[2,1] = "Motor"
  Ext[3,1] = "MF"
  Ext[4,1] = "FP"
  Ext[5,1] = "DM"
  Ext[6,1] = "V2"
  Ext[7,1] = "V1"
  Ext[8,1] = "VA"
  
  
  #create dataframe to store Int data
  Int <- data.frame(
    Factor = "Int",
    est = standardizedResults$est[RegInt],
    se = standardizedResults$se[RegInt],
    est_se = standardizedResults$est_se[RegInt],
    pval = standardizedResults$pval[RegInt],
    FDR_pval = Int_p_fdr_round,
    stringsAsFactors = FALSE
  )
  Int[1,1] = "SC"
  Int[2,1] = "Motor"
  Int[3,1] = "MF"
  Int[4,1] = "FP"
  Int[5,1] = "DM"
  Int[6,1] = "V2"
  Int[7,1] = "V1"
  Int[8,1] = "VA"
  
  #create dataframe to store ADHD data
  Adhd <- data.frame(
    Factor = "Adhd",
    est = standardizedResults$est[RegADHD],
    se = standardizedResults$se[RegADHD],
    est_se = standardizedResults$est_se[RegADHD],
    pval = standardizedResults$pval[RegADHD],
    FDR_pval = Adhd_p_fdr_round,
    stringsAsFactors = FALSE
  )
  Adhd[1,1] = "SC"
  Adhd[2,1] = "Motor"
  Adhd[3,1] = "MF"
  Adhd[4,1] = "FP"
  Adhd[5,1] = "DM"
  Adhd[6,1] = "V2"
  Adhd[7,1] = "V1"
  Adhd[8,1] = "VA"
  
  
  
  
  
  
  
  
  ######## Saving Files #########
  
  GeneralOutput = paste("General_",output,"_FDR.csv",sep = "")
  GeneralOutput
  ExternalizingOutput = paste("Externalizing_",output,"_FDR.csv",sep = "")
  ExternalizingOutput  
  InternalizingOutput = paste("Internalizing_",output,"_FDR.csv",sep = "")
  InternalizingOutput
  ADHDOutput = paste("ADHD_",output,"_FDR.csv",sep = "")
  ADHDOutput
  
  
  write.table(Gen, GeneralOutput, quote = FALSE, sep = ",", row.names=FALSE)
  write.table(Ext, ExternalizingOutput, append = TRUE, quote = FALSE, sep = ",", row.names=FALSE, col.names = FALSE)
  write.table(Int, InternalizingOutput, append = TRUE, quote = FALSE, sep = ",", row.names=FALSE, col.names = FALSE)
  write.table(Adhd, ADHDOutput, append = TRUE, quote = FALSE, sep = ",", row.names=FALSE, col.names = FALSE)
  
  remove(list = ls())
  
}