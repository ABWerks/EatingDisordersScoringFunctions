# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the EDDS-5
# ABWerks
# October 2021
# 
# Description:
# Function to score the Eating Disorder Diagnostic Scale (EDDS-5) survey instrument. 
#   Original SPSS version: http://www.ori.org/files/Static%20Page%20Files/EDDSCode.pdf
# Also includes the modified version from Karen Mitchell
#
# @article{stice2000development,
#   title={Development and validation of the Eating Disorder Diagnostic Scale: a brief self-report measure of anorexia, bulimia, and binge-eating disorder.},
#   author={Stice, Eric and Telch, Christy F and Rizvi, Shireen L},
#   journal={Psychological assessment},
#   volume={12},
#   number={2},
#   pages={123},
#   year={2000},
#   publisher={American Psychological Association}
# }
#
#@article{bartlett2015eating,
#   title={Eating disorders in military and veteran men and women: A systematic review},
#   author={Bartlett, Brooke A and Mitchell, Karen S},
#   journal={International Journal of Eating Disorders},
#   volume={48},
#   number={8},
#   pages={1057--1069},
#   year={2015},
#   publisher={Wiley Online Library}
# }
# 
# Arguments:
#   data: The survey
#
# Details:
#   Each question is labeled with the number preceded by EDDS
#   “Yes” is coded as 1, and “No” is coded as 0.
#   For the BMI calculations: Male is coded as 1, and Female is coded as 2.
#   Introductory item for the EDDS-5: edds_txt0 EATING BEHAVIORS  Please carefully complete all questions, 
#     choosing “Not at all” or 0 for questions below that do not apply.        
#     Over the past 3 months . . . (select the best number for each question using the scale below)
#   To improve the sensitivity of the diagnosis, we included several modificaitons to the original scoring algorithm
#
# Additional request from Qual team:   
#   1) For items that have an N/A response (i.e., 5, 7, 8, 9, 10, 11, and 12) - if missing, then code as 0. No and N/A both get coded 0.
#   2) For binge eating, if item 4 is No, and item 5 is No or N/A, then item 6 should be zero ('0').
#   3) For compensatory and sleep items (i.e., items 13-17) - if missing, then code as zero ('0'). 
#
# Gotchas:
#   This function cleans several variables that contain open text:
#   Careful that all of the possible values are coded accurately
#     EDDS19: current weight. Should be kilograms, but Uncle Sam only deals by pounds  
#     EDDS20: current height. Similar, should be centimeters but the code works in feet and inches.
#       This item was very messy in my survey and needed several cleaning steps which you can ignore
#     
# Results:
#   Returns the original survey with a bunch of new columns containing the scored 
#     EDDS-5 items, z-scores, and composite scores
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Setup
library(tidyr)
library(dplyr)
library(tableone)
library(stringr)

# table shortcut
f_tableNA <- function(...) 
  table(..., useNA = "ifany")

# Convert character height to numeric
## Takes height in feet or feet and inches and returns the height
f_heightNumeric <- function(x){
  if(length(unlist(x)) > 1){
    feet <- as.numeric(unlist(x)[1])
    inches <- as.numeric(unlist(x)[2])
    heightInInches <- feet*12+inches
  } else {
    heightInInches <- as.numeric(unlist(x))
  }
  heightInInches
}

f_scoringEDDS5 <- function(data){
  varsEDDS5 <- colnames(data)[grepl("^edds_", colnames(data))] 
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Missing
  # Recode missing to zero
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  data$edds_1[is.na(data$edds_1)] <- 0 
  data$edds_2[is.na(data$edds_2)] <- 0 
  data$edds_3[is.na(data$edds_3)] <- 0 
  data$edds_4[is.na(data$edds_4)] <- 0 
  data$edds_5[is.na(data$edds_5)] <- 0 
  data$edds_7[is.na(data$edds_7)] <- 0 
  data$edds_8[is.na(data$edds_8)] <- 0 
  data$edds_9[is.na(data$edds_9)] <- 0 
  data$edds_10[is.na(data$edds_10)] <- 0 
  data$edds_11[is.na(data$edds_11)] <- 0 
  data$edds_12[is.na(data$edds_12)] <- 0 
  data$edds_13[is.na(data$edds_13)] <- 0 
  data$edds_14[is.na(data$edds_14)] <- 0 
  data$edds_15[is.na(data$edds_15)] <- 0 
  data$edds_16[is.na(data$edds_16)] <- 0 
  data$edds_17[is.na(data$edds_17)] <- 0 
  data$edds_18[is.na(data$edds_18)] <- 0 
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # EDDS19 -> current_weight: should be coded in total kg for BMI calculation to be accurate.
  # 19. How much do you weigh?  If uncertain, please give your best estimate. __________lbs.
  # This is already cleaned up in ahead00_ETLFunction.R 
  cat("Table of current weight", "\n")
  print(table(data$edds_19, useNA = "ifany"))
  data$current_weight <- as.numeric(gsub("\\?", "", gsub("(\\d+)\\s?\\lbs?", "\\1", data$edds_19)))
  cat("Summary of current weight", "\n")
  print(summary(data$current_weight))
  
  # EDDS20 -> current_height: should be coded in total cm for the BMI calculation to be accurate.
  # 20. How tall are you?  Please specify in inches (5 ft.= 60 in.) _________ in.
  data$current_height <- gsub("feet|ft|FT|â€|â€™", "'", data$edds_20)
  data$current_height <- gsub("'\u009d", "\"", data$current_height)
  data$current_height <- gsub("inches|in", "\"", data$current_height)
  data$current_height <- gsub("'{2}", "\"", data$current_height)
  data$current_height <- gsub("5 10", "5'10", data$current_height)
  data$current_height <- gsub("\\s+", "", data$current_height)
  data$current_height <- gsub(",", "", data$current_height)
  data$current_height <- gsub("ish", "", data$current_height)
  data$current_height <- gsub("5\"4", "5'4", data$current_height)
  data$current_height <- gsub("5'04", "5'4", data$current_height)
  data$current_height <- gsub("5”4", "5'4", data$current_height)
  data$current_height <- gsub('"$', "", data$current_height)
  data$current_height <- gsub('’', "'", data$current_height)
  data$current_height <- gsub('”', '', data$current_height)
  feet <- str_split(data$current_height, '\'')

  data$current_heightNumeric <- unlist(lapply(feet, f_heightNumeric))
  data$current_heightNumeric[data$current_height == "5/2"] <- 62
  data$current_heightNumeric[data$current_height == "5/7"] <- 67
  data$current_heightNumeric[data$current_height == "5\'3\""] <- 63
  data$current_heightNumeric[data$current_height == "5\'7\""] <- 67
  data$current_heightNumeric[data$current_height == "5\"-08"] <- 68
  data$current_heightNumeric[data$current_height == "64\""] <- 64
  data$current_heightNumeric[data$current_height == "66\"c"] <- 66
  data$current_heightNumeric[data$current_height == "6'"] <- 72
  data$current_heightNumeric[data$current_height == "6-"] <- 72
  data$current_heightNumeric[data$current_height == "6-"] <- 72
  data$current_heightNumeric[data$current_height == "5'10\""] <- 70
  data$current_heightNumeric[data$current_height == "70\""] <- 70
  data$current_heightNumeric[data$current_height == "70”"] <- 70
  data$current_heightNumeric[data$current_height == '5’ 7”'] <- 67
  data$current_heightNumeric[data$current_height == '64”'] <- 64
  data$current_heightNumeric[data$current_height == '5’3”'] <- 63
  data$current_heightNumeric[data$current_height == '5’7”'] <- 67
  data$current_heightNumeric[data$current_height == '5’10”'] <- 70
  data$current_heightNumeric[data$current_height == '5’3'] <- 63
  data$current_heightNumeric[data$current_height == '5’2'] <- 62
  data$current_heightNumeric[data$current_height == '5’5'] <- 65
  data$current_heightNumeric[data$current_height == '5’11'] <- 71
  data$current_heightNumeric[data$current_height == '5”4'] <- 64
  cat('Oddball heights\n')
  print(data$current_height[is.na(data$current_heightNumeric)])
  
  data$current_heightNumeric[is.na(data$current_heightNumeric) & grepl('5.3.', data$current_height)] <- 63
  data$current_heightNumeric[is.na(data$current_heightNumeric) & grepl('5.2', data$current_height)] <- 62
  data$current_heightNumeric[is.na(data$current_heightNumeric) & grepl('5.3', data$current_height)] <- 63
  data$current_heightNumeric[is.na(data$current_heightNumeric) & grepl('5.4', data$current_height)] <- 64
  data$current_heightNumeric[is.na(data$current_heightNumeric) & grepl('5.5', data$current_height)] <- 65
  data$current_heightNumeric[is.na(data$current_heightNumeric) & grepl('5.11', data$current_height)] <- 71
  data$current_heightNumeric[is.na(data$current_heightNumeric) & grepl('5.7.', data$current_height)] <- 67
  data$current_heightNumeric[is.na(data$current_heightNumeric) & grepl('5.10.', data$current_height)] <- 70
  data$current_heightNumeric[is.na(data$current_heightNumeric) & grepl('64.', data$current_height)] <- 64
  data$current_heightNumeric[is.na(data$current_heightNumeric) & grepl('70.', data$current_height)] <- 70
  
  data$current_heightNumeric <- ifelse(
    data$current_heightNumeric == 5
    , 60
    , ifelse(
      data$current_heightNumeric > 100
      , NA
      , data$current_heightNumeric))
  cat('Current height values aka EDDS20\n')
  print(f_tableNA(data$current_heightNumeric))
  
  # BMI
  # (EDDS19)/((EDDS20/100)**2)
  # https://www.cdc.gov/nccdphp/dnpao/growthcharts/training/bmiage/page5_2.html
  # Formula: weight (lb) / [height (in)]2 x 703
  data$BMI <- 703*(data$current_weight / (data$current_heightNumeric)^2)
  cat('Current BMI values\n')
  print(summary(data$BMI))
  
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # To adjust binge eating frequency in case of errors in recording:
  # edds_4 4. During the past 3 months, have there been times when you have eaten 
  #   what other people would regard as an unusually large amount of food 
  #   (e.g., a pint of ice cream) given the circumstances?
  # edds_5 5. During the times when you ate an unusually large amount of food, 
  #   did you experience a loss of control (e.g., felt you couldn’t stop eating or 
  #   control what or how much you were eating)?
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # If (EDDS4=0 and (EDDS5=0 or N/A)) EDDS6=0.
  # edds_6 6. How many times per month on average over the past 3 months have you 
  #   eaten an unusually large amount of food and experienced a loss of control? 
  #   (write in number of times) 
  data$EDDS6 <- ifelse((!is.na(data$edds_4) & data$edds_4 == 0) & (data$edds_5 == 0 | is.na(data$edds_5)), 0, data$edds_6)
  data$EDDS6[is.na(data$EDDS6)] <- 0
  # During episodes of overeating with a loss of control, did you . . .
  # edds_7 7. Eat much more rapidly than normal?
  # If (EDDS4=0 and EDDS5=0) EDDS7=0.
  data$EDDS7 <- ifelse((!is.na(data$edds_4) & data$edds_4 == 0) & (is.na(data$edds_5) | data$edds_5 == 0), 0, data$edds_7)
  # edds_8 8. Eat until you feel uncomfortably full?
  # If (EDDS4=0 and EDDS5=0) EDDS8=0.
  data$EDDS8 <- ifelse((!is.na(data$edds_4) & data$edds_4 == 0) & (is.na(data$edds_5) | data$edds_5 == 0), 0, data$edds_8)
  # edds_9    9. Eat large amounts of food when you didn’t feel physically hungry?  
  # If (EDDS4=0 and EDDS5=0) EDDS9=0.
  data$EDDS9 <- ifelse((!is.na(data$edds_4) & data$edds_4 == 0) & (is.na(data$edds_5) | data$edds_5 == 0), 0, data$edds_9)
  # edds_10      10. Eat alone because you were embarrassed by how much you were eating?    
  # If (EDDS4=0 and EDDS5=0) EDDS10=0.
  data$EDDS10 <- ifelse((!is.na(data$edds_4) & data$edds_4 == 0) & (is.na(data$edds_5) | data$edds_5 == 0), 0, data$edds_10)
  # edds_11        11. Feel disgusted with yourself, depressed, or very guilty after overeating?      
  # If (EDDS4=0 and EDDS5=0) EDDS11=0.
  data$EDDS11 <- ifelse((!is.na(data$edds_4) & data$edds_4 == 0) & (is.na(data$edds_5) | data$edds_5 == 0), 0, data$edds_11)
  # edds_12          12. If you have episodes of uncontrollable overeating, does it make you very upset?        
  # If (EDDS4=0 and EDDS5=0) EDDS12=0.
  data$EDDS12 <- ifelse((!is.na(data$edds_4) & data$edds_4 == 0) & (is.na(data$edds_5) | data$edds_5 == 0), 0, data$edds_12)
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # For Diagnosis
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # FEATURE
  # COMPUTE FEATURE=SUM.1(EDDS7, EDDS8, EDDS9, EDDS10, EDDS11).
  data$FEATURE <- rowSums(data[, paste("EDDS", 7:11, sep = "")])

  ## Modification 3A
  ## Use the default edds responses not the recoded EDDS items
  ## SUM.1(edds_7, edds_8, edds_9, edds_10, edds_11).  
  data$FEATURE_Mod3 <- rowSums(data[, paste("edds", 7:11, sep = "_")])
  
  # BINGE
  # COMPUTE BINGE=999.
  # If EDDS4=0 BINGE=0.
  # If (EDDS4=1 and EDDS5=1 and EDDS6>=2) BINGE=1.
  # If (EDDS4=1 and EDDS5=1 and EDDS6>=4) BINGE=2.
  # RECODE BINGE (999=SYSMIS).
  data$BINGE <- ifelse(
    data$edds_4 == 1 & data$edds_5 == 1 & data$EDDS6 >= 4
    , 2
    , ifelse(
      data$edds_4 == 1 & data$edds_5 == 1 & data$EDDS6 >= 2 & data$EDDS6 < 4
      , 1
      , ifelse(
        data$edds_4 == 0
        , 0
        , 999
      )))
  data$BINGE[data$BINGE == 999] <- NA
  
  ## Modification 3
  ## Use the default edds responses not the recoded EDDS items
  # COMPUTE BINGE=999.  
  # If (EDDS4=1 and EDDS5=1) BINGE=2.  
  # If (EDDS6>=2) BINGE=2.  
  # RECODE BINGE (999=SYSMIS).  
  
  data$BINGE_Mod3 <- ifelse(
    (data$edds_4 == 1 & data$edds_5 == 1) | data$edds_6 >= 2
    , 2
    , ifelse(
      (data$edds_4 == 1 & data$edds_5 == 1) | data$edds_6 == 1
      , 1
      , ifelse(
        data$edds_4 == 0
        , 0
        , 999
      )))
  data$BINGE_Mod3[data$BINGE_Mod3 == 999] <- NA

  # PSUM
  # COMPUTE COMPSUM=999.
  # COMPUTE COMPSUM=SUM.2(EDDS13, EDDS14, EDDS15, EDDS16).
  # RECODE COMPSUM (999=SYSMIS).
  # edds_txt2 In order to prevent weight gain or counteract the effects of eating, 
  #   how many times per month on average over the past 3 months have you:
  # edds_13 13. Made yourself vomit? (write in number of times)
  # edds_14 14. Used laxatives or diuretics? (write in number of times)
  # edds_15 15. Fasted (skipped at least 2 meals in a row)? (write in number of times)
  # edds_16: Engaged in more intense exercise specifically to counteract the effects of overeating?   
  data$COMPSUM <- rowSums(data[, paste("edds", 13:16, sep = "_")])

  # Karen Mitchell
  # COMPUTE COMPSUM=SUM.2(EDDS13, EDDS14).  
  data$COMPSUM_KM <- rowSums(data[, paste("edds", 13:14, sep = "_")])
  
  # PEN
  # COMPUTE COMPEN=999.
  # If COMPSUM<1 COMPEN=0.
  # If COMPSUM>=2 COMPEN=1.
  # If COMPSUM>=4 COMPEN=2.
  # RECODE COMPEN (999=SYSMIS).
  data$COMPEN <- ifelse(
    data$COMPSUM >= 4 
    , 2 
    , ifelse(
      !is.na(data$COMPSUM) &
        data$COMPSUM >= 2 & data$COMPSUM < 4
      , 1
      , ifelse(
        !is.na(data$COMPSUM) &
          data$COMPSUM <= 1
        , 0
        , 999))
  )
  data$COMPEN[data$COMPEN == 999] <- NA
  
  # Karen Mitchell
  data$COMPEN_KM <- ifelse(
    data$COMPSUM_KM >= 4 
    , 2 
    , ifelse(
      !is.na(data$COMPSUM_KM) &
        data$COMPSUM_KM >= 2 & data$COMPSUM_KM < 4
      , 1
      , ifelse(
        !is.na(data$COMPSUM_KM) &
          data$COMPSUM_KM <= 1
        , 0
        , 999))
  )
  data$COMPEN_KM[data$COMPEN_KM == 999] <- NA

  # PURGE
  # COMPUTE PURGE=999.
  # COMPUTE PURGE=SUM(EDDS13, EDDS14).
  data$PURGE <- rowSums(data[, paste("edds", 13:14, sep = "_")])

  # WTSHAP
  # edds_3: Has your weight or shape influenced how you judge yourself as a person? 
  #     Not at all        Slightly     Moderately	 Extremely 
  # COMPUTE WTSHAP=999.  
  # If  EDDS3<6 WTSHAP=0. 
  # If  EDDS3>=2 WTSHAP=1. 
  # If  EDDS3>=4 WTSHAP=2.
  data$WTSHAP <- ifelse(
    data$edds_3 >= 4 & data$edds_3 <=6
    , 2
    , ifelse(
      data$edds_3 >= 2 & data$edds_3 < 4
      , 1
      , ifelse(
        data$edds_3 < 2
        , 0
        , 999))
  )
  data$WTSHAP[data$WTSHAP == 999] <- NA

  #LOWBMI
  # 22. What is your sex? MALE FEMALE
  # 23. What is your age? ________
  # COMPUTE LOWBMI=999. 
  # If EDDSBMI>10 LOWBMI=0. 
  # If (EDDS23>=18 and EDDSBMI<=18.5) LOWBMI=1. 
  # If (EDDS22=2 and EDDS23=17 and EDDSBMI<=18) LOWBMI=1. SCORING GUIDE
  # If (EDDS22=1 and EDDS23=17 and EDDSBMI<=18.5) LOWBMI=1. 
  # If (EDDS22=2 and EDDS23=16 and EDDSBMI<=17.75) LOWBMI=1. 
  # If (EDDS22=1 and EDDS23=16 and EDDSBMI<=18) LOWBMI=1. 
  # If (EDDS22=2 and EDDS23=15 and EDDSBMI<=17.25) LOWBMI=1. 
  # If (EDDS22=1 and EDDS23=15 and EDDSBMI<=17.5) LOWBMI=1. 
  # If (EDDS23=14 and EDDSBMI<=16.75) LOWBMI=1. 
  # If (EDDS23=13 and EDDSBMI<=16.25) LOWBMI=1. 
  # If (EDDS23=12 and EDDSBMI<=15.75) LOWBMI=1. 
  # If (EDDS23=11 and EDDSBMI<=15.25) LOWBMI=1. 
  # If (EDDS23=10 and EDDSBMI<=14.75) LOWBMI=1.
  # Age
  # summary(data$demog_age)
  # demog_22_age Age at separation:
  # summary(data$demog_22_age)
  # demog_3 3. What sex were you assigned at birth on your original birth certificate? (Check one):
  # o	Male  (1) 
  # o	Female  (2) 
  # o	Intersex/something not listed here  (3) 
  # o	Prefer not to answer  (4) 
  # If EDDSBMI>10 LOWBMI=0.
  data$LOWBMI <- 0
  data$LOWBMI[is.na(data$BMI) | is.na(data$demog_age) | is.na(data$demog_3)] <- NA
  # If (EDDS23>=18 and EDDSBMI<=18.5) LOWBMI=1. 
  data$LOWBMI <- ifelse(
    !is.na(data$demog_age) & !is.na(data$BMI) &
      data$demog_age >= 18 & data$BMI <= 18.5
    , 1
    , data$LOWBMI)

  # FEARWT 
  # edds_2: Have you had a definite fear that you might gain weight or become fat? 
  # COMPUTE FEARWT=999.  
  # If EDDS2<6 FEARWT=0.  
  # If EDDS2>=2 FEARWT=1.  
  # If EDDS2>=4 FEARWT=2.
  data$FEARWT <- ifelse(
    data$edds_2 >= 4 & data$edds_2 <=6
    , 2
    , ifelse(
      data$edds_2 >= 2 & data$edds_2 < 4
      , 1
      , ifelse(
        data$edds_2 < 2
        , 0
        , 999))
  )
  data$FEARWT[data$FEARWT == 999] <- NA

  # WTLOSS 
  # COMPUTE WTLOSS=999.  
  # If (EDDS19>(0.9*(EDDS21))) WTLOSS=0.  
  # If (EDDS19<=(0.9*(EDDS21))) WTLOSS=1.  
  # RECODE WTLOSS (999=SYSMIS).
  data$WTLOSS <- ifelse(
    !is.na(data$highest_weight) & !is.na(data$current_weight) &
      data$current_weight > data$highest_weight*0.9
    , 0
    , 1
  )
  data$WTLOSS[is.na(data$highest_weight) | is.na(data$current_weight)] <- NA

  # EDDSDX 
  # edds_1: Have you felt fat? 
  # edds_17: How many times per month on average over the past 3 months have you eaten after 
  #   awakening from sleep or eaten an unusually large amount of food after your evening meal and felt distressed by the night eating? 
  # edds_12: If you have episodes of uncontrollable overeating, does it make you very upset? 
  # COMPUTE EDDSDX=999.  
  # If EDDS1<6 EDDSDX=0.  
  # If EDDS17>=4 EDDSDX=8.  
  # If (PURGE>=4 and BINGE=0 and WTSHAP=2) EDDSDX=7.  
  # If (BINGE=1 and FEATURE>=3 and EDDS12=1 and COMPEN=0) EDDSDX=6.  
  # If (BINGE=1 and COMPEN=1 and WTSHAP=2) EDDSDX=5.  
  # If (WTLOSS=1 and FEARWT=2 and WTSHAP=2) EDDSDX=4.  
  # If (BINGE=2 and FEATURE>=3 and EDDS12=1 and COMPEN=0) EDDSDX=3.  
  # If (BINGE=2 and COMPEN=2 and WTSHAP=2) EDDSDX=2.  
  # If (LOWBMI=1 and FEARWT=2 and WTSHAP=2) EDDSDX=1.  
  # If (LOWBMI=1 and COMPEN=2 and WTSHAP=2) EDDSDX=1.  
  # If EDDS18<3 EDDSDX=0.  
  # *1=AN, 2=BN, 3=BED, 4=Atypical AN, 5=low frequency BN, 6=low frequency BED, 7=purging disorder, 8=night eating syndrome  
  data$scoreEDDSDX <- '00 Ref'
  # COMPUTE scoreEDDSDX=999.  
  data$scoreEDDSDX <- '999'
  # If EDDS1<6 scoreEDDSDX=0.
  data$scoreEDDSDX[data$edds_1<6] <- '00 Ref'
  # If EDDS17>=4 scoreEDDSDX=8.
  data$scoreEDDSDX[data$edds_17>=4] <- '08 night eating syndrome'
  # If (PURGE>=4 and BINGE=0 and WTSHAP=2) scoreEDDSDX=7.  
  data$scoreEDDSDX[!is.na(data$PURGE) & !is.na(data$BINGE) & !is.na(data$WTSHAP) & 
              data$PURGE >= 4 & data$BINGE == 0 & data$WTSHAP == 2] <- '07 purging disorder'
  # If (BINGE=1 and FEATURE>=3 and EDDS12=1 and COMPEN=0) scoreEDDSDX=6.  
  data$scoreEDDSDX[!is.na(data$FEATURE) & !is.na(data$BINGE) & !is.na(data$COMPEN) & !is.na(data$EDDS12) &
              data$FEATURE >= 3 & data$BINGE == 1 & data$COMPEN == 0 & data$EDDS12 == 1] <- '06 low frequency BED'
  # If (BINGE=1 and COMPEN=1 and WTSHAP=2) scoreEDDSDX=5.  
  data$scoreEDDSDX[!is.na(data$WTSHAP) & !is.na(data$BINGE) & !is.na(data$COMPEN) &
              data$WTSHAP == 2 & data$BINGE == 1 & data$COMPEN == 1] <- '05 low frequency BN'
  # If (WTLOSS=1 and FEARWT=2 and WTSHAP=2) scoreEDDSDX=4.  
  data$scoreEDDSDX[!is.na(data$WTLOSS) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
              data$WTLOSS == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '04 Atypical AN'
  # If (BINGE=2 and FEATURE>=3 and EDDS12=1 and COMPEN=0) scoreEDDSDX=3.  
  data$scoreEDDSDX[!is.na(data$BINGE) & !is.na(data$FEATURE) & !is.na(data$COMPEN) & !is.na(data$EDDS12) &
              data$BINGE == 2 & data$FEATURE >= 3 & data$COMPEN == 0 & data$EDDS12 == 1] <- '03 BED'
  # If (BINGE=2 and COMPEN=2 and WTSHAP=2) scoreEDDSDX=2. 
  data$scoreEDDSDX[!is.na(data$WTSHAP) & !is.na(data$BINGE) & !is.na(data$COMPEN) &
              data$WTSHAP == 2 & data$BINGE == 2 & data$COMPEN == 2] <- '02 BN'
  # If (LOWBMI=1 and FEARWT=2 and WTSHAP=2) scoreEDDSDX=1. 
  data$scoreEDDSDX[!is.na(data$LOWBMI) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
              data$LOWBMI == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '01 AN'
  # If (LOWBMI=1 and COMPEN=2 and WTSHAP=2) scoreEDDSDX=1.  
  data$scoreEDDSDX[!is.na(data$LOWBMI) & !is.na(data$COMPEN) & !is.na(data$WTSHAP) &
              data$LOWBMI == 1 & data$COMPEN == 2 & data$WTSHAP == 2] <- '01 AN'
  # If EDDS18<3 scoreEDDSDX=0.
  data$scoreEDDSDX[data$edds_18<3] <- '00 Ref'
  
  cat('EDDSDX composite factor default method:\n')
  print(f_tableNA(data$scoreEDDSDX))
  
  # KM version
  data$scoreEDDSDX_KM <- '00 Ref'
  # COMPUTE scoreEDDSDX_KM=999.  
  data$scoreEDDSDX_KM <- '999'
  # If EDDS1<6 scoreEDDSDX_KM=0.
  data$scoreEDDSDX_KM[data$edds_1<6] <- '00 Ref'
  # If EDDS17>=4 scoreEDDSDX_KM=8.
  data$scoreEDDSDX_KM[data$edds_17>=4] <- '08 night eating syndrome'
  # If (PURGE>=4 and BINGE=0 and WTSHAP=2) scoreEDDSDX_KM=7.  
  data$scoreEDDSDX_KM[!is.na(data$PURGE) & !is.na(data$BINGE) & !is.na(data$WTSHAP) & 
                        data$PURGE >= 4 & data$BINGE == 0 & data$WTSHAP == 2] <- '07 purging disorder'
  # If (BINGE=1 and FEATURE>=3 and EDDS12=1 and COMPEN_KM=0) scoreEDDSDX_KM=6.  
  data$scoreEDDSDX_KM[!is.na(data$FEATURE) & !is.na(data$BINGE) & !is.na(data$COMPEN_KM) & !is.na(data$EDDS12) &
                        data$FEATURE >= 3 & data$BINGE == 1 & data$COMPEN_KM == 0 & data$EDDS12 == 1] <- '06 low frequency BED'
  # If (BINGE=1 and COMPEN_KM=1 and WTSHAP=2) scoreEDDSDX_KM=5.  
  data$scoreEDDSDX_KM[!is.na(data$WTSHAP) & !is.na(data$BINGE) & !is.na(data$COMPEN_KM) &
                        data$WTSHAP == 2 & data$BINGE == 1 & data$COMPEN_KM == 1] <- '05 low frequency BN'
  # If (WTLOSS=1 and FEARWT=2 and WTSHAP=2) scoreEDDSDX_KM=4.  
  data$scoreEDDSDX_KM[!is.na(data$WTLOSS) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
                        data$WTLOSS == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '04 Atypical AN'
  # If (BINGE=2 and FEATURE>=3 and EDDS12=1 and COMPEN_KM=0) scoreEDDSDX_KM=3.  
  data$scoreEDDSDX_KM[!is.na(data$BINGE) & !is.na(data$FEATURE) & !is.na(data$COMPEN_KM) & !is.na(data$EDDS12) &
                        data$BINGE == 2 & data$FEATURE >= 3 & data$COMPEN_KM == 0 & data$EDDS12 == 1] <- '03 BED'
  # If (BINGE=2 and COMPEN_KM=2 and WTSHAP=2) scoreEDDSDX_KM=2. 
  data$scoreEDDSDX_KM[!is.na(data$WTSHAP) & !is.na(data$BINGE) & !is.na(data$COMPEN_KM) &
                        data$WTSHAP == 2 & data$BINGE == 2 & data$COMPEN_KM == 2] <- '02 BN'
  # If (LOWBMI=1 and FEARWT=2 and WTSHAP=2) scoreEDDSDX_KM=1. 
  data$scoreEDDSDX_KM[!is.na(data$LOWBMI) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
                        data$LOWBMI == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '01 AN'
  # If (LOWBMI=1 and COMPEN_KM=2 and WTSHAP=2) scoreEDDSDX_KM=1.  
  data$scoreEDDSDX_KM[!is.na(data$LOWBMI) & !is.na(data$COMPEN_KM) & !is.na(data$WTSHAP) &
                        data$LOWBMI == 1 & data$COMPEN_KM == 2 & data$WTSHAP == 2] <- '01 AN'
  # If EDDS18<3 scoreEDDSDX_KM=0.
  data$scoreEDDSDX_KM[data$edds_18<3] <- '00 Ref'
  
  cat('EDDSDX_KM composite factor\n')
  print(f_tableNA(data$scoreEDDSDX_KM))
  
  # Modification 3 version
  data$scoreEDDSDX_Mod3A <- '00 Ref'
  data$scoreEDDSDX_Mod3B <- '00 Ref'
  # COMPUTE scoreEDDSDX_Mod3=999.  
  data$scoreEDDSDX_Mod3A <- '999'
  data$scoreEDDSDX_Mod3B <- '999'
  # If EDDS1<6 scoreEDDSDX_Mod3=0.
  data$scoreEDDSDX_Mod3A[data$edds_1<6] <- '00 Ref'
  data$scoreEDDSDX_Mod3B[data$edds_1<6] <- '00 Ref'
  # If EDDS17>=4 scoreEDDSDX_Mod3=8.
  data$scoreEDDSDX_Mod3A[data$edds_17>=4] <- '08 night eating syndrome'
  data$scoreEDDSDX_Mod3B[data$edds_17>=4] <- '08 night eating syndrome'
  # If (PURGE>=4 and BINGE_Mod3=0 and WTSHAP=2) scoreEDDSDX_Mod3=7.  
  data$scoreEDDSDX_Mod3A[!is.na(data$PURGE) & !is.na(data$BINGE_Mod3) & !is.na(data$WTSHAP) & 
              data$PURGE >= 4 & data$BINGE_Mod3 == 0 & data$WTSHAP == 2] <- '07 purging disorder'
  data$scoreEDDSDX_Mod3B[!is.na(data$PURGE) & !is.na(data$BINGE_Mod3) & !is.na(data$WTSHAP) & 
              data$PURGE >= 4 & data$BINGE_Mod3 == 0 & data$WTSHAP == 2] <- '07 purging disorder'
  # If (BINGE_Mod3=1 and FEATURE_Mod3>=3 and EDDS12=1 and COMPEN=0) scoreEDDSDX_Mod3=6.  
  data$scoreEDDSDX_Mod3A[!is.na(data$FEATURE_Mod3) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN) & !is.na(data$edds_12) &
              data$FEATURE_Mod3 >= 3 & data$BINGE_Mod3 == 1 & data$COMPEN == 0 & data$edds_12 == 1] <- '06 low frequency BED'
  data$scoreEDDSDX_Mod3B[!is.na(data$FEATURE_Mod3) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN) & !is.na(data$edds_12) &
              data$FEATURE_Mod3 >= 3 & data$BINGE_Mod3 == 1 & data$COMPEN == 0 & data$edds_12 == 1] <- '06 low frequency BED'
  # If (BINGE_Mod3=1 and COMPEN=1 and WTSHAP=2) scoreEDDSDX_Mod3=5.  
  data$scoreEDDSDX_Mod3A[!is.na(data$WTSHAP) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN) &
              data$WTSHAP == 2 & data$BINGE_Mod3 == 1 & data$COMPEN == 1] <- '05 low frequency BN'
  data$scoreEDDSDX_Mod3B[!is.na(data$WTSHAP) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN) &
              data$WTSHAP == 2 & data$BINGE_Mod3 == 1 & data$COMPEN == 1] <- '05 low frequency BN'
  # If (WTLOSS=1 and FEARWT=2 and WTSHAP=2) scoreEDDSDX_Mod3=4.  
  data$scoreEDDSDX_Mod3A[!is.na(data$WTLOSS) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
              data$WTLOSS == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '04 Atypical AN'
  data$scoreEDDSDX_Mod3B[!is.na(data$WTLOSS) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
              data$WTLOSS == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '04 Atypical AN'
  
  ## If (BINGE_Mod3=2 and FEATURE_Mod3>=3 and EDDS12=1 and COMPEN=0) scoreEDDSDX_Mod3=3.  
  data$scoreEDDSDX_Mod3A[!is.na(data$BINGE_Mod3) & !is.na(data$FEATURE_Mod3) & !is.na(data$COMPEN) & !is.na(data$edds_12) &
              data$BINGE_Mod3 == 2 & data$FEATURE_Mod3 >= 3 & data$COMPEN == 0 & data$edds_12 == 1] <- '03 BED'
  ## If (BINGE_Mod3=2) EDDSDX=3.  
  data$scoreEDDSDX_Mod3B[!is.na(data$BINGE_Mod3) & data$BINGE_Mod3 == 2] <- '03 BED'
  
  # If (BINGE_Mod3=2 and COMPEN=2 and WTSHAP=2) scoreEDDSDX_Mod3=2. 
  data$scoreEDDSDX_Mod3A[!is.na(data$WTSHAP) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN) &
              data$WTSHAP == 2 & data$BINGE_Mod3 == 2 & data$COMPEN == 2] <- '02 BN'
  data$scoreEDDSDX_Mod3B[!is.na(data$WTSHAP) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN) &
              data$WTSHAP == 2 & data$BINGE_Mod3 == 2 & data$COMPEN == 2] <- '02 BN'
  # If (LOWBMI=1 and FEARWT=2 and WTSHAP=2) scoreEDDSDX_Mod3=1. 
  data$scoreEDDSDX_Mod3A[!is.na(data$LOWBMI) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
              data$LOWBMI == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '01 AN'
  data$scoreEDDSDX_Mod3B[!is.na(data$LOWBMI) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
              data$LOWBMI == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '01 AN'
  # If (LOWBMI=1 and COMPEN=2 and WTSHAP=2) scoreEDDSDX_Mod3=1.  
  data$scoreEDDSDX_Mod3A[!is.na(data$LOWBMI) & !is.na(data$COMPEN) & !is.na(data$WTSHAP) &
              data$LOWBMI == 1 & data$COMPEN == 2 & data$WTSHAP == 2] <- '01 AN'
  data$scoreEDDSDX_Mod3B[!is.na(data$LOWBMI) & !is.na(data$COMPEN) & !is.na(data$WTSHAP) &
              data$LOWBMI == 1 & data$COMPEN == 2 & data$WTSHAP == 2] <- '01 AN'
  # If EDDS18<3 scoreEDDSDX_Mod3=0.
  data$scoreEDDSDX_Mod3A[data$edds_18<3] <- '00 Ref'
  data$scoreEDDSDX_Mod3B[data$edds_18<3] <- '00 Ref'
  
  cat('EDDSDX_Mod3A composite factor\n')
  print(f_tableNA(data$scoreEDDSDX_Mod3A))
  cat('EDDSDX_Mod3B composite factor\n')
  print(f_tableNA(data$scoreEDDSDX_Mod3B))
  
  # Modification 3 version
  data$scoreEDDSDX_Mod3A_KM <- '00 Ref'
  data$scoreEDDSDX_Mod3B_KM <- '00 Ref'
  # COMPUTE scoreEDDSDX_Mod3=999.  
  data$scoreEDDSDX_Mod3A_KM <- '999'
  data$scoreEDDSDX_Mod3B_KM <- '999'
  # If EDDS1<6 scoreEDDSDX_Mod3=0.
  data$scoreEDDSDX_Mod3A_KM[data$edds_1<6] <- '00 Ref'
  data$scoreEDDSDX_Mod3B_KM[data$edds_1<6] <- '00 Ref'
  # If EDDS17>=4 scoreEDDSDX_Mod3=8.
  data$scoreEDDSDX_Mod3A_KM[data$edds_17>=4] <- '08 night eating syndrome'
  data$scoreEDDSDX_Mod3B_KM[data$edds_17>=4] <- '08 night eating syndrome'
  # If (PURGE>=4 and BINGE_Mod3=0 and WTSHAP=2) scoreEDDSDX_Mod3=7.  
  data$scoreEDDSDX_Mod3A_KM[!is.na(data$PURGE) & !is.na(data$BINGE_Mod3) & !is.na(data$WTSHAP) & 
                           data$PURGE >= 4 & data$BINGE_Mod3 == 0 & data$WTSHAP == 2] <- '07 purging disorder'
  data$scoreEDDSDX_Mod3B_KM[!is.na(data$PURGE) & !is.na(data$BINGE_Mod3) & !is.na(data$WTSHAP) & 
                           data$PURGE >= 4 & data$BINGE_Mod3 == 0 & data$WTSHAP == 2] <- '07 purging disorder'
  # If (BINGE_Mod3=1 and FEATURE_Mod3>=3 and EDDS12=1 and COMPEN_KM=0) scoreEDDSDX_Mod3=6.  
  data$scoreEDDSDX_Mod3A_KM[!is.na(data$FEATURE_Mod3) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN_KM) & !is.na(data$edds_12) &
                           data$FEATURE_Mod3 >= 3 & data$BINGE_Mod3 == 1 & data$COMPEN_KM == 0 & data$edds_12 == 1] <- '06 low frequency BED'
  data$scoreEDDSDX_Mod3B_KM[!is.na(data$FEATURE_Mod3) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN_KM) & !is.na(data$edds_12) &
                           data$FEATURE_Mod3 >= 3 & data$BINGE_Mod3 == 1 & data$COMPEN_KM == 0 & data$edds_12 == 1] <- '06 low frequency BED'
  # If (BINGE_Mod3=1 and COMPEN_KM=1 and WTSHAP=2) scoreEDDSDX_Mod3=5.  
  data$scoreEDDSDX_Mod3A_KM[!is.na(data$WTSHAP) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN_KM) &
                           data$WTSHAP == 2 & data$BINGE_Mod3 == 1 & data$COMPEN_KM == 1] <- '05 low frequency BN'
  data$scoreEDDSDX_Mod3B_KM[!is.na(data$WTSHAP) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN_KM) &
                           data$WTSHAP == 2 & data$BINGE_Mod3 == 1 & data$COMPEN_KM == 1] <- '05 low frequency BN'
  # If (WTLOSS=1 and FEARWT=2 and WTSHAP=2) scoreEDDSDX_Mod3=4.  
  data$scoreEDDSDX_Mod3A_KM[!is.na(data$WTLOSS) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
                           data$WTLOSS == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '04 Atypical AN'
  data$scoreEDDSDX_Mod3B_KM[!is.na(data$WTLOSS) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
                           data$WTLOSS == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '04 Atypical AN'
  
  ## If (BINGE_Mod3=2 and FEATURE_Mod3>=3 and EDDS12=1 and COMPEN_KM=0) scoreEDDSDX_Mod3=3.  
  data$scoreEDDSDX_Mod3A_KM[!is.na(data$BINGE_Mod3) & !is.na(data$FEATURE_Mod3) & !is.na(data$COMPEN_KM) & !is.na(data$edds_12) &
                           data$BINGE_Mod3 == 2 & data$FEATURE_Mod3 >= 3 & data$COMPEN_KM == 0 & data$edds_12 == 1] <- '03 BED'
  ## If (BINGE_Mod3=2) EDDSDX=3.  
  data$scoreEDDSDX_Mod3B_KM[!is.na(data$BINGE_Mod3) & data$BINGE_Mod3 == 2] <- '03 BED'
  
  # If (BINGE_Mod3=2 and COMPEN_KM=2 and WTSHAP=2) scoreEDDSDX_Mod3=2. 
  data$scoreEDDSDX_Mod3A_KM[!is.na(data$WTSHAP) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN_KM) &
                           data$WTSHAP == 2 & data$BINGE_Mod3 == 2 & data$COMPEN_KM == 2] <- '02 BN'
  data$scoreEDDSDX_Mod3B_KM[!is.na(data$WTSHAP) & !is.na(data$BINGE_Mod3) & !is.na(data$COMPEN_KM) &
                           data$WTSHAP == 2 & data$BINGE_Mod3 == 2 & data$COMPEN_KM == 2] <- '02 BN'
  # If (LOWBMI=1 and FEARWT=2 and WTSHAP=2) scoreEDDSDX_Mod3=1. 
  data$scoreEDDSDX_Mod3A_KM[!is.na(data$LOWBMI) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
                           data$LOWBMI == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '01 AN'
  data$scoreEDDSDX_Mod3B_KM[!is.na(data$LOWBMI) & !is.na(data$FEARWT) & !is.na(data$WTSHAP) &
                           data$LOWBMI == 1 & data$FEARWT == 2 & data$WTSHAP == 2] <- '01 AN'
  # If (LOWBMI=1 and COMPEN_KM=2 and WTSHAP=2) scoreEDDSDX_Mod3=1.  
  data$scoreEDDSDX_Mod3A_KM[!is.na(data$LOWBMI) & !is.na(data$COMPEN_KM) & !is.na(data$WTSHAP) &
                           data$LOWBMI == 1 & data$COMPEN_KM == 2 & data$WTSHAP == 2] <- '01 AN'
  data$scoreEDDSDX_Mod3B_KM[!is.na(data$LOWBMI) & !is.na(data$COMPEN_KM) & !is.na(data$WTSHAP) &
                           data$LOWBMI == 1 & data$COMPEN_KM == 2 & data$WTSHAP == 2] <- '01 AN'
  # If EDDS18<3 scoreEDDSDX_Mod3=0.
  data$scoreEDDSDX_Mod3A_KM[data$edds_18<3] <- '00 Ref'
  data$scoreEDDSDX_Mod3B_KM[data$edds_18<3] <- '00 Ref'
  
  cat('EDDSDX_Mod3A_KM composite factor\n')
  print(f_tableNA(data$scoreEDDSDX_Mod3A_KM))
  cat('EDDSDX_Mod3B_KM composite factor\n')
  print(f_tableNA(data$scoreEDDSDX_Mod3B_KM))
  
  # Any EDDDX
  ## Original
  data$scoreAnyEDDS5 <- ifelse(!data$scoreEDDSDX%in%c('00 Ref', '999'), 1, 0)
  data$scoreAnyEDDS5[data$scoreEDDSDX == '999'] <- NA
  cat('EDDSDX with AnyEDDDX composite factors\n')
  print(f_tableNA(data$scoreAnyEDDS5))
  print(addmargins(f_tableNA(scoreEDDSDX = data$scoreEDDSDX, scoreAnyEDDS5 = data$scoreAnyEDDS5),1))
  ## KM
  data$scoreAnyEDDS5_KM <- ifelse(!data$scoreEDDSDX_KM%in%c('00 Ref', '999'), 1, 0)
  data$scoreAnyEDDS5_KM[data$scoreEDDSDX_KM == '999'] <- NA
  cat('scoreEDDSDX with AnyEDDDX_KM composite factors\n')
  print(f_tableNA(data$scoreAnyEDDS5_KM))
  print(addmargins(f_tableNA(scoreEDDSDX = data$scoreEDDSDX, scoreAnyEDDS5_KM = data$scoreAnyEDDS5_KM),1))
  ## Mod 3
  ## 3. If edds_4>1 AND edds_5>1 OR If edds_6 >/=2, then scoreAnyEDDS5 = 1
  data$scoreAnyEDDS5Mod03 <- data$scoreAnyEDDS5
  data$scoreAnyEDDS5Mod03[data$EDDS6 > 1 | (data$edds_5 == 1 & data$edds_4 == 1)] <- 1
  cat('scoreEDDSDX with AnyEDDDXMod03 composite factors\n')
  print(f_tableNA(data$scoreAnyEDDS5Mod03))
  print(addmargins(f_tableNA(scoreEDDSDX = data$scoreEDDSDX, scoreAnyEDDS5Mod03 = data$scoreAnyEDDS5Mod03),1))
  ## Mod 3A
  data$scoreAnyEDDS5_Mod3A <- ifelse(!data$scoreEDDSDX_Mod3A%in%c('00 Ref', '999'), 1, 0)
  data$scoreAnyEDDS5_Mod3A[data$scoreEDDSDX_Mod3A == '999'] <- NA
  cat('scoreEDDSDX with AnyEDDDX_Mod3A composite factors\n')
  print(f_tableNA(data$scoreAnyEDDS5_Mod3A))
  print(addmargins(f_tableNA(scoreEDDSDX = data$scoreEDDSDX, scoreAnyEDDS5_Mod3A = data$scoreAnyEDDS5_Mod3A),1))
  ## Mod 3B
  data$scoreAnyEDDS5_Mod3B <- ifelse(!data$scoreEDDSDX_Mod3B%in%c('00 Ref', '999'), 1, 0)
  data$scoreAnyEDDS5_Mod3B[data$scoreEDDSDX_Mod3B == '999'] <- NA
  cat('scoreEDDSDX with AnyEDDDX_Mod3B composite factors\n')
  print(f_tableNA(data$scoreAnyEDDS5_Mod3B))
  print(addmargins(f_tableNA(scoreEDDSDX = data$scoreEDDSDX, scoreAnyEDDS5_Mod3B = data$scoreAnyEDDS5_Mod3B),1))
  ## Mod 3A_KM
  data$scoreAnyEDDS5_Mod3A_KM <- ifelse(!data$scoreEDDSDX_Mod3A_KM%in%c('00 Ref', '999'), 1, 0)
  data$scoreAnyEDDS5_Mod3A_KM[data$scoreEDDSDX_Mod3A_KM == '999'] <- NA
  cat('scoreEDDSDX with AnyEDDDX_Mod3A composite factors\n')
  print(f_tableNA(data$scoreAnyEDDS5_Mod3A_KM))
  print(addmargins(f_tableNA(scoreEDDSDX = data$scoreEDDSDX, scoreAnyEDDS5_Mod3A_KM = data$scoreAnyEDDS5_Mod3A_KM),1))
  ## Mod 3B_KM
  data$scoreAnyEDDS5_Mod3B_KM <- ifelse(!data$scoreEDDSDX_Mod3B_KM%in%c('00 Ref', '999'), 1, 0)
  data$scoreAnyEDDS5_Mod3B_KM[data$scoreEDDSDX_Mod3B_KM == '999'] <- NA
  cat('scoreEDDSDX with AnyEDDDX_Mod3B composite factors\n')
  print(f_tableNA(data$scoreAnyEDDS5_Mod3B_KM))
  print(addmargins(f_tableNA(scoreEDDSDX = data$scoreEDDSDX, scoreAnyEDDS5_Mod3B_KM = data$scoreAnyEDDS5_Mod3B_KM),1))
  
  # Symptom Composite Scores  
  # For symptom composite scores, you can use the raw scores unless items are so 
  # positively skewed that the alpha is not acceptable. In those instances, 
  # we recommend taking the z-score of items before averaging.  
  # 
  newVarsEDDS5 <-c( paste0("edds_", 1:5), paste0("EDDS", 6:12), paste0("edds_", 13:18) )

  data <- data %>% 
    mutate(
      zEDDS1 = as.numeric(scale(data$edds_1))
      , zEDDS2 = as.numeric(scale(data$edds_2))
      , zEDDS3 = as.numeric(scale(data$edds_3))
      , zEDDS4 = as.numeric(scale(data$edds_4))
      , zEDDS5 = as.numeric(scale(data$edds_5))
      , zEDDS6 = as.numeric(scale(data$EDDS6))
      , zEDDS7 = as.numeric(scale(data$EDDS7))
      , zEDDS8 = as.numeric(scale(data$EDDS8))
      , zEDDS9 = as.numeric(scale(data$EDDS9))
      , zEDDS10 = as.numeric(scale(data$EDDS10))
      , zEDDS11 = as.numeric(scale(data$EDDS11))
      , zEDDS12 = as.numeric(scale(data$EDDS12))
      , zEDDS13 = as.numeric(scale(data$edds_13))
      , zEDDS14 = as.numeric(scale(data$edds_14))
      , zEDDS15 = as.numeric(scale(data$edds_15))
      , zEDDS16 = as.numeric(scale(data$edds_16))
      , zEDDS17 = as.numeric(scale(data$edds_17))
      , zEDDS18 = as.numeric(scale(data$edds_18))
    )

  # DSM-5 symptom composite via average of z-scores.  
  # EDSYMZ 
  # COMPUTE EDSYMZ=999.  
  # COMPUTE EDSYMZ=Mean(zEDDS1 to zEDDS17).  
  data$EDSYMZ <- rowMeans(data[, grepl("^zE", colnames(data))])

  # DSM-5 symptom composite via sum of raw.  
  # EDSYMRAW 
  # COMPUTE EDSYMRAW=999.  
  # COMPUTE EDSYMRAW=SUM(EDDS1 to EDDS17).  
  data$EDSYMRAW <- rowSums(data[, newVarsEDDS5])

  # Create factors for display
  data$EDDS1 <- factor(data$edds_1, labels = c("0 Not At All", "1", "2 Slightly", "3", "4 Moderately", '5', "6 Extremely"))
  data$EDDS2 <- factor(data$edds_2, labels = c("0 Not At All", "1", "2 Slightly", "3", "4 Moderately", '5', "6 Extremely"))
  data$EDDS3 <- factor(data$edds_3, labels = c("0 Not At All", "1", "2 Slightly", "3", "4 Moderately", '5', "6 Extremely"))
  data$EDDS4 <- factor(data$edds_4, labels = c("0 No", "1 Yes"))
  data$EDDS5 <- factor(data$edds_5, labels = c("0 No", "1 Yes"))
  
  attr(data$EDDS1, 'label') <- 'edds1 Have you felt fat?'
  attr(data$EDDS2, 'label') <- 'edds2 Have you had a definite fear that you might gain weight or become fat?'
  attr(data$EDDS3, 'label') <- 'edds3 Has your weight or shape influenced how you judge yourself as a person?'
  attr(data$EDDS4, 'label') <- 'edds4 During the past 3 months, have there been times when you have eaten what other people would regard as an unusually large amount of food (e.g., a pint of ice cream) given the circumstances?'
  attr(data$EDDS5, 'label') <- 'edds5 During the times when you ate an unusually large amount of food, did you experience a loss of control (e.g., felt you couldn’t stop eating or control what or how much you were eating)?'
  
  data$EDDS_6 <- factor(data$EDDS6)
  attr(data$EDDS6, 'label') <- 'EDDS6 How many times per month on average over the past 3 months have you eaten an unusually large amount of food and experienced a loss of control? (write in number of times)'
  attr(data$EDDS_6, 'label') <- 'EDDS6 How many times per month on average over the past 3 months have you eaten an unusually large amount of food and experienced a loss of control? (write in number of times)'
  data$EDDS7 <- factor(data$EDDS7, labels = c("0 No", "1 Yes"))
  attr(data$EDDS7, 'label') <- 'EDDS7 During episodes of overeating with a loss of control, did you: Eat much more rapidly than normal?'
  # edds8 8. Eat until you feel uncomfortably full?
  # If (EDDS4=0 and EDDS5=0) EDDS8=0.
  data$EDDS8 <- factor(data$EDDS8, labels = c("0 No", "1 Yes"))
  attr(data$EDDS8, 'label') <- 'EDDS8 During episodes of overeating with a loss of control, did you: Eat until you feel uncomfortably full?'
  # edds9    9. Eat large amounts of food when you didn’t feel physically hungry?
  # If (EDDS4=0 and EDDS5=0) EDDS9=0.
  data$EDDS9 <- factor(data$EDDS9, labels = c("0 No", "1 Yes"))
  attr(data$EDDS9, 'label') <- 'EDDS9 During episodes of overeating with a loss of control, did you: Eat large amounts of food when you didn’t feel physically hungry?'
  # edds10      10. Eat alone because you were embarrassed by how much you were eating?
  # If (EDDS4=0 and EDDS5=0) EDDS10=0.
  data$EDDS10 <- factor(data$EDDS10, labels = c("0 No", "1 Yes"))
  attr(data$EDDS10, 'label') <- 'EDDS10 During episodes of overeating with a loss of control, did you: Eat alone because you were embarrassed by how much you were eating?'
  # edds11        11. Feel disgusted with yourself, depressed, or very guilty after overeating?
  # If (EDDS4=0 and EDDS5=0) EDDS11=0.
  data$EDDS11 <- factor(data$EDDS11, labels = c("0 No", "1 Yes"))
  attr(data$EDDS11, 'label') <- 'EDDS11 During episodes of overeating with a loss of control, did you: Feel disgusted with yourself, depressed, or very guilty after overeating?'
  # edds12          12. If you have episodes of uncontrollable overeating, does it make you very upset?
  # If (EDDS4=0 and EDDS5=0) EDDS12=0.
  data$EDDS12 <- factor(data$EDDS12, labels = c("0 No", "1 Yes"))
  attr(data$EDDS12, 'label') <- 'EDDS12 During episodes of overeating with a loss of control, did you: If you have episodes of uncontrollable overeating, does it make you very upset?'
  # edds13 - edds17 are continuous, but for display we create factors
  data$EDDS13 <- factor(data$edds_13)
  attr(data$edds_13, 'label') <- 'EDDS13 In order to prevent weight gain or counteract the effects of eating, how many times per month on average over the past 3 months have you: Made yourself vomit?'
  attr(data$EDDS13, 'label') <- 'EDDS13 In order to prevent weight gain or counteract the effects of eating, how many times per month on average over the past 3 months have you: Made yourself vomit?'
  data$EDDS14 <- factor(data$edds_14)
  attr(data$edds_14, 'label') <- 'EDDS14 In order to prevent weight gain or counteract the effects of eating, how many times per month on average over the past 3 months have you: Used laxatives or diuretics?'
  attr(data$EDDS14, 'label') <- 'EDDS14 In order to prevent weight gain or counteract the effects of eating, how many times per month on average over the past 3 months have you: Used laxatives or diuretics?'
  data$EDDS15 <- factor(data$edds_15)
  attr(data$edds_15, 'label') <- 'EDDS15 In order to prevent weight gain or counteract the effects of eating, how many times per month on average over the past 3 months have you: Fasted (skipped at least 2 meals in a row)?'
  attr(data$EDDS15, 'label') <- 'EDDS15 In order to prevent weight gain or counteract the effects of eating, how many times per month on average over the past 3 months have you: Fasted (skipped at least 2 meals in a row)?'
  data$EDDS16 <- factor(data$edds_16)
  attr(data$edds_16, 'label') <- 'EDDS16 In order to prevent weight gain or counteract the effects of eating, how many times per month on average over the past 3 months have you: Engaged in more intense exercise specifically to counteract the effects of overeating?'
  attr(data$EDDS16, 'label') <- 'EDDS16 In order to prevent weight gain or counteract the effects of eating, how many times per month on average over the past 3 months have you: Engaged in more intense exercise specifically to counteract the effects of overeating?'
  data$EDDS17 <- factor(data$edds_17)
  attr(data$edds_17, 'label') <- 'EDDS17 How many times per month on average over the past 3 months have you eaten after awakening from sleep or eaten an unusually large amount of food after your evening meal and felt distressed by the night eating?'
  attr(data$EDDS17, 'label') <- 'EDDS17 How many times per month on average over the past 3 months have you eaten after awakening from sleep or eaten an unusually large amount of food after your evening meal and felt distressed by the night eating?'
  data$EDDS18 <- factor(data$edds_18, labels = c("0 Not At All", "1", "2 Slightly", "3", "4 Moderately", "5", "6 Extremely"))
  attr(data$edds_18, 'label') <- 'EDDS18 How much do eating or body image problems impact your relationships with friends and family, work performance, and school performance? (Please select the best answer from 0 to 6)'
  attr(data$EDDS18, 'label') <- 'EDDS18 How much do eating or body image problems impact your relationships with friends and family, work performance, and school performance? (Please select the best answer from 0 to 6)'
  
  return(data)
}
