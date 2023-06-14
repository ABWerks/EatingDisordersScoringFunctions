# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the SDE
# Author: ABWerks
# September 2022
# 
# Description:
# Function to score the Screen for Disordered Eating items (mostly just formatting)
# @article{maguen2018screen,
#   title={Screen for disordered eating: improving the accuracy of eating disorder screening in primary care},
#   author={Maguen, Shira and Hebenstreit, Claire and Li, Yongmei and Dinh, Julie V and Donalson, Rosemary and Dalton, Sarah and Rubin, Emma and Masheb, Robin},
#   journal={General Hospital Psychiatry},
#   volume={50},
#   pages={20--25},
#   year={2018},
#   publisher={Elsevier}
# }
# 
# Arguments:
#   data: The survey
#
# Details:
#   All 37 questions scored yes/no
#
# Reults:
#   Returns the survey with 37 new columns of the formatted SDE items
#   Original items remain unchanged
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
f_scoringSDE <- function(data){
  # Domains
  ## Distress
  varsSDEDistress <- c(paste('SDE', 1:5, sep = '_'), "scoff_5")
  varsSDEDistressF <- c(paste0('SDE', 1:5), "SCOFF5")
  data[, varsSDEDistress] <-sapply(
    data[, varsSDEDistress]
    , function(x) ifelse(is.na(x), 0, x))
  data$SDE1 <- factor(data$SDE_1, labels = c('0 No', '1 Yes'))
  data$SDE2 <- factor(data$SDE_2, labels = c('0 No', '1 Yes'))
  data$SDE3 <- factor(data$SDE_3, labels = c('0 No', '1 Yes'))
  data$SCOFF5 <- factor(data$scoff_5, labels = c('0 No', '1 Yes'))
  data$SDE4 <- factor(data$SDE_4, labels = c('0 No', '1 Yes'))
  data$SDE5 <- factor(data$SDE_5, labels = c('0 No', '1 Yes'))
  cat('varsSDEDistressF Items \n')
  print(apply(data[, varsSDEDistressF], 2, f_tableNA))
  
  ## Restraint
  varsSDERestraint <- paste('SDE', 6:10, sep = '_')
  varsSDERestraintF <- paste0('SDE', 6:10)
  data[, varsSDERestraint] <-sapply(
    data[, varsSDERestraint]
    , function(x) ifelse(is.na(x), 0, x))
  data$SDE6 <- factor(data$SDE_6, labels = c('0 No', '1 Yes'))
  data$SDE7 <- factor(data$SDE_7, labels = c('0 No', '1 Yes'))
  data$SDE8 <- factor(data$SDE_8, labels = c('0 No', '1 Yes'))
  data$SDE9 <- factor(data$SDE_9, labels = c('0 No', '1 Yes'))
  data$SDE10 <- factor(data$SDE_10, labels = c('0 No', '1 Yes'))
  cat('varsSDERestraintF Items \n')
  print(apply(data[, varsSDERestraintF], 2, f_tableNA))
  
  ## BodySatisfaction
  varsSDEBodySatisfaction <- c('scoff_4', paste('SDE', 11:15, sep = '_'))
  varsSDEBodySatisfactionF <- c('SCOFF4', paste0('SDE', 11:15))
  data[, varsSDEBodySatisfaction] <-sapply(
    data[, varsSDEBodySatisfaction]
    , function(x) ifelse(is.na(x), 0, x))
  data$SCOFF4 <- factor(data$scoff_4, labels = c('0 No', '1 Yes'))
  data$SDE11 <- factor(data$SDE_11, labels = c('0 No', '1 Yes'))
  data$SDE12 <- factor(data$SDE_12, labels = c('0 No', '1 Yes'))
  data$SDE13 <- factor(data$SDE_13, labels = c('0 No', '1 Yes'))
  data$SDE14 <- factor(data$SDE_14, labels = c('0 No', '1 Yes'))
  data$SDE15 <- factor(data$SDE_15, labels = c('0 No', '1 Yes'))
  cat('varsSDEBodySatisfactionF Items \n')
  print(apply(data[, varsSDEBodySatisfactionF], 2, f_tableNA))
  
  ## Weight
  varsSDEWeight <- c('scoff_3', paste('SDE', 16:19, sep = '_'))
  varsSDEWeightF <- c('SCOFF3', paste0('SDE', 16:19))
  data[, varsSDEWeight] <-sapply(
    data[, varsSDEWeight]
    , function(x) ifelse(is.na(x), 0, x))
  data$SDE16 <- factor(data$SDE_16, labels = c('0 No', '1 Yes'))
  data$SDE17 <- factor(data$SDE_17, labels = c('0 No', '1 Yes'))
  data$SDE18 <- factor(data$SDE_18, labels = c('0 No', '1 Yes'))
  data$SCOFF3 <- factor(data$scoff_3, labels = c('0 No', '1 Yes'))
  data$SDE19 <- factor(data$SDE_19, labels = c('0 No', '1 Yes'))
  cat('varsSDEWeightF Items \n')
  print(apply(data[, varsSDEWeightF], 2, f_tableNA))
  
  ## Binge 
  varsSDEBinge <- c('PHQ_6a', 'PHQ_6b', paste('SDE', 20:23, sep = '_'))
  varsSDEBingeF <- c('PHQ6a', 'PHQ6b', paste0('SDE', 20:23))
  data$SDE_22 <- ifelse(data$SDE_22==2, 0, data$SDE_22) 
  data[, varsSDEBinge] <-sapply(
    data[, varsSDEBinge]
    , function(x) ifelse(is.na(x), 0, x))
  data$PHQ6a <- factor(data$PHQ_6a, labels = c('0 No', '1 Yes'))
  data$PHQ6b <- factor(data$PHQ_6b, labels = c('0 No', '1 Yes'))
  data$SDE20 <- factor(data$SDE_20, labels = c('0 No', '1 Yes'))
  data$SDE21 <- factor(data$SDE_21, labels = c('0 No', '1 Yes'))
  data$SDE22 <- factor(data$SDE_22, labels = c('0 No', '1 Yes'))
  data$SDE23 <- factor(data$SDE_23, labels = c('0 No', '1 Yes'))
  cat('varsSDEBingeF Items \n')
  print(apply(data[, varsSDEBingeF], 2, f_tableNA))
  
  ## Purge
  varsSDEPurge <- c('scoff_1', paste('SDE', 24:25, sep = '_'))
  varsSDEPurgeF <- c('SCOFF1', paste0('SDE', 24:25))
  data[, varsSDEPurge] <-sapply(
    data[, varsSDEPurge]
    , function(x) ifelse(is.na(x), 0, x))
  data$SDE24 <- factor(data$SDE_24, labels = c('0 No', '1 Yes'))
  data$SDE25 <- factor(data$SDE_25, labels = c('0 No', '1 Yes'))
  data$SCOFF1 <- factor(data$scoff_1, labels = c('0 No', '1 Yes'))
  cat('varsSDEPurgeF Items \n')
  print(apply(data[, varsSDEPurgeF], 2, f_tableNA))
  
  ## Night Eating
  varsSDENEQ <- c(paste('SDE', 26:31, sep = '_'))
  varsSDENEQF <- c(paste0('SDE', 26:31))
  data$SDE_30 <- ifelse(data$SDE_30==2, 0, data$SDE_30) 
  data[, varsSDENEQ] <-sapply(
    data[, varsSDENEQ]
    , function(x) ifelse(is.na(x), 0, x))
  data$SDE26 <- factor(data$SDE_26, labels = c('0 No', '1 Yes'))
  data$SDE27 <- factor(data$SDE_27, labels = c('0 No', '1 Yes'))
  data$SDE28 <- factor(data$SDE_28, labels = c('0 No', '1 Yes'))
  data$SDE29 <- factor(data$SDE_29, labels = c('0 No', '1 Yes'))
  data$SDE30 <- factor(data$SDE_30, labels = c('0 No', '1 Yes'))
  data$SDE31 <- factor(data$SDE_31, labels = c('0 No', '1 Yes'))
  cat('varsSDENEQF Items \n')
  print(apply(data[, varsSDENEQF], 2, f_tableNA))
  
  return(data)
}

