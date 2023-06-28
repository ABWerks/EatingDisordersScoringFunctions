# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the SCOFF
# ABWerks
# October 2021
# 
# Depends: Hmisc
#
# Description:
# Function to score the SCOFF items
#@article{morgan2000scoff,
#   title={The SCOFF questionnaire},
#   author={Morgan, John F and Reid, Fiona and Lacey, J Hubert},
#   journal={The Western journal of medicine},
#   volume={172},
#   number={3},
#   pages={164},
#   year={2000},
#   publisher={BMJ Publishing Group LTD}
# }
# 
# Depends: Hmisc
# 
# Arguments:
#   data: The survey. Assumes item names in the original survey have underscores ~ EDDS_1, EDDS_2, etc...
#
# Details:
# Score one point for each question answered “yes”
# Two or more points suggests an Eating Disorder
# (https://fpnotebook.com/Psych/Exam/ScfQstnr.htm) 
# Item	Question	Values
# 0 = No, 1 = Yes
# SCOFF_1	S Do you make yourself Sick because you feel uncomfortably full? 	
# SCOFF_2	C Do you worry you have lost Control over how much you eat?
# SCOFF_3	O Have you recently lost more than (One stone 6.35 kg) 14 lbs in a three month period?
# SCOFF_4	F Do you believe yourself to be Fat when others say you are too thin?
# SCOFF_5	F Would you say that Food dominates your life?
#
# Values:
#   scoreSCOFFTotal: item total score for all SCOFF items
#   SCOFF_*: All missing values converted to 0
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Items And Questions
dsItems  <- read.csv("EatingDisorderItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(SCOFF)", dsItems$Item, ignore.case = T), ]

# table shortcut
f_tableNA <- function(...)
  table(..., useNA = "ifany")
  
f_scoringSCOFF <- function(data){
  varsScoff <- paste('SCOFF', 1:5, sep = '_')
  data$scoreSCOFFTotal <- rowSums(data[, varsScoff], na.rm = T)
  data[, varsScoff] <- sapply(
    data[, varsScoff]
    , function(x) ifelse(is.na(x), 0, x))
  cat('SCOFF Total \n')
  print(f_tableNA(data$scoreSCOFFTotal))
  
  data$SCOFF1 <- factor(data$SCOFF_1, labels = c('0 No', '1 Yes'))
  data$SCOFF2 <- factor(data$SCOFF_2, labels = c('0 No', '1 Yes'))
  data$SCOFF3 <- factor(data$SCOFF_3, labels = c('0 No', '1 Yes'))
  data$SCOFF4 <- factor(data$SCOFF_4, labels = c('0 No', '1 Yes'))
  data$SCOFF5 <- factor(data$SCOFF_5, labels = c('0 No', '1 Yes'))
  cat('SCOFF Items \n')
  print(apply(data[, gsub("_", "", varsScoff)], 2, f_tableNA))
  
  for(i in 1:5)
    Hmisc::label(data[, paste0("SCOFF", i)]) <- dsItems$Question[dsItems$Item == paste0("SCOFF", i)]

  return(data)
}
