# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the SCOFF
# ABWerks
# October 2021
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
# Arguments:
#   data: The survey
#
# Details:
# Score one point for each question answered “yes”
# Two or more points suggests an Eating Disorder
# (https://fpnotebook.com/Psych/Exam/ScfQstnr.htm) 
# Item	Question	Values
# 0 = No, 1 = Yes
# scoff_1	 Do you make yourself sick because you feel uncomfortably full? 	
# scoff_2	Do you worry you have lost control over how much you eat?
# scoff_3	Have you recently lost more than 14 lbs in a three month period?
# scoff_4	Do you believe yourself to be fat when others say you are too thin?
# scoff_5	Would you say that food dominates your life?
#
# Values:
#   scoreSCOFFTotal: item total score for all SCOFF items
#   scoff_*: All missing values converted to 0
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# table shortcut
f_tableNA <- function(...)
  table(..., useNA = "ifany")
  
f_scoringSCOFF <- function(data){
  varsScoff <- paste('scoff', 1:5, sep = '_')
  data$scoreSCOFFTotal <- rowSums(data[, varsScoff], na.rm = T)
  data[, varsScoff] <- sapply(
    data[, varsScoff]
    , function(x) ifelse(is.na(x), 0, x))
  cat('SCOFF Total \n')
  print(f_tableNA(data$scoreSCOFFTotal))
  cat('SCOFF Items \n')
  print(apply(data[, varsScoff], 2, f_tableNA))
  
  return(data)
}
