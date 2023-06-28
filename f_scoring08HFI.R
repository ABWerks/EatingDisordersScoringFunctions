# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the HFI
# ABWerks
# October 2021
# 
# Depends: Hmisc
#
# Description:
#   Function to score the Household Food Insecurity (HFI) survey
#@article{blumberg1999effectiveness,
#   title={The effectiveness of a short form of the Household Food Security Scale.},
#   author={Blumberg, Stephen J and Bialostosky, Karil and Hamilton, William L and Briefel, Ronette R},
#   journal={American journal of public health},
#   volume={89},
#   number={8},
#   pages={1231--1234},
#   year={1999},
#   publisher={American Public Health Association}
# }
# 
# Arguments:
#   data: The survey. Assumes item names in the original survey have underscores ~ HFI_1, HFI_2, etc...
#
# Details:
# (1) Coding Responses and Assessing Households’ Food Security Status:   
#   Responses of “often” or “sometimes” on questions HH3 and HH4, 
#   and “yes” on AD1, AD2, and AD3 are coded as affirmative (yes). 
#   Responses of “almost every month” and “some months but not every month” 
#   on AD1a are coded as affirmative (yes). The sum of affirmative responses 
#   to the six questions in the module is the household’s raw score on the scale. 
# Food security status is assigned as follows: 
#   Raw score 0-1—High or marginal food security (raw score 1 may be considered 
#     marginal food security, but a large proportion of households that would 
#     be measured as having marginal food security using the household or 
#     adult scale will have raw score zero on the six-item scale) 
#   Raw score 2-4—Low food security 
#   Raw score 5-6—Very low food security 
# For some reporting purposes, the food security status of households with 
#   raw score 0-1 is described as food secure and the two categories 
#   “low food security” and “very low food security” in combination are 
#   referred to as food insecure.
# Items
#   HFI_1 HH3) In the last 12 months, the food that I/we bought just didn’t last, and I/we didn’t have money to get more
#   HFI_2 HH4) In the last 12 months, I/we couldn’t afford to eat balanced meals.
#   HFI_3 AD1) In the last 12 months, did you (or other adults in your household) ever cut the size of your meals or skip meals because there wasn’t enough money for food?
#   HFI_4 AD2) In the last 12 months, did you ever eat less than you felt you should because there wasn’t enough money for food?
#   HFI_5 AD3) In the last 12 months, were you ever hungry but didn’t eat because there wasn’t enough money for food?
# 
# Values:
#   scoreHFITotal: item total score for all HFI items
#   scoreHFICat: 0-1—High or marginal food security, 2-4—Low food security, 5-6—Very low food security 
#   HFI*: All missing values converted to 0
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Items And Questions
dsItems  <- read.csv("EatingDisorderItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(HFI)", dsItems$Item, ignore.case = T), ]

# table shortcut
f_tableNA <- function(...)
  table(..., useNA = "ifany")

f_scoringHFI <- function(data){
  varsHFI <- paste('HFI', 1:5, sep = '_')
  data$scoreHFITotal <- rowSums(data[, varsHFI], na.rm = T)
  data$scoreHFICat <- factor(ifelse(
    data$scoreHFITotal < 2, '0 High or marginal food security'
    , ifelse(data$scoreHFITotal >= 2 & data$scoreHFITotal < 5, '1 Low food security'
             , '2 Very low food security')))
  data[, varsHFI] <- sapply( 
    data[, varsHFI]
    , function(x) ifelse(is.na(x), 0, x))
  cat('HFI Total \n')
  print(f_tableNA(data$scoreHFITotal))
  cat('HFI Cat \n')
  print(f_tableNA(data$scoreHFICat))
  
  data$HFI1 <- factor(data$HFI_1, labels = c("0 Never true", "1 Sometimes to often true"))
  data$HFI2 <- factor(data$HFI_2, labels = c("0 Never true", "1 Sometimes to often true"))
  data$HFI3 <- factor(data$HFI_3, labels = c("0 Never", "1 Some months to every month"))
  data$HFI4 <- factor(data$HFI_4, labels = c("0 N0", "1 Yes"))
  data$HFI5 <- factor(data$HFI_5, labels = c("0 N0", "1 Yes"))

  for(i in 1:5)
    Hmisc::label(data[, paste0("HFI", i)]) <- dsItems$Question[dsItems$Item == paste0("HFI", i)]
  cat('HFI Items \n')
  print(apply(data[, paste0("HFI", 1:5)], 2, f_tableNA))
  
  return(data)
}
