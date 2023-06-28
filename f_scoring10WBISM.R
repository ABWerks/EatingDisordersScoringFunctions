# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the WBIS-M
# Adam Batten
# October 2021
# 
# Depends: Hmisc
#
# Description:
# Function to score the Modified Weight Bias Internalization Scale (WBIS-M)
#
# @article{pearl2014measuring,
#   title={Measuring internalized weight attitudes across body weight categories: validation of the modified weight bias internalization scale},
#   author={Pearl, Rebecca L and Puhl, Rebecca M},
#   journal={Body image},
#   volume={11},
#   number={1},
#   pages={89--92},
#   year={2014},
#   publisher={Elsevier}
# }
# Arguments:
#   data: The survey. Assumes item names in the original survey have underscores ~ WBISM_1, WBISM_2, etc...
#
# Details:
# All items are Likert scale 1-7: strongly disagree to strongly agree
# Question 1 is reverse scored
# 
# Items
#   WBIS_1 Because of my weight, I feel that I am just as competent as anyone
#   WBIS_2 I am less attractive than most other people because of my weight.
#   WBIS_3 I feel anxious about my weight because of what people might think of me.
#   WBIS_4 I wish I could drastically change my weight.
#   WBIS_5 Whenever I think a lot about my weight, I feel depressed.
#   WBIS_6 I hate myself for my weight.
#   WBIS_7 My weight is a major way that I judge my value as a person.
#   WBIS_8 I don’t feel that I deserve to have a really fulfilling social life, because of my weight.
#   WBIS_9 I am OK being the weight that I am.
#   WBIS_10 Because of my weight, I don’t feel like my true self.
#   WBIS_11 Because of my weight, I don’t understand how anyone attractive would want to d
#
# Values:
#   scoreWBISMTotal: item total score for all WBISM items
#   WBIS_*: All missing values converted to 0
#   WBIS[1-11]: Factors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Items And Questions
dsItems  <- read.csv("EatingDisorderItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(WBIS)", dsItems$Item, ignore.case = T), ]

# table shortcut
f_tableNA <- function(...)
  table(..., useNA = "ifany")

f_scoringWBISM <- function(data){
  varsWBISM <- paste('WBIS', 1:11, sep = '_')
  data$scoreWBISMTotal <- rowSums(data[, varsWBISM], na.rm = T)
  data[, varsWBISM] <- sapply(
    data[, varsWBISM]
    , function(x) ifelse(is.na(x), 0, x))
  cat('WBISM Total \n')
  print(summary(data$scoreWBISMTotal))
  
  foo <- data[, varsWBISM]
  colnames(foo) <- paste0('WBIS', 1:11)
  data  <- cbind(data, foo)

  factorLabels <- c("0 Not applicable", "1 Strongly disagree", "2 Disagree", "3 Somewhat disagree", "4 Neither agree nor disagree", "5 Somewhat agree", "6 Agree", "7 Strongly agree")
  for(i in 2:11){
    data[, paste0("WBIS", i)] <- factor(
      data[, paste0("WBIS", i)]
      , labels = factorLabels[sort(unique(data[, paste0("WBIS", i)]))+1]
    )
    Hmisc::label(data[, paste0("WBIS", i)]) <- dsItems$Question[dsItems$Item == paste0("WBIS", i)]
    cat(paste0("WBIS", i), "\n")
    print(f_tableNA(data[, paste0("WBIS", i)]))
  }
  # WBISM-1 is reverse coded
  data[, "WBIS1"]  <- factor(data[, "WBIS1"], labels = c(
    "0 Not Applicable", "1 Strongly agree", "2 Agree", "3 Somewhat agree", "4 Neither agree nor disagree", "5 Somewhat disagree", "6 Disagree", "7 Strongly disagree"
  ))
  Hmisc::label(data[, paste0("WBIS", 1)]) <- dsItems$Question[dsItems$Item == paste0("WBIS", 1)]
  cat(paste0("WBIS", 1), "\n")
  print(f_tableNA(data[, paste0("WBIS", 1)]))

  return(data)
}
