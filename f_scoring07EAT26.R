# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the EAT-26
# ABWerks
# October 2021
# 
# Depends: Hmisc
#
# Description:
# Score the eating attitudes test (EAT26)
# (https://www.eat-26.com/scoring/)
# @article{garner1982eating,
#   title={The eating attitudes test: psychometric features and clinical correlates},
#   author={Garner, David M and Olmsted, Marion P and Bohr, Yvonne and Garfinkel, Paul E},
#   journal={Psychological medicine},
#   volume={12},
#   number={4},
#   pages={871--878},
#   year={1982},
#   publisher={Cambridge University Press}
# }
# 
# Arguments:
#   data: The survey. Assumes item names in the original survey have underscores ~ EAT26_1, EAT26_2, etc...
#
# Details:
# Originally scored:
#   Items 1-25 are scored as follows: Always = 3; Usually = 2; Often = 1; Other answers = 0
#   Item 26 is scored in the opposite direction: Never = 3; Rarely = 2; Sometimes = 1; Other answers = 0
# New scoring:
#   New labels created to be consistent with other ED measures: [Never | Rarely | Sometimes] = 0; Often = 1; Usually = 2; Always = 3
#   Handles items with missing factor levels
# Total Test Score: Add item scores for a total test score.
# Flag: Is the total 20 or more?  If yes, make a referral
#
# Values:
#   scoreEAT26Total: item total score for all EAT-26 items
#   scoreEAT26Flag: Dummy variable for scoreEAT26Total > 20
#   EAT26*: Coded items
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Items And Questions
dsItems  <- read.csv("EatingDisorderItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(EAT26)", dsItems$Item, ignore.case = T), ]

# table shortcut
f_tableNA <- function(...)
  table(..., useNA = "ifany")

f_scoringEAT26 <- function(data){
  varsEAT26 <- paste('EAT26', 1:26, sep = '_')
  data$scoreEAT26Total <- rowSums(data[, varsEAT26], na.rm = T)
  data$scoreEAT26Flag <- ifelse(data$scoreEAT26Total>19, 1, 0)
  data[, varsEAT26] <- sapply(
    data[, varsEAT26]
    , function(x) ifelse(is.na(x), 0, x))
  cat('EAT26 Total \n')
  print(summary(data$scoreEAT26Total))
  cat('EAT26 Flag: Is the total 20 or more?  If yes, make a referral \n')
  print(f_tableNA(data$scoreEAT26Flag))

  foo <- data[, varsEAT26]
  colnames(foo) <- paste0('EAT26', 1:26)
  data  <- cbind(data, foo)

  factorLabels <- c("0 Never/Rarely/Sometimes", "1 Often", "2 Usually", "3 Always")
  for(i in 1:25){
    data[, paste0("EAT26", i)] <- factor(
      data[, paste0("EAT26", i)]
      , labels = factorLabels[sort(unique(data[, paste0("EAT26", i)]))+1]
    )
    Hmisc::label(data[, paste0("EAT26", i)]) <- dsItems$Question[dsItems$Item == paste0("EAT26", i)]
    cat(paste0("EAT26", i), "\n")
    print(f_tableNA(data[, paste0("EAT26", i)]))
  }
  data[, "EAT2626"]  <- factor(ifelse(
    data[, "EAT2626"] == 3, "3 Never"), ifelse(
      data[, "EAT2626"] == 2, "2 Rarely", ifelse(
        data[, "EAT2626"] == 1, "1 Sometimes", "0 Often/Usually/Always"
      )
    )
  )
  Hmisc::label(data[, paste0("EAT26", 26)]) <- dsItems$Question[dsItems$Item == paste0("EAT26", 26)]
  cat(paste0("EAT26", 26), "\n")
  print(f_tableNA(data[, paste0("EAT26", 26)]))
  
  return(data)
}
