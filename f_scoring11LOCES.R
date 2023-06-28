# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the ELOCS/LOCES
# ABWerks
# October 2021
# 
# Depends: Hmisc
#
# Description:
# Function to score the Eating Loss of Control/Loss of Control of Eating Scale (ELOCS/LOCES)
# @article{blomquist2014development,
#   title={Development and validation of the eating loss of control scale.},
#   author={Blomquist, Kerstin K and Roberto, Christina A and Barnes, Rachel D and White, Marney A and Masheb, Robin M and Grilo, Carlos M},
#   journal={Psychological assessment},
#   volume={26},
#   number={1},
#   pages={77},
#   year={2014},
#   publisher={American Psychological Association}
# }
#
# Arguments:
#   data: The survey. Assumes item names in the original survey have underscores ~ LOCES_1, LOCES_2, etc...
#
# Details:
# ELOCS/LOCES
# All 7 questions scored on Likert scale 1 to 5, “never” to “always”
# Sum all items for a total score
# Items
#   LOCES1	I continued to eat past the point when I wanted to stop.
#   LOCES2	I felt like I had “blown it” and might as well keep eating.
#   LOCES3	I felt helpless about controlling my eating.
#   LOCES4	My eating felt like a ball rolling down a hill that just kept going and going.
#   LOCES5	I found myself eating despite negative consequences.
#   LOCES6	I felt like the craving to eat overpowered me.
#   LOCES7	I felt like I could not do anything other than eat.
#
# Values:
#   scoreLOCESTotal: item total score for all ELOCS/LOCES items
#   LOCES_*: All missing values converted to 1
#   LOCES[1-7]: Factors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Items And Questions
dsItems  <- read.csv("EatingDisorderItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(LOCES)", dsItems$Item, ignore.case = T), ]

# table shortcut
f_tableNA <- function(...)
  table(..., useNA = "ifany")

f_scoringLOCES <- function(data){
  varsLOCES <- paste('LOCES', 1:7, sep = '_')
  data$scoreLOCESTotal <- rowSums(data[, varsLOCES], na.rm = T)
  data[, varsLOCES] <- sapply(
    data[, varsLOCES]
    , function(x) ifelse(is.na(x), 1, x))
  cat('LOCES Total \n')
  print(summary(data$scoreLOCESTotal))

  foo <- data[, varsLOCES]
  colnames(foo) <- paste0('LOCES', 1:7)
  data  <- cbind(data, foo)

  factorLabels <- c("1 Never", "2 Rarely", "3 Occasionally", "4 Often", "5 Always")
  for(i in 1:7){
    data[, paste0("LOCES", i)] <- factor(
      data[, paste0("LOCES", i)]
      , labels = factorLabels[sort(unique(data[, paste0("LOCES", i)]))]
    )
    Hmisc::label(data[, paste0("LOCES", i)]) <- dsItems$Question[dsItems$Item == paste0("LOCES", i)]
    cat(paste0("LOCES", i), "\n")
    print(f_tableNA(data[, paste0("LOCES", i)]))
  }

  return(data)
}
