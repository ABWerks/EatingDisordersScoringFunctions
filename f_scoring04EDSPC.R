# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the EDS-PC
# ABWerks
# October 2021
#
# Depends: Hmisc
# 
# Description:
# Function to score the Eating Disorders Screen for Primary Care (EDS-PC) items
# @article{cotton2003four,
#   title={Four simple questions can help screen for eating disorders},
#   author={Cotton, Mary-Anne and Ball, Christopher and Robinson, Paul},
#   journal={Journal of general internal medicine},
#   volume={18},
#   number={1},
#   pages={53--56},
#   year={2003},
#   publisher={Wiley Online Library}
# }
# 
# Arguments:
#   data: The survey. Assumes item names in the original survey have underscores ~ EDS_PC1, EDS_PC2, etc...
#
# Details:
# Score one point for each question answered “yes”
# EDS_PC1 Are you satisfied with your eating patterns?
# EDS_PC2	Do you ever eat in secret?
# EDS_PC3	Does your weight affect the way you feel about yourself?
# EDS_PC4	Have any members of your family suffered with an eating disorder?
# EDS_PC5	Do you currently suffer with or have you ever suffered in the past with an eating disorder?
#
# Values:
#   scoreEDSPCTotal: item total score for all EDS-PC items
#   EDS_PC*: All missing values converted to 0
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Items And Questions
dsItems  <- read.csv("EatingDisorderItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(EDSPC)", dsItems$Item, ignore.case = T), ]

# table shortcut
f_tableNA <- function(...)
  table(..., useNA = "ifany")

f_scoringEDSPC <- function(data){
  varsEDSPC <- paste('EDS_PC', 1:5, sep = '')
  data$scoreEDSPCTotal <- rowSums(data[, varsEDSPC], na.rm = T)
  data[, varsEDSPC] <- sapply(
    data[, varsEDSPC]
    , function(x) ifelse(is.na(x), 0, x))
  cat('EDSPC Total \n')
  print(f_tableNA(data$scoreEDSPCTotal))

  data$EDSPC1 <- factor(data$EDS_PC1, labels = c('0 No', '1 Yes'))
  data$EDSPC2 <- factor(data$EDS_PC2, labels = c('0 No', '1 Yes'))
  data$EDSPC3 <- factor(data$EDS_PC3, labels = c('0 No', '1 Yes'))
  data$EDSPC4 <- factor(data$EDS_PC4, labels = c('0 No', '1 Yes'))
  data$EDSPC5 <- factor(data$EDS_PC5, labels = c('0 No', '1 Yes'))
  cat('EDSPC Items \n')
  print(apply(data[, gsub("_", "", varsEDSPC)], 2, f_tableNA))

 for(i in 1:5)
    Hmisc::label(data[, paste0("EDSPC", i)]) <- dsItems$Question[dsItems$Item == paste0("EDSPC", i)]
 
  return(data)
}