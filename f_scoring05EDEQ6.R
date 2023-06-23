# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the EDE-Q 6.0
# Adam Batten
# October 2021
# 
# Description:
# Function to score the EDE-Q 6.0 items
# 
# @book{fairburn2008cognitive,
#   title={Cognitive behavior therapy and eating disorders},
#   author={Fairburn, Christopher G},
#   year={2008},
#   publisher={Guilford Press}
# }
#
# Depends: Hmisc
# 
# Arguments:
#   data: The survey. Assumes item names in the original survey have underscores ~ EDEQ_1, EDEQ_2, etc...
#
# Details:
# EDE-Q 6.0
# To obtain a particular subscale score, the ratings for the relevant items 
# (listed below) are added together and the sum divided by the total number of 
#items forming the subscale. If ratings are only available on some items, a 
#score may nevertheless be obtained by dividing the resulting total by the 
#number of rated items so long as more than half the items have been rated. 
# To obtain an overall or ‘global’ score, the four subscales scores are summed 
#and the resulting total divided by the number of subscales (i.e. four). 
#Subscale scores are reported as means and standard deviations. 
# Subscale Items (the numbers are the item number on the EDE-Q): 
#   Restraint 
#     1 Restraint over eating 
#     2 Avoidance of eating 
#     3 Food avoidance 
#     4 Dietary Rules 
#     5 Empty stomach 
#   Eating Concern 
#     7 Preoccupation with food, eating or calories 
#     9 Fear of losing control over eating 
#     19 Eating in secret 
#     21 Social eating 
#     20 Guilt about eating 
#   Shape Concern 
#     6 Flat stomach 
#     8 Preoccupation with shape or weight 
#     23 Importance of shape 
#     10 Fear of weight gain 
#     26 Dissatisfaction with shape 
#     27 Discomfort seeing body 
#     28 Avoidance of exposure 
#     11 Feelings of fatness4 
#   Weight Concern 
#     22 Importance of weight 
#     24 Reaction to prescribed weighing 
#     8 Preoccupation with shape or weight 
#     25 Dissatisfaction with weight 
#     12 Desire to lose weight 
#
# Values:
#   scoreEDEQ6Restraint: item mean score for EDE-Q 6.0 Restraint subscale
#   scoreEDEQ6EatingConcern: item mean score for EDE-Q 6.0 Eating Concern subscale
#   scoreEDEQ6ShapeConcern: item mean score for EDE-Q 6.0 Shape Concern subscale
#   scoreEDEQ6WeightConcern: item mean score for EDE-Q 6.0 Weight Concern subscale
#   scoreEDEQ6Total: all EDEQ6 item total score
#   scoreEDEQ6Mean: subscale mean score
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Items And Questions
dsItems  <- read.csv("EatingDisorderItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(EDEQ)", dsItems$Item, ignore.case = T), ]

# table shortcut
f_tableNA <- function(...)
  table(..., useNA = "ifany")
  
f_scoringEDEQ6 <- function(data){
  varsEDEQ <- colnames(data)[grepl('EDEQ', colnames(data))]
  varsRestraint <- paste0('EDEQ_', 1:5)
  varsEatingConcern <- c(
    paste0('EDEQ_', c(7,9))
    , paste0('EDEQ_', c(19,21,20)))
  varsShapeConcern <- c(
    paste0('EDEQ_', c(6,8,10,11))
    , paste0('EDEQ_', c(23,26:28)))
  varsWeightConcern <- c(
    paste0('EDEQ_', c(8,12))
    , paste0('EDEQ_', c(22,24,25)))  
  
  # Total
  data$scoreEDEQ6Total <- rowSums(
    data[, c(varsRestraint
             , varsEatingConcern
             , varsShapeConcern
             , varsWeightConcern)]
  , na.rm = T)
  cat('EDEQ6 Total \n')
  print(summary(data$scoreEDEQ6Total))
  
  # Subscale means
  data$scoreEDEQ6Restraint <- rowMeans(data[, varsRestraint], na.rm = T)
  data$scoreEDEQ6EatingConcern <- rowMeans(data[, varsEatingConcern], na.rm = T)
  data$scoreEDEQ6ShapeConcern <- rowMeans(data[, varsShapeConcern], na.rm = T)
  data$scoreEDEQ6WeightConcern <- rowMeans(data[, varsWeightConcern], na.rm = T)
  
  # Subscale mean of means
  data$scoreEDEQ6Mean <- rowMeans(
    data[, c('scoreEDEQ6Restraint'
             , 'scoreEDEQ6EatingConcern'
             , 'scoreEDEQ6ShapeConcern'
             , 'scoreEDEQ6WeightConcern')]
  )
  cat('EDEQ6 Mean \n')
  print(summary(data$scoreEDEQ6Mean))
  
  # Freqs
  cat('EDEQ6 Restraint Items \n')
  print(apply(data[, varsRestraint], 2, f_tableNA))
  cat('EDEQ6 Eating Concern Items \n')
  print(apply(data[, varsEatingConcern], 2, f_tableNA))
  cat('EDEQ6 Shape Concern Items \n')
  print(apply(data[, varsShapeConcern], 2, f_tableNA))
  cat('EDEQ6 Weight Concern Items \n')
  print(apply(data[, varsWeightConcern], 2, f_tableNA))
  
  # Formatting
  cat('EDEQ6 All Items \n')
  sapply(data[, varsEDEQ], function(x) print(f_tableNA(x)))
  data[, varsEDEQ] <-  sapply(data[, varsEDEQ]
    , function(x) ifelse(is.na(x), 0, x))
  
  data$EDEQ1 <- factor(data$EDEQ_1, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                , '3 13-15 days', '4 16-22 days'
                                                , '5 23-27 days', '6 Every day'))
  data$EDEQ2 <- factor(data$EDEQ_2, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                , '3 13-15 days', '4 16-22 days'
                                                , '5 23-27 days', '6 Every day'))
  data$EDEQ3 <- factor(data$EDEQ_3, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                , '3 13-15 days', '4 16-22 days'
                                                , '5 23-27 days', '6 Every day'))
  data$EDEQ4 <- factor(data$EDEQ_4, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                , '3 13-15 days', '4 16-22 days'
                                                , '5 23-27 days', '6 Every day'))
  data$EDEQ5 <- factor(data$EDEQ_5, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                , '3 13-15 days', '4 16-22 days'
                                                , '5 23-27 days', '6 Every day'))
  data$EDEQ6 <- factor(data$EDEQ_6, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                , '3 13-15 days', '4 16-22 days'
                                                , '5 23-27 days', '6 Every day'))
  data$EDEQ7 <- factor(data$EDEQ_7, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                , '3 13-15 days', '4 16-22 days'
                                                , '5 23-27 days', '6 Every day'))
  data$EDEQ8 <- factor(data$EDEQ_8, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                , '3 13-15 days', '4 16-22 days'
                                                , '5 23-27 days', '6 Every day'))
  data$EDEQ9 <- factor(data$EDEQ_9, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                , '3 13-15 days', '4 16-22 days'
                                                , '5 23-27 days', '6 Every day'))
  data$EDEQ10 <- factor(data$EDEQ_10, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                  , '3 13-15 days', '4 16-22 days'
                                                  , '5 23-27 days', '6 Every day'))
  data$EDEQ11 <- factor(data$EDEQ_11, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                  , '3 13-15 days', '4 16-22 days'
                                                  , '5 23-27 days', '6 Every day'))
  data$EDEQ12 <- factor(data$EDEQ_12, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                  , '3 13-15 days', '4 16-22 days'
                                                  , '5 23-27 days', '6 Every day'))
  data$EDEQ19 <- factor(data$EDEQ_19, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                  , '3 13-15 days', '4 16-22 days'
                                                  , '5 23-27 days', '6 Every day'))
  data$EDEQ20 <- factor(data$EDEQ_20, labels = c('0 Not at all', '1 A few of the times', '2 Less than half'
                                                , '3 Half of the time', '4 More than half'
                                                , '5 Most of the times', '6 Every time'))
  data$EDEQ21 <- factor(data$EDEQ_21, labels = c('0 Not at all', '1 ', '2 Slightly'
                                                , '3 ', '4 Moderately'
                                                , '5 ', '6 Markedly'))
  data$EDEQ22 <- factor(data$EDEQ_22, labels = c('0 Not at all', '1', '2 Slightly'
                                                    , '3', '4 Moderately', '5'
                                                    , '6 Markedly'))
  data$EDEQ23 <- factor(data$EDEQ_23, labels = c('0 Not at all', '1', '2 Slightly'
                                                    , '3', '4 Moderately', '5'
                                                    , '6 Markedly'))
  data$EDEQ24 <- factor(data$EDEQ_24, labels = c('0 Not at all', '1 ', '2 Slightly'
                                                , '3 ', '4 Moderately'
                                                , '5 ', '6 Markedly'))
  data$EDEQ25 <- factor(data$EDEQ_25, labels = c('0 Not at all', '1 ', '2 Slightly'
                                                , '3 ', '4 Moderately'
                                                , '5 ', '6 Markedly'))
  data$EDEQ26 <- factor(data$EDEQ_26, labels = c('0 Not at all', '1 ', '2 Slightly'
                                                , '3 ', '4 Moderately'
                                                , '5 ', '6 Markedly'))
  data$EDEQ27 <- factor(data$EDEQ_27, labels = c('0 Not at all', '1 ', '2 Slightly'
                                                , '3 ', '4 Moderately'
                                                , '5 ', '6 Markedly'))
  data$EDEQ28 <- factor(data$EDEQ_28, labels = c('0 Not at all', '1 ', '2 Slightly'
                                                , '3 ', '4 Moderately'
                                                , '5 ', '6 Markedly'))
  data$EDEQ29 <- factor(data$EDEQ_29, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                    , '3 13-15 days', '4 16-22 days'
                                                    , '5 23-27 days', '6 Every day'))
  data$EDEQ30 <- factor(data$EDEQ_30, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                    , '3 13-15 days', '4 16-22 days'
                                                    , '5 23-27 days', '6 Every day'))
  data$EDEQ31 <- factor(data$EDEQ_31, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                    , '3 13-15 days', '4 16-22 days'
                                                    , '5 23-27 days', '6 Every day'))
  data$EDEQ32 <- factor(data$EDEQ_32, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                    , '3 13-15 days', '4 16-22 days'
                                                    , '5 23-27 days', '6 Every day'))
  data$EDEQ33 <- factor(data$EDEQ_33, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                    , '3 13-15 days', '4 16-22 days'
                                                    , '5 23-27 days', '6 Every day'))
  data$EDEQ34 <- factor(data$EDEQ_34, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                    , '3 13-15 days', '4 16-22 days'
                                                    , '5 23-27 days', '6 Every day'))
  data$EDEQ35 <- factor(data$EDEQ_35, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                    , '3 13-15 days', '4 16-22 days'
                                                    , '5 23-27 days', '6 Every day'))
  data$EDEQ36 <- factor(data$EDEQ_36, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                    , '3 13-15 days', '4 16-22 days'
                                                    , '5 23-27 days', '6 Every day'))
  data$EDEQ37 <- factor(data$EDEQ_37, labels = c('0 No days', '1 1-5 days', '2 6-12 days'
                                                    , '3 13-15 days', '4 16-22 days'
                                                    , '5 23-27 days', '6 Every day'))
  return(data)
}
