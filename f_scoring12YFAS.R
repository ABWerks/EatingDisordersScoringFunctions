# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the YFAS
# ABWerks
# October 2021
# 
# Depends: Hmisc
#
# Description:
# Function to score the Modified Yale Food Addiction Scale (MYFAS)
# 
# Arguments:
#   data: The survey. Assumes item names in the original survey have underscores ~ MYFAS_1, MYFAS_2, etc...
#
# Details:
# YFAS
# Substance taken in larger amount and for longer period than intended
# Question #1
# Persistent desire or repeated unsuccessful attempts to quit
# Question #11
# Much time/activity to obtain, use, recover
# Question #2
# Important social, occupational, or recreational activities given up or reduced
# Question #3
# Use continues despite knowledge of adverse consequences (e.g., emotional problems, physical problems)
# Question #8
# Tolerance (marked increase in amount; marked decrease in effect)
# Question #9
# Characteristic withdrawal symptoms; substance taken to relieve withdrawal
# Question #4
# Continued use despite social or interpersonal problems
# Question #13
# Failure to fulfill major role obligation (e.g., work, school, home)
# Question #7
# Use in physically hazardous situations
# Question #12
# Craving, or a strong desire or urge to use
# Question #10
# Use causes clinically significant impairment or distress
# Questions #5, #6
# Each question has a different threshold: 0 = threshold not met, 1 = threshold is met
#   A.	Once a month (=>2): #3, #7, #12, #13
#   B.	Once a week (=>4): #1, #4, #8, #10
#   C.	Two to three times a week (=>5): #2, #5, #6, #9, #11
#   After computing the threshold for each question, if the score for the symptom 
#   criterion is > 1, then the criterion has been met and is scored as 1. 
#   If the score = 0, then the symptom criterion has not been met and is scored as 0.
# For the symptom count scoring option, add up all of the scores for each of the 
#   11 criterion (e.g. Tolerance, Withdrawal, Use Despite Negative Consequence). 
#   Do not add clinical significance (#5 and #6) to the score. This score should 
#   range from 0 to 11 (0 symptoms to 11 symptoms.)
# For the “diagnosis” scoring option, a participant can meet for mild, moderate 
#   or severe food addiction. Both the symptom count score and the clinical 
#   significance criterion are used.
#   A.	No Food Addiction = 1 or fewer symptoms
#   B.	No Food Addiction = Items 5 & 6 below threshold: Does not meet criteria for clinical significance 
#   C.	Mild Food Addiction = 2 or 3 symptoms and clinical significance (at least one of items 5 or 6 at threshold)
#   D.	Moderate Food Addiction = 4 or 5 symptoms and clinical significance
#   E.	Severe Food Addiction = 6 or more symptoms and clinical significance
#   (https://fastlab.psych.lsa.umich.edu/yale-food-addiction-scale/) 
#
# Items
#   Likert scale: 
#     Never  (0)
#     Less than monthly  (1)
#     Once a month  (2)
#     2-3 times a month  (3)
#     Once a week  (4)
#     2-3 times a week  (5)
#     4-6 times a week  (6)
#     Every day  (7)
#   MYFAS_1 I ate to the point where I felt physically ill
#   MYFAS_2 I spent a lot of time feeling sluggish or tired from overeating.
#   MYFAS_3 I avoided work, school or social activities because I was afraid I would overeat there.
#   MYFAS_4 If I had emotional problems because I hadn’t eaten certain foods, I would eat those foods to feel better.
#   MYFAS_5 My eating behavior caused me a lot of distress.
#   MYFAS_6 I had significant problems in my life because of food and eating. These may have been problems with my daily routine, work, school, friends, family, or health.
#   MYFAS_7 My overeating got in the way of me taking care of my  family or doing household chores.
#   MYFAS_8 I kept eating in the same way even though my eating caused emotional problems.
#   MYFAS_9 Eating the same amount of food did not give me as   much enjoyment as it used to.
#   MYFAS_10 I had such strong urges to eat certain foods that I  couldn’t think of anything else.
#   MYFAS_11 I tried and failed to cut down on or stop eating certain  foods.
#   MYFAS_12 I was so distracted by eating that I could have been hurt (e.g., when driving a car, crossing the street, operating machinery).
#   MYFAS_13 My friends or family were worried about how much I overate.
#
# Values:
#   scoreYFAS[1-]: item threshold for the YFAS items (0, 1)
#   scoreYFASSymptoms: threshold score for the YFAS symptom items (0 - 11)
#   YFAS_*: All missing values converted to 0
#   YFAS1-YFAS13: Factors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Items And Questions
dsItems  <- read.csv("EatingDisorderItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(MYFAS)", dsItems$Item, ignore.case = T), ]

# table shortcut
f_tableNA <- function(...)
  table(..., useNA = "ifany")

# Logic functions
## >= 2
f_ge2 <- function(x)
  ifelse(x >= 2, 1, 0)
## >= 4
f_ge4 <- function(x)
  ifelse(x >= 4, 1, 0)
## >= 5
f_ge5 <- function(x)
  ifelse(x >= 5, 1, 0)
# YFAS scoring function
f_scoringYFAS <- function(data){
  varsYFAS <- paste('MYFAS', 1:13, sep = '_')
  
  # Recode to 0
  data[, varsYFAS] <- sapply(
    data[, varsYFAS]
    , function(x) ifelse(is.na(x), 0, x))
  
  # Score
  data$scoreYFAS_1 <- f_ge4(data$MYFAS_1)
  data$scoreYFAS_2 <- f_ge5(data$MYFAS_2)
  data$scoreYFAS_3 <- f_ge2(data$MYFAS_3)
  data$scoreYFAS_4 <- f_ge4(data$MYFAS_4)
  data$scoreYFAS_5 <- f_ge5(data$MYFAS_5)
  data$scoreYFAS_6 <- f_ge5(data$MYFAS_6)
  data$scoreYFAS_7 <- f_ge2(data$MYFAS_7)
  data$scoreYFAS_8 <- f_ge4(data$MYFAS_8)
  data$scoreYFAS_9 <- f_ge5(data$MYFAS_9)
  data$scoreYFAS_10 <- f_ge4(data$MYFAS_10)
  data$scoreYFAS_11 <- f_ge5(data$MYFAS_11)
  data$scoreYFAS_12 <- f_ge2(data$MYFAS_12)
  data$scoreYFAS_13 <- f_ge2(data$MYFAS_13)
  
  # Total score without items 5 & 6
  data$scoreYFASTotal <- rowSums(
    data[, varsYFAS[!grepl('[56]', varsYFAS)]], na.rm = T)

  # Factor
  data$scoreYFASSymptoms <- ifelse(
    data$scoreYFASTotal < 2 | (data$scoreYFAS_5 == 0 & data$scoreYFAS_6 == 0)
    , '00 No Food Addiction'
    , ifelse(
      data$scoreYFASTotal%in%2:3 & (data$scoreYFAS_5 == 1 | data$scoreYFAS_6 == 1)
      , '01 Mild Food Addiction'
      , ifelse(
        data$scoreYFASTotal%in%4:5 & (data$scoreYFAS_5 == 1 | data$scoreYFAS_6 == 1)
        , '02 Moderate Food Addiction'
        , ifelse(
          data$scoreYFASTotal>5 & (data$scoreYFAS_5 == 1 | data$scoreYFAS_6 == 1)
          , '03 Severe Food Addiction'
          , NA
        )
      )
    )
  )
  
  cat('YFAS Items \n')
  for(i in 1:13){
    cat(paste0('\n', 'MYFAS_',i,' by scoreYFAS_', i))
    print(f_tableNA(
      as.data.frame(data)[,paste0('MYFAS_',i)]
      , as.data.frame(data)[,paste0('scoreYFAS_', i)]))
  }
  cat('YFAS Total \n')
  print(summary(data$scoreYFASTotal))
  cat('YFAS Factor \n')
  print(f_tableNA(data$scoreYFASSymptoms))

  foo <- data[, varsYFAS]
  colnames(foo) <- paste0('MYFAS', 1:7)
  data  <- cbind(data, foo)

  factorLabels <- c("0 Never", "1 Less than monthly", "2 Once a month", "3 2-3 times a month", "4 Once a week", "5 2-3 times a week", "6 4-6 times a week", "7 Every day")
  for(i in 1:13){
    data[, paste0("MYFAS", i)] <- factor(
      data[, paste0("MYFAS", i)]
      , labels = factorLabels[sort(unique(data[, paste0("MYFAS", i)]))+1]
    )
    Hmisc::label(data[, paste0("MYFAS", i)]) <- dsItems$Question[dsItems$Item == paste0("MYFAS", i)]
    cat(paste0("MYFAS", i), "\n")
    print(f_tableNA(data[, paste0("MYFAS", i)]))
  }

  return(data)
}
