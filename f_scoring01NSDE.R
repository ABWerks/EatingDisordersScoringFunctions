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
# Depends: Hmisc
#
# Arguments:
#   data: The survey. Assumes item names in the original survey have underscores ~ SDE_1, SCOFF_5, etc...
#
# Details:
#   38 items scored yes/no and labeled with the corresponding question:
#   SDE1	Do you often feel the desire to eat when you are emotionally upset or stressed?
#   SDE2	Has thinking about food, eating, or calories made it very difficult to concentrate on things you are interested in (for example, working, following a conversation, or reading)?
#   SDE3	Do you give too much time and thought to food?
#   SDE4	Does your eating or weight cause distress or difficulty in your life (for example, working/school, in relationships, or in some other important way)?
#   SDE5	Have you engaged in unhealthy dieting that you or others are concerned about?
#   SDE6	Are you often preoccupied with a desire to be thinner?
#   SDE7	Have you been deliberately trying to limit the amount of food you eat to influence your shape or weight (whether or not you have succeeded)?
#   SDE8	Have you gone for long periods of time (8 waking hours or more) without eating anything at all in order to influence your shape or weight?
#   SDE9	Have you tried to follow definite rules regarding your eating (for example, a calorie limit) in order to influence your shape or weight (whether or not you have succeeded)?
#   SDE10	Have you engaged in strict dieting to alter your weight or shape (whether or not you have succeeded)?
#   SDE11	Has your shape or weight influenced how you think about (judge) yourself as a person?
#   SDE12	Are you mostly dissatisfied with your shape or weight?
#   SDE13	Has seeing your reflection (for example, in a mirror or shop window) made you feel bad about your shape?
#   SDE14	Do you exaggerate or magnify the importance of your weight?
#   SDE15	Is your weight or shape one of the most important things about you, as a person?
#   SDE16	Have you had a definite fear that you might gain weight, or gain more weight?
#   SDE17	Do other people think that you are too thin or have lost too much weight?
#   SDE18	Have you lost 20 pounds or more in the past 6 months?
#   SDE19	Are you putting extreme effort in maintaining your weight?
#   SDE20	Have you regularly eaten extremely large amounts of food at one time and felt that your eating was out of control at that time?
#   SDE21	Have you had a definite fear of losing control over eating?
#   SDE22	Have you gone on eating binges where you felt that you could not stop?
#   SDE23	Over the last three months, have you eaten excessive amounts of food at one time and felt that your eating was out of control at that time?
#   SDE24	Do you sometimes make yourself throw up (vomit) to control your weight?
#   SDE25	To control your weight or shape do you do ANY of the following: vomit, use laxatives or diuretics (water pills), exercise excessively in response to overeating, use over the counter diet aids, or alter prescription medication use (for example, insulin)?
#   SDE26	Do you eat half or more of your food after suppertime?
#   SDE27	Do you need to eat in order to get back to sleep when you wake up at night?
#   SDE28	Do you have cravings or urges to eat snacks when you wake up at night?
#   SDE29	Do you feel out of control over your eating while you are up at night?
#   SDE30	If you eat at night, is it upsetting to you?
#   SDE31	Do you eat half or more of your food after suppertime OR  Do you need to eat in order to go to, or get back to sleep?
#   SCOFF1	Do you make yourself sick because you feel uncomfortably full?
#   SCOFF2* Do you worry you have lost control over how much you eat?
#   SCOFF3  Have you recently lost more than 14 lbs in a three month period?
#   SCOFF4  Do you believe yourself to be fat when others say you are too thin?
#   SCOFF5  Would you say that food dominates your life?
#   PHQ6a	Do you often feel that you can’t control what or how much you eat?
#   PHQ6b	Do you often eat, within any 2-hour period, what most people would regard as an unusually large amount of food? (If you checked “NO” to either a or b, go to question #9).
#
# Reults:
#   Returns the survey with 37 new columns of the formatted SDE items
#   Original items remain unchanged
#   *SCOFF_2 is not part of the NSDE, but it's included here for completeness
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# table shortcut
f_tableNA <- function(...) 
  table(..., useNA = "ifany")

# Items And Questions
dsItems  <- read.csv("EatingDisorderItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(SDE)|(SCOFF)|(PHQ6[ab])", dsItems$Item, ignore.case = T), ]

f_scoringNSDE <- function(data){
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
  
  data$SCOFF2 <- factor(data$SCOFF_2, labels = c('0 No', '1 Yes'))

  for(i in 1:31)
    Hmisc::label(data[, paste0("SDE", i)]) <- dsItems$Question[dsItems$Item == paste0("SDE", i)]
  for(i in c(1:5))
    Hmisc::label(data[, paste0("SCOFF", i)]) <- dsItems$Question[dsItems$Item == paste0("SCOFF", i)]  
  for(i in c("a","b"))
    Hmisc::label(data[, paste0("PHQ6", i)]) <- dsItems$Question[dsItems$Item == paste0("PHQ6", i)]  

  return(data)
}

