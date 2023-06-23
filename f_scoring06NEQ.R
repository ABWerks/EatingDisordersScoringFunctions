# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the NEQ
# ABWerks
# October 2021
# 
# Depends: Hmisc
#
# Description:
# Function to score the NEQ items
# @book{allison2004overcoming,
#   title={Overcoming night eating syndrome: a step-by-step guide to breaking the cycle},
#   author={Allison, Kelly C and Stunkard, Albert J and Thier, Sara L},
#   year={2004},
#   publisher={New Harbinger Publications}
# }
# 
# Arguments:
#   data: The survey. 
#
# Details:
# NEQ
# Items 1, 4 and 14 are reverse scored. Items 1-12 and 14 are summed.
# Oddly item 7 missing is actually a level of the factor meaning "Your mood does not change during the day".
# Item 13 is not included in the total score, but is used to rule out the parasomnia, Nocturnal Sleep Related Eating Disorder (NS-RED).
# Item 15 is not added to the total score, but instead is used as a descriptor of the course of the symptoms.
# Items 16 and 17 are used to confirm the presence of distress or impairment if NES is present.
# A score of 25 or greater is suggestive of night eating syndrome, and a score of 30 and above is a strong indicator of NES, but we suggest that the answers are reviewed with the patient in an interview before a firm diagnosis is made. For example, many patients with night eating symptoms over-estimate their intake at night. Also, if patients are depressed in the late evening and have trouble falling asleep, but only minimal night eating, this could inflate their scores.
#
# NEQ1	How hungry are you usually in the morning?
# NEQ2	When do you usually eat for the first time?
# NEQ3	Do you have cravings or urges to eat snacks after supper, but before bedtime?
# NEQ4	How much control do you have over your eating between supper and bedtime?
# NEQ5	How much of your daily food intake do you consume after suppertime?
# NEQ6	Are you currently feeling blue or down in the dumps?
# NEQ7	When you are feeling blue, is your mood lower in the:
# NEQ8	How often do you have trouble getting to sleep?
# NEQ9	Other than only to use the bathroom, how often do you get up at least once in the middle of the night?
# NEQ10	Do you have cravings or urges to eat snacks when you wake up at night?
# NEQ11	Do you need to eat in order to get back to sleep when you awake at night?
# NEQ12	When you get up in the middle of the night, how often do you snack?
# NEQ13	When you snack in the middle of the night, how aware are you of your eating?
# NEQ14	How much control do you have over your eating while you are up at night?
# NEQ15	How long have your current difficulties with night eating been going on? (in years)
# NEQ16	Is your night eating upsetting to you?
# NEQ17	How much has your night eating affected your life?
#
# Values:
#   scoreNEQTotal: item total score for all NEQ items
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
f_scoringNEQ <- function(data){
  # Recode missing to the lowest level for all items except NEQ[1,4,7,14]
  data[, c('NEQ_2','NEQ_3','NEQ_5','NEQ_6','NEQ_8','NEQ_9','NEQ_10','NEQ_11','NEQ_12')] <- sapply(
    data[, c('NEQ_2','NEQ_3','NEQ_5','NEQ_6','NEQ_8','NEQ_9','NEQ_10','NEQ_11','NEQ_12')]
    , function(x) ifelse(is.na(x), 0, x))
  data$NEQ_1[is.na(data$NEQ_1)] <- 4  
  data$NEQ_4[is.na(data$NEQ_4)] <- 4
  data$NEQ_14[is.na(data$NEQ_14)] <- 4

  # Total score across items 1-12 and 14
  data$neq_tot <- rowSums(
    data[, c('NEQ_1','NEQ_2','NEQ_3','NEQ_4','NEQ_5','NEQ_6','NEQ_7','NEQ_8','NEQ_9','NEQ_10'
    ,'NEQ_11','NEQ_12','NEQ_14')], na.rm = T)
  cat("Summary of neq_tot: total var for all NEQ variables \n")
  print(summary(data$neq_tot))
  
  # NEQ1 is reverse scored
  NEQ1 <- factor(data$NEQ_1, labels = c("0 Very", "1 Moderately", "2 Somewhat", "3 A little", "4 Not at all"))
  NEQ2 <- factor(data$NEQ_2, labels = c("0 Before 9am", "1 9:01 - 12pm", "2 12:01 - 3pm", "3 3:01 - 6pm", "4 6:01 or later"))
  NEQ3 <- factor(data$NEQ_3, labels = c("0 Not at all", "1 A little", "2 Somewhat", "3 Very much so", "4 Extremely so"))
  # NEQ4 is reverse scored
  NEQ4 <- factor(data$NEQ_4, labels = c("0 Complete", "1 Very much", "2 Some", "3 A little", "4 Not at all"))
  NEQ5 <- factor(data$NEQ_5, labels = c("0 0% (none)", "1 1 - 25% (up to a quarter)", "2 26% - 50% (about half)", "3 51% - 75% (more than half)", "4 76% - 100% (almost all)"))
  NEQ6 <- factor(data$NEQ_6, labels = c("0 Not at all", "1 A little", "2 Somewhat", "3 Very much so", "4 Extremely"))
  NEQ7 <- factor(data$NEQ_7, labels = c("0 Early Morning", "1 Late Morning", "2 Afternoon", "3 Early Evening", "4 Nighttime"))
  NEQ8 <- factor(data$NEQ_8, labels = c("0 Never", "1 Sometimes", "2 About half the time", "3 Usually", "4 Always"))
  NEQ9 <- factor(data$NEQ_9, labels = c("0 Never", "1 Less than once a week", "2 About once a week", "3 More than once a week", "4 Every night"))
  NEQ10 <- factor(data$NEQ_10, labels = c("0 Not at all", "1 A little", "2 Somewhat", "3 Very much so", "4 Extremely so"))
  NEQ11 <- factor(data$NEQ_11, labels = c("0 Not at all", "1 A little", "2 Somewhat", "3 Very much so", "4 Extremely so"))
  NEQ12 <- factor(data$NEQ_12, labels = c("0 Not at all", "1 A little", "2 Somewhat", "3 Very much so", "4 Extremely so"))
  NEQ13 <- factor(data$NEQ_13, labels = c("0 Not at all", "1 A little", "2 Somewhat", "3 Very much so", "4 Completely"))
  # NEQ14 is reverse scored
  NEQ14 <- factor(data$NEQ_14, labels = c("0 Complete", "1 Very much", "2 Some", "3 A little", "4 Not at all"))

  for(i in 1:14)
    Hmisc::label(data[, paste0("NEQ", i)]) <- dsItems$Question[dsItems$Item == paste0("NEQ", i)]

  return(data)
}