# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Eating Disorders Survey Analysis
# Function: Scoring the EPSI
# ABWerks
# October 2021
# 
# Description:
# Function to format the Eating Pathology Symptoms Inventory (EPSI)
# @article{forbush2013development,
#   title={Development and validation of the Eating Pathology Symptoms Inventory (EPSI).},
#   author={Forbush, Kelsie T and Wildes, Jennifer E and Pollack, Lauren O and Dunbar, Danica and Luo, Jing and Patterson, Kathryn and Petruzzi, Liana and Pollpeter, Molly and Miller, Haylie and Stone, Andrea and others},
#   journal={Psychological assessment},
#   volume={25},
#   number={3},
#   pages={859},
#   year={2013},
#   publisher={American Psychological Association}
# }
# 
# Arguments:
#   data: The survey. Assumes item names in the original survey have underscores ~ EPSI_1, EPSI_2, etc...
#   1. I did not like how clothes fit the shape of my body
#   2. I tried to exclude “unhealthy” foods from my diet
#   3. I ate when I was not hungry
#   4. People told me that I do not eat very much
#   5. I felt that I needed to exercise nearly every day
#   6. People would be surprised if they knew how little I ate
#   7. I used muscle building supplements
#   8. I pushed myself extremely hard when I exercised
#   9. I snacked throughout the evening without realizing it
#   10. I got full more easily than most people
#   11. I considered taking diuretics to lose weight
#   12. I tried on different outfits, because I did not like how I looked
#   13. I thought laxatives are a good way to lose weight
#   14. I thought that obese people lack self-control
#   15. I thought about taking steroids as a way to get more muscular
#   16. I used diet teas or cleansing teas to lose weight
#   17. I used diet pills
#   18. I did not like how my body looked
#   19. I ate until I was uncomfortably full
#   20. I felt that overweight people are lazy 21. I counted the calories of foods I ate 22. I planned my days around exercising 23. I thought my butt was too big
#   24. I did not like the size of my thighs
#   25. I wished the shape of my body was different
#   26. I was disgusted by the sight of an overweight person wearing tight clothes 27. I made myself vomit in order to lose weight
#   28. I did not notice how much I ate until after I had finished eating
#   29. I considered taking a muscle building supplement
#   30. I felt that overweight people are unattractive
#   31. I engaged in strenuous exercise at least five days per week
#   32. I thought my muscles were too small
#   33. I got full after eating what most people would consider a small amount of food 34. I was not satisfied with the size of my hips
#   35. I used protein supplements
#   36. People encouraged me to eat more
#   37. If someone offered me food, I felt that I could not resist eating it
#   38. I was disgusted by the sight of obese people
#   39. I stuffed myself with food to the point of feeling sick
#   40. I tried to avoid foods with high calorie content 41. I exercised to the point of exhaustion
#   42. I used diuretics in order to lose weight
#   43. I skipped two meals in a row
#   44. I ate as if I was on auto-pilot
#   45. I ate a very large amount of food in a short period of time (e.g., within 2 hours)
#
# Details:
#   Excessive Exercise, Negative Attitudes toward Obesity, and Muscle Building subscales
#   Questions 5, 7, 8, 14, 15, 20, 22, 26, 29, 30, 31, 32, 35, 38, 41
#   All questions scored on Likert scale 0-4 (“never” to “very often”)
# 
# Sum the scores for individual items for each scale 
#   Excessive Exercise: 1, 3, 7, 11, 15
#   Negative Attitudes toward Obesity 4, 6, 8, 10, 14
#   Muscle Building 2, 5, 9, 12, 13
#
# Values:
#   scoreEPSIExcessiveExercise: item total score for items 1, 3, 7, 11, 15
#   scoreEPSINegativeAttitudes: item total score for items 4, 6, 8, 10, 14
#   scoreEPSIMuscleBuilding: item total score for items 2, 5, 9, 12, 13
#   EPSI_*: All missing values converted to 0
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Items And Questions
dsItems  <- read.csv("EatingDisorderItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(EPSI)", dsItems$Item, ignore.case = T), ]

# table shortcut
f_tableNA <- function(...)
  table(..., useNA = "ifany")

f_scoringEPSI <- function(data){
  varsEPSI <- paste('EPSI', c(1:45), sep = '_')
  data$scoreEPSIExcessiveExercise <- rowSums(
    data[, paste('EPSI', c(1,3,7,11,15), sep = '_')]
    , na.rm = T)
  data$scoreEPSINegativeAttitudes <- rowSums(
    data[, paste('EPSI', c(4,6,8,10,14), sep = '_')]
    , na.rm = T)
  data$scoreEPSIMuscleBuilding <- rowSums(
    data[, paste('EPSI', c(2,5,9,12,13), sep = '_')]
    , na.rm = T)
  
  cat('EPSI Excessive Exercise Total \n')
  print(summary(data$scoreEPSIExcessiveExercise))
  cat('EPSI Negative Attitudes Total \n')
  print(summary(data$scoreEPSINegativeAttitudes))
  cat('EPSI Muscle Building Total \n')
  print(summary(data$scoreEPSIMuscleBuilding))

  data[, varsEPSI[varsEPSI%in%colnames(data)]] <- sapply(
    data[, varsEPSI[varsEPSI%in%colnames(data)]]
    , function(x) ifelse(is.na(x), 0, x))

  foo <- data[, varsEPSI[varsEPSI%in%colnames(data)]]
  colnames(foo) <- gsub("_", "", colnames(foo))
  data  <- cbind(data, foo)
  
  factorLabels <- c("0 Never", "1 Rarely", "2 Sometimes", "3 Often", "4 Very often")
  for(i in as.gsub("EPSI(\\d+)", "\\1", colnames(foo))){
    data[, paste0("EPSI", i)] <- factor(
      data[, paste0("EPSI", i)]
      , labels = factorLabels[sort(unique(data[, paste0("EPSI", i)]))+1]
    )
    Hmisc::label(data[, paste0("EPSI", i)]) <- dsItems$Question[dsItems$Item == paste0("EPSI", i)]
    cat(paste0("EPSI", i), "\n")
    print(f_tableNA(data[, paste0("EPSI", i)]))
  }

  return(data)
}
