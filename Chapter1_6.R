library(readr)
Chapter1_5 <- read_csv("Downloads/IntroR/Solutions/Chapter1_5.R")
View(StudyArea)
for (fire in 1:nrow(StudyArea)) {
  if(StudyArea[fire, "TOTALACRES"]>100000){
    print(paste("100K Fire: ", StudyArea[fire,"FIRENAME"],sep=""))
  }
  else if (StudyArea[fire, "TOTALACRES"]>75000){
    print(paste("75K FIRE: ",StudyArea[fire, "FIRENAME"], sep=""))
  }else if (StudyArea[fire, "TOTALACRES"]>50000){
    print(paste("50K Fire: ", StudyArea[fire, "FIRENAME"], sep = ""))
  }}
