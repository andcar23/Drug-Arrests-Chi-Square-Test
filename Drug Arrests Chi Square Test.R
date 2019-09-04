# Andrew Carroll

# install readxl package if needed
#install.packages("readxl")

# load BPD arrests excel sheet into R
library(readxl)
police <- read_excel(file.choose(), sheet="BPD_Arrests")

# create new column with properly formatted time
police$ArrestTime.f <- format(police$ArrestTime, format="%H:%M:%S")

# create new column with time of day as "Early Morning", "Morning", "Afternoon", "Evening"
police$TimeOfDay <- factor(
  ifelse(police$ArrestTime.f >= "00:00:00" & police$ArrestTime.f <= "05:59:59", 1,
         ifelse(police$ArrestTime.f >= "06:00:00" & police$ArrestTime.f <= "11:59:59", 2,
                ifelse(police$ArrestTime.f >= "12:00:00" & police$ArrestTime.f <= "17:59:59", 3,
                       ifelse(police$ArrestTime.f >= "18:00:00" & police$ArrestTime.f <= "23:59:59", 4, NA)))), 
  labels= c("Early Morning", "Morning", "Afternoon", "Evening"), ordered=T) 

# subset to only include drug offenses
police.sub <- subset(police, police$IncidentOffense == "87NARCOTICS")

# create table for Chi-sq test
tbl <- table(police.sub$TimeOfDay, police.sub$IncidentOffense)

# conduct chi-square test
chisq.test(tbl)

# convert table to matrix
tbl.m <- as.data.frame.matrix(tbl)

# display PQ bar chart
barplot(tbl.m$`87NARCOTICS`, main = "Frequency of Drug Offenses\nby Time of Day",
        names.arg = c("Early Morning", "Morning", "Afternoon", "Evening"),
        xlab = "Time of Day",
        ylab = "Number of Drug Offenses",
        ylim = c(0,700),
        col = "forestgreen")

#Function to find Cramer's V
library(lsr)
cramersV(tbl.m)
