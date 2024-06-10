########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     clean_diary3.R
# About:    Script to clean travel diary 3 survey
########################################

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
# library("")

########################################
# Load data

# Read raw CSV from Qualtrics
# data
temp <- read.csv(file=file.path("Data", "Survey 2019 Winter", "Data 0 Raw", 
                                "2019 Winter Transportation Study -- Day 3 Travel Diary Survey_May 23, 2019_15.53.csv"), 
                 stringsAsFactors=F)
temp <- temp[-c(1:2),]
# names
tempN <- read.csv(file=file.path("Data", "Survey 2019 Winter", "Data 0 Raw", 
                                 "2019 Winter Transportation Study -- Day 3 Travel Diary Survey_May 23, 2019_15.53.csv"), 
                  stringsAsFactors=F)
# temp_names <- temp_names[1:2,]
temp_names <- data.frame(NAME1=as.character(names(tempN)))
temp_names$NAME1 <- as.character(temp_names$NAME1)
temp_names$NAME2 <- as.character(tempN[1,])
temp_names$NAME3 <- as.character(tempN[2,])
rm(tempN)

# Inspect
names(temp)
str(temp, list.len=ncol(temp))

# Backup
old_temp <- temp
# temp <- old_temp

########################################
# Format columns and data types

# Survey metadata
temp$StartDate <- as.POSIXct(temp$StartDate, tz="America/Denver")
temp$EndDate <- as.POSIXct(temp$EndDate, tz="America/Denver")
# temp$Status
# temp$IPAddress
temp$Progress <- as.integer(temp$Progress)
names(temp)[which(names(temp)=="Duration..in.seconds.")] <- "Duration"
temp$Duration <- as.integer(temp$Duration)
temp$Finished <- as.logical(temp$Finished)
temp$RecordedDate <- as.POSIXct(temp$RecordedDate, tz="America/Denver")
# temp$ResponseId
# temp$RecipientLastName
# temp$RecipientFirstName
# temp$RecipientEmail
# temp$ExternalReference
temp$LocationLatitude <- as.numeric(temp$LocationLatitude)
temp$LocationLongitude <- as.numeric(temp$LocationLongitude)
# temp$DistributionChannel
# temp$UserLanguage

# Start 1
names(temp)[which(names(temp)=="Q2")] <- "CHECKAGE"
temp$CHECKAGE <- ifelse(temp$CHECKAGE=="I confirm that I am at least 18 years of age.", T, NA)
names(temp)[grepl("Q40_", names(temp))] <- gsub("Q40", "T1", names(temp)[grepl("Q40_", names(temp))])
for (i in grep("T1_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Start 2
names(temp)[which(names(temp)=="Q5")] <- "HHResponseId"
temp$HHResponseId <- gsub("â€‹", "", temp$HHResponseId)
temp$HHResponseId <- gsub(": ", "", temp$HHResponseId)
temp$HHResponseId <- gsub(" ", "", temp$HHResponseId)
temp$HHResponseId <- gsub("​", "", temp$HHResponseId)
# - NOTE: code removed for confidentiality
# - example: temp$HHResponseId[which(temp$HHResponseId=="R_0123456789abcde")] <- "R_0I23456789abcdE"
# temp$HHResponseId
names(temp)[which(names(temp)=="Q6")] <- "PERWHO"
temp$PERWHO <- factor(temp$PERWHO, levels=c("Usted", "Ã©l/ella"))
levels(temp$PERWHO) <- c("I am filling out my own survey.", "I am filling out the survey for someone else.")
names(temp)[grepl("Q41_", names(temp))] <- gsub("Q41", "T2", names(temp)[grepl("Q41_", names(temp))])
for (i in grep("T2_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Start 3
names(temp)[which(names(temp)=="Q9")] <- "PERNAME"
# - NOTE: code removed for confidentiality
# - example: temp$PERNAME[c(1)] <- c("Patrick")
# temp$PERNAME
names(temp)[which(names(temp)=="Q11")] <- "DATE"
# - NOTE: code removed for confidentiality
# - example: temp$DATE[c(1)] <- "01-30-2019"
# temp$DATE
temp$DATE <- as.Date(temp$DATE, format="%m-%d-%Y", tz="America/Denver")
table(temp$DATE)
names(temp)[grepl("Q42_", names(temp))] <- gsub("Q42", "T3", names(temp)[grepl("Q42_", names(temp))])
for (i in grep("T3_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Start 4
names(temp)[grepl("Q43_", names(temp))] <- gsub("Q43", "T4", names(temp)[grepl("Q43_", names(temp))])
for (i in grep("T4_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Places
temp1 <- temp[,1:(which(names(temp)=="X1_Q14")-1)]
temp2 <- temp[,which(names(temp)=="Q33"):ncol(temp)]
tempX <- temp[,which(names(temp)=="X1_Q14"):(which(names(temp)=="Q33")-1)]
names(tempX)
names(tempX)[grepl("Q14", names(tempX))] <- gsub("Q14", "XPLACE1", names(tempX)[grepl("Q14", names(tempX))])
names(tempX)[grepl("Q15", names(tempX))] <- gsub("Q15", "XPLACE2", names(tempX)[grepl("Q15", names(tempX))])
names(tempX)[grepl("Q16_4", names(tempX))] <- gsub("Q16_4", "XPLACE_NAME", names(tempX)[grepl("Q16_4", names(tempX))])
names(tempX)[grepl("Q16_2", names(tempX))] <- gsub("Q16_2", "XPLACE_STREET", names(tempX)[grepl("Q16_2", names(tempX))])
names(tempX)[grepl("Q16_3", names(tempX))] <- gsub("Q16_3", "XPLACE_CITY", names(tempX)[grepl("Q16_3", names(tempX))])
names(tempX)[grepl("Q52.1_1", names(tempX))] <- gsub("Q52.1_1", "XARR_HOUR", names(tempX)[grepl("Q52.1_1", names(tempX))])
names(tempX)[grepl("Q52.2_1", names(tempX))] <- gsub("Q52.2_1", "XARR_MIN", names(tempX)[grepl("Q52.2_1", names(tempX))])
names(tempX)[grepl("Q52.3_1", names(tempX))] <- gsub("Q52.3_1", "XARR_AMPM", names(tempX)[grepl("Q52.3_1", names(tempX))])
names(tempX)[grepl("Q18_10", names(tempX))] <- gsub("_10", "", names(tempX)[grepl("Q18_10", names(tempX))])
names(tempX)[grepl("Q18", names(tempX))] <- gsub("Q18", "XACTIVITY", names(tempX)[grepl("Q18", names(tempX))])
names(tempX)[grepl("Q19", names(tempX))] <- gsub("Q19", "XLEAVE", names(tempX)[grepl("Q19", names(tempX))])
names(tempX)[grepl("Q54.1_1", names(tempX))] <- gsub("Q54.1_1", "XDEP_HOUR", names(tempX)[grepl("Q54.1_1", names(tempX))])
names(tempX)[grepl("Q54.2_1", names(tempX))] <- gsub("Q54.2_1", "XDEP_MIN", names(tempX)[grepl("Q54.2_1", names(tempX))])
names(tempX)[grepl("Q54.3_1", names(tempX))] <- gsub("Q54.3_1", "XDEP_AMPM", names(tempX)[grepl("Q54.3_1", names(tempX))])
names(tempX)[grepl("Q22_21", names(tempX))] <- gsub("_21", "", names(tempX)[grepl("Q22_21", names(tempX))])
names(tempX)[grepl("Q22", names(tempX))] <- gsub("Q22", "XMODE", names(tempX)[grepl("Q22", names(tempX))])
names(tempX)[grepl("Q55", names(tempX))] <- gsub("Q55", "XPEOPLE", names(tempX)[grepl("Q55", names(tempX))])
names(tempX)[grepl("Q30_12", names(tempX))] <- gsub("_12", "", names(tempX)[grepl("Q30_12", names(tempX))])
names(tempX)[grepl("Q30", names(tempX))] <- gsub("Q30", "XVEHICLE", names(tempX)[grepl("Q30", names(tempX))])
names(tempX)[grepl("Q21_12", names(tempX))] <- gsub("_12", "", names(tempX)[grepl("Q21_12", names(tempX))])
names(tempX)[grepl("Q21", names(tempX))] <- gsub("Q21", "XNOTRIPS", names(tempX)[grepl("Q21", names(tempX))])
names(tempX)[grepl("Q45_", names(tempX))] <- gsub("Q45", "T5", names(tempX)[grepl("Q45_", names(tempX))])
tempX0 <- data.frame(one=cbind(tempX, one=1)[,"one"])
for (j in 1:20) {
  Xx <- paste0("X", j, "_")
  tempXx <- tempX[,grepl(Xx, names(tempX))]
  tempXx[,paste0(Xx, "XPLACE1")] <- factor(tempXx[,paste0(Xx, "XPLACE1")], levels=c("Home", "School", "Primary job", "Secondary job", "Other place"))
  tempXx[,paste0(Xx, "XPLACE2")] <- factor(tempXx[,paste0(Xx, "XPLACE2")], levels=c("Home", "School", "Primary job", "Secondary job", "Bus stop or parking location (not at your destination)", "Other place"))
  # tempXx[,paste0(Xx, "XPLACE_NAME")]
  # tempXx[,paste0(Xx, "XPLACE_STREET")]
  # tempXx[,paste0(Xx, "XPLACE_CITY")]
  tempXx[,paste0(Xx, "XARR_HOUR")] <- as.integer(tempXx[,paste0(Xx, "XARR_HOUR")])
  tempXx[,paste0(Xx, "XARR_MIN")] <- as.integer(tempXx[,paste0(Xx, "XARR_MIN")])
  tempXx[,paste0(Xx, "XARR_AMPM")] <- factor(tempXx[,paste0(Xx, "XARR_AMPM")], levels=c("AM", "PM"))
  tempXx[,paste0(Xx, "XACTIVITY")] <- factor(tempXx[,paste0(Xx, "XACTIVITY")], level=c("Work- or school-related activities", "Eat meal at restaurant", "Service private vehicle (gas, oil, repairs, etc.)", "Shopping (groceries, clothing, convenience store, etc)", "Drop off or pick up passenger(s)", "Civic or religious activities", "Other errands or appointments (bank, professional office, doctor/dentist, etc.)", "Outdoor or indoor exercise (sports, jogging, bicycling, walking dog, gym, etc.)", "Social or entertainment activities (friends/relatives, movie, etc.)", "Other (please specify):"))
  # tempXx[,paste0(Xx, "XACTIVITY_TEXT")]
  tempXx[,paste0(Xx, "XLEAVE")] <- factor(tempXx[,paste0(Xx, "XLEAVE")], levels=c("Yes", "No"))
  tempXx[,paste0(Xx, "XDEP_HOUR")] <- as.integer(tempXx[,paste0(Xx, "XDEP_HOUR")])
  tempXx[,paste0(Xx, "XDEP_MIN")] <- as.integer(tempXx[,paste0(Xx, "XDEP_MIN")])
  tempXx[,paste0(Xx, "XDEP_AMPM")] <- factor(tempXx[,paste0(Xx, "XDEP_AMPM")], levels=c("AM", "PM"))
  tempXx[,paste0(Xx, "XMODE")] <- factor(tempXx[,paste0(Xx, "XMODE")], levels=c("Walk", "Bicycle", "Car/Van/Truck/SUV Driver", "Car/Van/Truck/SUV Passenger", "Motorcycle/Scooter/Moped", "Local Bus (CVTD or Aggie Shuttle)", "School Bus", "Other (please specify)"))
  # tempXx[,paste0(Xx, "XMODE_TEXT")]
  tempXx[,paste0(Xx, "XPEOPLE")] <- factor(tempXx[,paste0(Xx, "XPEOPLE")], levels=c("0 (none)", "1", "2", "3", "4", "5+"))
  tempXx[,paste0(Xx, "XVEHICLE")] <- factor(tempXx[,paste0(Xx, "XVEHICLE")], levels=c("Primary household vehicle", "Other household vehicle (please describe)", "A non-household (someone else's) vehicle"))
  # tempXx[,paste0(Xx, "XVEHICLE_TEXT")]
  tempXx[,paste0(Xx, "XNOTRIPS")] <- factor(tempXx[,paste0(Xx, "XNOTRIPS")], levels=c("Vacation or personal day", "Not scheduled to work", "Sick", "Child or other household member was sick", "Home-bound, elderly, or disabled", "Worked at home (for pay)", "Worked around home (not for pay)", "No transportation available", "No reason to travel", "Weather", "Other (please specify)"))
  # tempXx[,paste0(Xx, "XNOTRIPS_TEXT")]
  for (i in grep("T5_", names(tempXx), value=T)) { tempXx[,i] <- as.numeric(tempXx[,i]) }; rm(i)
  # tempX[,grepl(Xx, names(tempX))] <- tempXx
  tempX0 <- cbind(tempX0, tempXx)
  rm(tempXx, Xx)
}; rm(j)
tempX0$one <- NULL
temp <- cbind(temp1, tempX0, temp2)
rm(temp1, temp2, tempX, tempX0)

# End 1
names(temp)[which(names(temp)=="Q33")] <- "SPECIAL"
names(temp)[which(names(temp)=="Q34_1")] <- "RATE_TRAFFIC"
names(temp)[which(names(temp)=="Q34_2")] <- "RATE_WEATHER"
names(temp)[which(names(temp)=="Q34_3")] <- "RATE_AIRQUAL"
temp$RATE_TRAFFIC <- factor(temp$RATE_TRAFFIC, levels=c("Great", "Good", "Fair", "Bad", "Terrible"))
temp$RATE_WEATHER <- factor(temp$RATE_WEATHER, levels=c("Great", "Good", "Fair", "Bad", "Terrible"))
temp$RATE_AIRQUAL <- factor(temp$RATE_AIRQUAL, levels=c("Great", "Good", "Fair", "Bad", "Terrible"))
names(temp)[which(names(temp)=="Q35")] <- "IDLE"
temp$IDLE <- factor(temp$IDLE, levels=c("Yes", "No"))
names(temp)[which(names(temp)=="Q36_1_1")] <- "IDLE_MIN"
temp$IDLE_MIN[temp$IDLE_MIN=="2 min"] <- 2
temp$IDLE_MIN <- as.integer(temp$IDLE_MIN)
names(temp)[grepl("Q44_", names(temp))] <- gsub("Q44", "T6", names(temp)[grepl("Q44_", names(temp))])
for (i in grep("T6_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# End 2
names(temp)[which(names(temp)=="Q38")] <- "COMMENTS"
names(temp)[grepl("Q45_", names(temp))] <- gsub("Q45", "T7", names(temp)[grepl("Q45_", names(temp))])
for (i in grep("T7_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Other
temp$pername_ask <- NULL
temp$pername_pre <- NULL
temp$pername_ask_2 <- NULL
temp$pername <- NULL
temp$pername_low <- NULL

########################################
# Check HHResponseId, PERNAME

# Load formatted HH and PER files
HH <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "HH.rds"))
PER <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PER.rds"))

# Inspect
str(HH)
str(PER)

# Check HHResponseId & PERNAME
temp$HHPER <- paste(temp$HHResponseId, temp$PERNAME, sep=" ")
PER$HHPER <- paste(PER$ResponseId, PER$NAME, sep=" ")
table(temp$HHPER %in% PER$HHPER) # 2
tempa <- temp[which(!(temp$HHPER %in% PER$HHPER)),] # blanks/incompletes
tempa[,c("HHResponseId", "PERNAME")]
rm(tempa)
temp$HHPER <- NULL
PER$HHPER <- NULL

# Cleanup
rm(HH, PER)

########################################
# Save

# Change names
diary3 <- temp
diary3_names <- temp_names
row.names(diary3) <- NULL

# Inspect
names(diary3)
str(diary3, list.len=ncol(diary3))
summary(diary3)
str(diary3_names)

# Save
t_folder <- "Data 2 Cleaned"
saveRDS(diary3, file=file.path("Data", "Survey 2019 Winter", t_folder, "diary3.rds"))
write.csv(diary3, file=file.path("Data", "Survey 2019 Winter", t_folder, "diary3.csv"), row.names=F)
write.csv(diary3_names, file=file.path("Data", "Survey 2019 Winter", t_folder, "diary3_names.csv"), row.names=F)
rm(t_folder)

########################################
# Cleanup

# Remove
rm(diary3, diary3_names)
rm(temp, temp_names)
rm(old_temp)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################