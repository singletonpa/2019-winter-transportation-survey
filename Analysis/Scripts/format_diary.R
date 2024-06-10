########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     format_diary.R
# About:    Script to format diary surveys
########################################

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
# library("")

########################################
# Load data

# Read rds data file
t_folder <- "Data 2 Cleaned"
diary12 <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "diary12.rds"))
diary3 <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "diary3.rds"))
diary4 <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "diary4.rds"))
diary5 <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "diary5.rds"))
diary6 <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "diary6.rds"))
rm(t_folder)

# Backup
temp12 <- diary12
temp3 <- diary3
temp4 <- diary4
temp5 <- diary5
temp6 <- diary6

# Add diary information
diary12$DIARY <- "DIARY12"
diary3$DIARY <- "DIARY3"
diary4$DIARY <- "DIARY4"
diary5$DIARY <- "DIARY5"
diary6$DIARY <- "DIARY6"

# Inspect
names(diary12)
names(diary3)
names(diary4)
names(diary5)
names(diary6)
str(diary12, list.len=ncol(diary12))
str(diary3, list.len=ncol(diary3))
str(diary4, list.len=ncol(diary4))
str(diary5, list.len=ncol(diary5))
str(diary6, list.len=ncol(diary6))

########################################
# Assemble metadata

# Initialize
temp <- diary12
mycols <- c("ResponseId", "StartDate", "EndDate", "Status", "IPAddress", 
            "Progress", "Duration", "Finished", "RecordedDate", "DIARY", 
            "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", 
            "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage")
mycols <- c(mycols, c("CHECKAGE"))
mycols <- c(mycols, grep("T1_", names(temp), value=T))
mycols <- c(mycols, grep("T2_", names(temp), value=T))
mycols <- c(mycols, grep("T3_", names(temp), value=T))
mycols <- c(mycols, grep("T4_", names(temp), value=T))
mycols <- c(mycols, grep("T5_", names(temp), value=T))
mycols <- c(mycols, grep("T6_", names(temp), value=T))
mycols <- c(mycols, grep("T7_", names(temp), value=T))
rm(temp)

# Notes on T5_ timing questions
# only ended up being displayed for people who did not travel, 
# so only appears for subset of people in X1

# Merge diaries
meta_diary <- data.frame(diary12[,mycols])
meta_diary <- rbind(meta_diary, diary3[,mycols])
meta_diary <- rbind(meta_diary, diary4[,mycols])
meta_diary <- rbind(meta_diary, diary5[,mycols])
meta_diary <- rbind(meta_diary, diary6[,mycols])
rm(mycols)
meta_diary$DIARY <- as.factor(meta_diary$DIARY)

# Inspect
names(meta_diary)
str(meta_diary, list.len=ncol(meta_diary))
summary(meta_diary)

########################################
# Assemble diary data

# Initialize
mycols <- c("ResponseId", "HHResponseId", "PERWHO", "PERNAME", "DATE", "DIARY")
mycols <- c(mycols, c("SPECIAL", "RATE_TRAFFIC", "RATE_WEATHER", "RATE_AIRQUAL", "IDLE", "IDLE_MIN", "COMMENTS"))

# Merge diaries
diary <- data.frame(diary12[,mycols])
diary <- rbind(diary, diary3[,mycols])
diary <- rbind(diary, diary4[,mycols])
diary <- rbind(diary, diary5[,mycols])
diary <- rbind(diary, diary6[,mycols])
rm(mycols)

# Construct
diary$DIARY <- as.factor(diary$DIARY)
## ratings
summary(diary$RATE_TRAFFIC)
summary(diary$RATE_WEATHER)
summary(diary$RATE_AIRQUAL)
## idling
summary(diary$IDLE)
summary(diary$IDLE_MIN)
## text responses
# unique(diary$SPECIAL)
# diary$COMMENTS[diary$COMMENTS!=""]

# Inspect
names(diary)
str(diary, list.len=ncol(diary))
summary(diary)

########################################
# Assemble places_wide data

# Initialize
temp <- diary12
mycols <- c("DIARY", "HHResponseId", "PERNAME", "DATE", "ResponseId")
for (j in 1:20) {
  Xx <- paste0("X", j, "_")
  mycols <- c(mycols, grep(Xx, names(temp), value=T))
  rm(Xx)
}; rm(j)
mycols2 <- grep("T5_", names(temp), value=T)
mycols <- mycols[!(mycols %in% mycols2)]
rm(mycols2, temp)

# Merge diaries
places <- data.frame(diary12[,mycols])
places <- rbind(places, diary3[,mycols])
places <- rbind(places, diary4[,mycols])
places <- rbind(places, diary5[,mycols])
places <- rbind(places, diary6[,mycols])
rm(mycols)
places$DIARY <- as.factor(places$DIARY)

# Rename
places_wide <- places
rm(places)

# Inspect
names(places_wide)
str(places_wide, list.len=ncol(places_wide))
summary(places_wide)

########################################
# Assemble places data (initial)

# Initialize
mycols <- c("DIARY", "HHResponseId", "PERNAME", "DATE", "ResponseId", "PLANO")
mycols1 <- grep("X1_", names(places_wide), value=T)
mycols2 <- grep("T5_", mycols1, value=T)
mycols1 <- mycols1[!(mycols1 %in% mycols2)]
mycols0 <- gsub("X1_X", "", mycols1)
rm(mycols1, mycols2)

# Initialize
places_wide$PLANO <- 0L
colsrn <- which(c(mycols, mycols0) %in% mycols0)
i <- 1
j <- 1
Xx <- paste0("X", j, "_X")
t0 <- data.frame(places_wide[i,c(mycols,paste0(Xx,mycols0))])
names(t0)[colsrn] <- mycols0
temp <- t0[0,]
rm(i,j,Xx,t0)

# While loop
# for (i in 1:500) {
for (i in 1:nrow(places_wide)) {
  j <- 1L
  leave <- ""
  while ((j <= 20) & (leave != "No")) {
    Xx <- paste0("X", j, "_X")
    t1 <- data.frame(places_wide[i,c(mycols,paste0(Xx,mycols0))])
    t1$PLANO <- j
    names(t1)[colsrn] <- mycols0
    # if (is.na(t1$LEAVE)) { t1$LEAVE <- "Yes"} # this is a problem
    if (sum(is.na(t1[,mycols0]) | t1[,mycols0]=="") < length(mycols0)) {
      leave <- as.character(t1$LEAVE)
      if (is.na(leave)) { leave <- "Yes" }
      temp <- rbind(temp, t1)
    }
    rm(Xx,t1)
    j <- j + 1L
  }
  rm(j, leave)
}; rm(i)
places_wide$PLANO <- NULL
rm(colsrn)

# Check
table(table(temp$ResponseId))
# View(temp[temp$ResponseId %in% temp[temp$PLANO==20,"ResponseId"],])

# Rename
places <- temp
rm(temp, mycols, mycols0)

# Inspect
names(places)
str(places, list.len=ncol(places))
summary(places)

########################################
# Edit DATE in diary and places_wide

# Rename
temp <- diary

# Edit missing/wrong dates
temp$DATE_ORIG <- temp$DATE
table(temp$DATE)
table(is.na(temp$DATE))

# Create target date of diary
temp$DATE_DIARY <- ifelse(temp$DIARY=="DIARY12", "2019-01-14", 
                    ifelse(temp$DIARY=="DIARY3", "2019-01-29", ifelse(temp$DIARY=="DIARY4", "2019-01-30", 
                    ifelse(temp$DIARY=="DIARY5", "2019-02-21", ifelse(temp$DIARY=="DIARY6", "2019-02-22", NA)))))
temp$DATE_DIARY <- as.Date(temp$DATE_DIARY, tz="America/Denver")

# Create submit time
temp$EndDate <- meta_diary$EndDate
temp$Progress <- meta_diary$Progress

# Create time of last trip
# temp[,c("ARR_HOUR", "ARR_MIN", "ARR_AMPM")] <- list(0L, 0L, "")
temp[,c("ARR_HOUR", "ARR_MIN", "ARR_AMPM")] <- list(NA, NA, NA)
# append last trip time
for (i in 1:nrow(temp)) {
  tplace <- places[places$ResponseId==temp$ResponseId[i],]
  tplace <- tplace[max(tplace$PLANO),]
  tplace$ARR_AMPM <- as.character(tplace$ARR_AMPM)
  temp[i,c("ARR_HOUR", "ARR_MIN", "ARR_AMPM")] <- tplace[,c("ARR_HOUR", "ARR_MIN", "ARR_AMPM")]
  rm(tplace)
}; rm(i)
table(is.na(temp$ARR_HOUR)); table(is.na(temp$ARR_MIN)); table(is.na(temp$ARR_AMPM))
# create last trip day/time
temp$ARR_TIME <- paste(temp$DATE, paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME <- as.POSIXct(temp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
summary(temp$ARR_TIME)
# temp[,c("ARR_HOUR", "ARR_MIN", "ARR_AMPM")] <- NULL

# Summary of checks for missing/wrong dates
# a. missing DATE
# b. with DATE after submit date
# c. with DATE before diary date
# d. with DATE >= 7 days after diary date
# e. with DATE >= 7 days before submit date
# f. with submit date/time > 1 hour before last trip date/time
# g. duplicated HH, PER, DATE

# Edit DATE

# a. (47) missing DATE
tr1 <- unique(temp$HHResponseId[is.na(temp$DATE)])
# assume target date if only one missing and submitted late on that day
# assume target date if later but tend to submit for earlier day
# assume submit date if late on that day, or if tend to submit for day of
# - NOTE: code removed for confidentiality
# - example: temp$DATE[temp$ResponseId=="R_0123456789abcde"] <- as.Date("2019-01-30", tz="America/Denver")
# of 47, 1 all NAs, 14 incomplete (22% progress), 2 unable to be guessed
# temp[temp$HHResponseId==tr1[1],]
rm(tr1)
# re-create last trip day/time
temp$ARR_TIME <- paste(temp$DATE, paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME <- as.POSIXct(temp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$TDIFF <- difftime(temp$EndDate, temp$ARR_TIME, units="min")

# b. (8) with DATE after submit date
tr4 <- unique(temp$HHResponseId[temp$DATE > as.Date(temp$EndDate, tz="America/Denver")])
# change if clear to do so
# - NOTE: code removed for confidentiality
# - example: temp$DATE[temp$ResponseId=="R_0123456789abcde"] <- as.Date("2019-01-30", tz="America/Denver") # one week earlier
# of 8, 1 all NAs
# temp[temp$HHResponseId==tr4[1],]
rm(tr4)
# re-create last trip day/time
temp$ARR_TIME <- paste(temp$DATE, paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME <- as.POSIXct(temp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$TDIFF <- difftime(temp$EndDate, temp$ARR_TIME, units="min")

# c.(23) with DATE before diary date
table(difftime(temp$DATE, temp$DATE_DIARY, units="days"))
tr2 <- unique(temp$HHResponseId[temp$DATE < temp$DATE_DIARY])
# don't change unless have a compelling reason
# e.g., duplicate dates, typically fills out for diary date
# - NOTE: code removed for confidentiality
# - example: temp$DATE[temp$ResponseId=="R_0123456789abcde"] <- as.Date("2019-01-30", tz="America/Denver") # previous day
# of 23, 1 all NAs, 13 didn't change, 3 changed partially, 6 changed
# temp[temp$HHResponseId==tr2[1],]
rm(tr2)
# re-create last trip day/time
temp$ARR_TIME <- paste(temp$DATE, paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME <- as.POSIXct(temp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$TDIFF <- difftime(temp$EndDate, temp$ARR_TIME, units="min")

# d. (18) with DATE >= 7 days after diary date
table(difftime(temp$DATE, temp$DATE_DIARY, units="days"))
tr3 <- unique(temp$HHResponseId[temp$DATE >= (temp$DATE_DIARY + ifelse(temp$DIARY=="DIARY12", 8, 7))])
# don't change unless have a compelling reason
# of 18, 1 all NAs, 17 didn't change
# temp[temp$HHResponseId==tr3[1],]
rm(tr3)
# re-create last trip day/time
temp$ARR_TIME <- paste(temp$DATE, paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME <- as.POSIXct(temp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$TDIFF <- difftime(temp$EndDate, temp$ARR_TIME, units="min")

# e. (47) with DATE >= 7 days before submit date
table(difftime(temp$DATE, as.Date(temp$EndDate, tz="America/Denver"), units="days"))
tr5 <- unique(temp$HHResponseId[temp$DATE <= (as.Date(temp$EndDate, tz="America/Denver") - 7)])
# don't change unless have a compelling reason
# - NOTE: code removed for confidentiality
# - example: temp$DATE[temp$ResponseId=="R_0123456789abcde"] <- as.Date("2019-01-30", tz="America/Denver") # same day as partner
# of 47, 1 all NAs, 44 didn't change, 1 changed
# temp[temp$HHResponseId==tr5[1],]
rm(tr5)
# re-create last trip day/time
temp$ARR_TIME <- paste(temp$DATE, paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME <- as.POSIXct(temp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$TDIFF <- difftime(temp$EndDate, temp$ARR_TIME, units="min")

# f. (45) with submit date/time > 1 hour before last trip date/time
sort(temp$TDIFF[temp$TDIFF<(-60)])
# tr6 <- unique(temp$HHResponseId[temp$EndDate < temp$ARR_TIME])
tr6 <- unique(temp$HHResponseId[temp$TDIFF<(-60)])
# don't change unless have a compelling reason
# e.g., filled out after midnight or morning on next day
# lots of people filled out during an evening event (e.g., church), so didn't change
# lots of people filled out for someone else before they got home, also didn't change
# - NOTE: code removed for confidentiality
# - example: temp$DATE[temp$ResponseId=="R_0123456789abcde"] <- as.Date("2019-01-30", tz="America/Denver")
# of 45, 1 all NAs, 36 didn't change, 1 changed partially, 7 changed
# temp[temp$HHResponseId==tr6[1],]
rm(tr6)
# re-create last trip day/time
temp$ARR_TIME <- paste(temp$DATE, paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME <- as.POSIXct(temp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$TDIFF <- difftime(temp$EndDate, temp$ARR_TIME, units="min")

# g. (43) duplicated HH, PER, DATE
tr <- unique(temp$HHResponseId[duplicated(temp[,c("HHResponseId", "PERNAME", "DATE")])])
# many are filled out on next day
# - NOTE: code removed for confidentiality
# - example: temp$DATE[temp$ResponseId=="R_0123456789abcde"] <- as.Date("2019-01-30", tz="America/Denver")
# of 43, 1 all NAs, 29 have duplicates or incompletes (remove later), 11 changed, 2 unable to be guessed
temp[temp$HHResponseId==tr[1],]
rm(tr)
# re-create last trip day/time
temp$ARR_TIME <- paste(temp$DATE, paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME <- as.POSIXct(temp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$TDIFF <- difftime(temp$EndDate, temp$ARR_TIME, units="min")

# Remove unnecessary rows
temp[,c("DATE_DIARY", "EndDate", "Progress", "ARR_HOUR", "ARR_MIN", "ARR_AMPM", "ARR_TIME", "TDIFF")] <- NULL

# Check Joshua Ward comments/edits
# View(temp[temp$DIARY=="DIARY12",])
# View(temp[temp$DIARY=="DIARY3",])
# View(temp[temp$DIARY=="DIARY4",])
# View(temp[temp$DIARY=="DIARY5",])
# View(temp[temp$DIARY=="DIARY6",])
# checks
# - Don't remove duplicate?
# - Don't remove incomplete?
# - Remove duplicate?
# - Remove incomplete? 
# - Double-check date?
# - Other reasons?
# temp[temp$ResponseId=="",]
# temp[temp$HHResponseId=="",]
# places[places$ResponseId=="", ]
# View(places[places$HHResponseId=="", ])

# Check reasons to remove diaries
# a. incomplete progress
table(meta_diary$Progress)
# View(temp[meta_diary$Progress<100,])
# 11 = only clicked through the first page
# 22 = maybe name, no date or trips
# 32 = name and date, no trips
# 38 = some trips but incomplete
# 89 = answered all questions, didn't click submit on last page
tr_inc <- temp$ResponseId[meta_diary$Progress<89]
# - NOTE: code removed for confidentiality
# - example: tr_inc <- c(tr_inc, "R_0123456789abcde"")
# b. duplicate (from g.)
# - NOTE: code removed for confidentiality
# - example: tr_dup <- c("R_0123456789abcde")
# c. withdrawn
# - NOTE: code removed for confidentiality
# - example: tr_wth <- c("R_0123456789abcde")
# d. other reason
# - NOTE: code removed for confidentiality
# - example: tr_oth <- c("R_0123456789abcde") # missing date, cannot guess
# append to temp
temp$REMOVE <- ""
temp$REMOVE[temp$ResponseId %in% tr_inc] <- "incomplete"
temp$REMOVE[temp$ResponseId %in% tr_dup] <- "duplicate"
temp$REMOVE[temp$ResponseId %in% tr_oth] <- "other"
temp$REMOVE[temp$ResponseId %in% tr_wth] <- "withdraw"
# cleanup
rm(tr_inc, tr_dup, tr_oth, tr_wth)

# Rename
diary <- temp
rm(temp)

# Inspect
names(diary)
str(diary, list.len=ncol(diary))
summary(diary)

# Fix date in other files
places_wide$DATE <- diary$DATE

########################################
# Assemble places data (again)

# Initialize
mycols <- c("DIARY", "HHResponseId", "PERNAME", "DATE", "ResponseId", "PLANO")
mycols1 <- grep("X1_", names(places_wide), value=T)
mycols2 <- grep("T5_", mycols1, value=T)
mycols1 <- mycols1[!(mycols1 %in% mycols2)]
mycols0 <- gsub("X1_X", "", mycols1)
rm(mycols1, mycols2)

# Initialize
places_wide$PLANO <- 0L
colsrn <- which(c(mycols, mycols0) %in% mycols0)
i <- 1
j <- 1
Xx <- paste0("X", j, "_X")
t0 <- data.frame(places_wide[i,c(mycols,paste0(Xx,mycols0))])
names(t0)[colsrn] <- mycols0
temp <- t0[0,]
rm(i,j,Xx,t0)

# While loop
# for (i in 1:500) {
for (i in 1:nrow(places_wide)) {
  j <- 1L
  leave <- ""
  while ((j <= 20) & (leave != "No")) {
    Xx <- paste0("X", j, "_X")
    t1 <- data.frame(places_wide[i,c(mycols,paste0(Xx,mycols0))])
    t1$PLANO <- j
    names(t1)[colsrn] <- mycols0
    # if (is.na(t1$LEAVE)) { t1$LEAVE <- "Yes"} # this is a problem
    if (sum(is.na(t1[,mycols0]) | t1[,mycols0]=="") < length(mycols0)) {
      leave <- as.character(t1$LEAVE)
      if (is.na(leave)) { leave <- "Yes" }
      temp <- rbind(temp, t1)
    }
    rm(Xx,t1)
    j <- j + 1L
  }
  rm(j, leave)
}; rm(i)
places_wide$PLANO <- NULL
rm(colsrn)

# Check
table(table(temp$ResponseId))
# View(temp[temp$ResponseId %in% temp[temp$PLANO==20,"ResponseId"],])

# Rename
places <- temp
rm(temp, mycols, mycols0)

# Inspect
names(places)
str(places, list.len=ncol(places))
summary(places)

########################################
# Edit places

# Rename
temp <- places

# Merge PLACE1 & PLACE2 --> PLACE
temp$PLACE2[!is.na(temp$PLACE1)] <- temp$PLACE1[!is.na(temp$PLACE1)]
summary(temp$PLACE2)
temp$PLACE1 <- NULL
names(temp)[which(names(temp)=="PLACE2")]  <- "PLACE"

# Fix missing PLACE
levels(temp$PLACE)
summary(temp$PLACE)
table(is.na(temp$PLACE))
tr <- unique(temp$ResponseId[is.na(temp$PLACE)])
# assume: missing last PLACES are Home, other missing are Other place, unless context determines
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==1, c("PLACE")] <- list("Other place")
# only 1 of 35 were unable to be guessed
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Merge PLACE with ACTIVITY
table(temp$PLACE, is.na(temp$ACTIVITY))
temp$ACT <- temp$ACTIVITY # create temp
# add activities from PLACE
levels(temp$ACTIVITY) <- c(levels(temp$ACT), "Home activities", "Work activities", "School activities", "Change mode")
temp$ACTIVITY[temp$PLACE=="Home"] <- "Home activities"
temp$ACTIVITY[temp$PLACE %in% c("Primary job", "Secondary job")] <- "Work activities"
temp$ACTIVITY[temp$PLACE=="School"] <- "School activities"
temp$ACTIVITY[temp$PLACE=="Bus stop or parking location (not at your destination)"] <- "Change mode"
table(temp$PLACE, is.na(temp$ACTIVITY))
summary(temp$ACTIVITY)
# reorder levels of ACTIVITY
myacts <- c("Home activities", "Work activities", "School activities", "Change mode", levels(temp$ACT))
temp$ACTIVITY <- as.character(temp$ACTIVITY)
temp$ACTIVITY <- factor(temp$ACTIVITY, levels = myacts)
summary(temp$ACTIVITY)
temp$ACT <- NULL # remove temp
rm(myacts)

# Fix missing ACTIVITY
levels(temp$ACTIVITY)
levels(temp$PLACE)
summary(temp$ACTIVITY)
table(is.na(temp$ACTIVITY))
tr <- unique(temp$ResponseId[is.na(temp$ACTIVITY)])
# assigned based on place name and/or activity duration
# Work- or school-related activities: Research greenhouse
# Shopping (groceries, clothing, convenience store, etc): Walmart, papa murphys, Store
# Other errands or appointments (bank, professional office, doctor/dentist, etc.): errands downtown
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2, c("ACTIVITY")] <- list("Shopping (groceries, clothing, convenience store, etc)")
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO %in% c(2,3), c("PLACE", "ACTIVITY")] <- list("Bus stop or parking location (not at your destination)", "Change mode")
# of the 17, 1 was all NA, 8 had no PLACE_NAME so left as NA b/c unknown
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Fix ACTIVITY_TEXT
levels(temp$ACTIVITY)
summary(temp$ACTIVITY)
# check ACTIVITY_TEXT
table(temp$ACTIVITY_TEXT)
unique(temp$ACTIVITY_TEXT)
# tr <- unique(temp$ResponseId[temp$ACTIVITY_TEXT!=""])
# tr <- unique(temp$ResponseId[temp$ACTIVITY=="Other (please specify):"])
# temp[temp$ACTIVITY_TEXT=="",]
# temp[temp$ResponseId=="",]
# View(temp[temp$HHResponseId=="",])
# O0: Other change (based on editing activities)
# most of this is for change mode activities (ACTIVITY edited later)
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2, c("PLACE")] <- list("Bus stop or parking location (not at your destination)")
# P1: Home activities
# including spending night at friend or family, or out of town at a hotel
# - NOTE: code removed for confidentiality, example: 
tp1 <- c("Eat and sleep", "Eat, Sleep and Study", "spent the night", "Cooked dinner", "Out of town")
# P2: Work activities
tp2 <- c()
# P3: School activities
tp3 <- c()
# P4: Change mode
# includes buses, rental car, airports
# - NOTE: code removed for confidentiality, example: 
tp4 <- c("Car rental", "Changed busses", "Change planes")
# A1: Work- or school-related activities
# includes volunteer work, classes, events at school
# - NOTE: code removed for confidentiality, example: 
ta1 <- c("Field trip ", "Parent teacher conference", "Rehearsal", "Volunteer work")
# A2: Eat meal at restaurant
# includes spending longer time at a restaurant
# - NOTE: code removed for confidentiality, example: 
ta2 <- c("Write", "eat lunch")
# A3: Service private vehicle (gas, oil, repairs, etc.)
# - NOTE: code removed for confidentiality, example: 
ta3 <- c("car wash", "oil change")
# A4: Shopping (groceries, clothing, convenience store, etc)
# include donations to Deseret; drive-thru food, pickup food from restaurants; post office; other shopping
# some drop-offs and pick-ups when at a store
# - NOTE: code removed for confidentiality, example: 
ta4 <- c("drop off donations", "Fill the car with gas and get groceries", "Drive thru", "Pick up food", "mailed packages")
# A5: Drop off or pick up passenger(s)
# includs people and pets (not other objects)
# - NOTE: code removed for confidentiality, example: 
ta5 <- c("Drop off dog", "pick up pet", "Pick up child")
# A6: Civic or religious activities
# includes public meetings and religious activities at churches
# - NOTE: code removed for confidentiality, example: 
ta6 <- c("meetings", "worship")
# A7: Other errands or appointments (bank, professional office, doctor/dentist, etc.)
# includes lessons, medical visits, and services (including appointments for others like kids), library
# temp[grep("Library", temp$PLACE_NAME),]; temp[grep("library", temp$PLACE_NAME),]
# includes most drop-offs and pick-ups of items (not people) (not at a store)
# - NOTE: code removed for confidentiality, example: 
ta7 <- c("banking", "piano lessons", "Appointment ", "drop off an item", "Pick up item", "return book", "Haircut")
# A8: Outdoor or indoor exercise (sports, jogging, bicycling, walking dog, gym, etc.)
# - NOTE: code removed for confidentiality, example: 
ta8 <- c("Excercise dogs", "Swimming lessons")
# A9: Social or entertainment activities (friends/relatives, movie, etc.)
# includes babysitting for family, visits to friends and family (including hospitals), funerals, birthday parties
# includes museums, sporting events, outdoor enjoyment with others
# - NOTE: code removed for confidentiality, example: 
ta9 <- c("Eat lunch", "Visit a museum ", "babysitting", "birthday party", "Funeral", "visit sister")
# keep as "Other (please specify):"
# - NOTE: code removed for confidentiality
ta0 <- c()
# edit ACTIVITY based on ACTIVITY_TEXT
temp[temp$ACTIVITY_TEXT %in% tp1, c("ACTIVITY")] <- "Home activities"
temp[temp$ACTIVITY_TEXT %in% tp2, c("ACTIVITY")] <- "Work activities"
temp[temp$ACTIVITY_TEXT %in% tp3, c("ACTIVITY")] <- "School activities"
temp[temp$ACTIVITY_TEXT %in% tp4, c("ACTIVITY")] <- "Change mode"
temp[temp$ACTIVITY_TEXT %in% ta1, c("ACTIVITY")] <- "Work- or school-related activities"
temp[temp$ACTIVITY_TEXT %in% ta2, c("ACTIVITY")] <- "Eat meal at restaurant"
temp[temp$ACTIVITY_TEXT %in% ta3, c("ACTIVITY")] <- "Service private vehicle (gas, oil, repairs, etc.)"
temp[temp$ACTIVITY_TEXT %in% ta4, c("ACTIVITY")] <- "Shopping (groceries, clothing, convenience store, etc)"
temp[temp$ACTIVITY_TEXT %in% ta5, c("ACTIVITY")] <- "Drop off or pick up passenger(s)"
temp[temp$ACTIVITY_TEXT %in% ta6, c("ACTIVITY")] <- "Civic or religious activities"
temp[temp$ACTIVITY_TEXT %in% ta7, c("ACTIVITY")] <- "Other errands or appointments (bank, professional office, doctor/dentist, etc.)"
temp[temp$ACTIVITY_TEXT %in% ta8, c("ACTIVITY")] <- "Outdoor or indoor exercise (sports, jogging, bicycling, walking dog, gym, etc.)"
temp[temp$ACTIVITY_TEXT %in% ta9, c("ACTIVITY")] <- "Social or entertainment activities (friends/relatives, movie, etc.)"
rm(tp1, tp2, tp3, tp4, ta1, ta2, ta3, ta4, ta5, ta6, ta7, ta8, ta9, ta0)
# check again, for missing
tr <- unique(temp$ResponseId[temp$ACTIVITY=="Other (please specify):"])
# assigned based on place name
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==1, c("ACTIVITY")] <- list("Civic or religious activities")
# of 9, 1 all NAs, 3 unable to be assigned
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Fix missing MODE
levels(temp$MODE)
# a. with LEAVE=="Yes"
table(temp$LEAVE, is.na(temp$MODE))
tr <- unique(temp$ResponseId[temp$LEAVE=="Yes" & is.na(temp$MODE)])
# assume same mode if in a trip chain, similar mode if used before/after, return mode
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2, c("MODE", "VEHICLE")] <- list("Car/Van/Truck/SUV Passenger", "Primary household vehicle")
# of the 7, 1 was all NA, 1 was unable to be guessed
# temp[temp$ResponseId==tr[1],]
rm(tr)
# b. with is.na(LEAVE)
table(is.na(temp$LEAVE), is.na(temp$MODE))
tr <- unique(temp$ResponseId[is.na(temp$LEAVE) & is.na(temp$MODE)])
# assume driver if VEHICLE==Primary household vehicle, other travel patterns
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2, c("MODE")] <- list("Car/Van/Truck/SUV Driver")
# of the 18, 1 with mostly NA, 5 unable to be guessed
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Fix MODE_TEXT
levels(temp$MODE)
summary(temp$MODE)
levels(temp$VEHICLE)
unique(temp$MODE_TEXT)
tr <- unique(temp$ResponseId[temp$MODE_TEXT!=""])
# change to Walk: Running
# change to Bicycle: 
# change to Car/Van/Truck/SUV Driver: Minivan, Church activity, Store, Uhaul, UTA Commuter Minivan (vanpool) [when 0 other people]
# change to Car/Van/Truck/SUV Passenger: Work shuttle, Work Shuttle, Lyft Driver, Cab
# change to Motorcycle/Scooter/Moped: 
# change to Local Bus (CVTD or Aggie Shuttle): 
# change to School Bus: 
# keep as Other: Airplane, Touring bus, CVTD special event bus
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO %in% c(1,2), c("MODE", "MODE_TEXT", "VEHICLE", "VEHICLE_TEXT")] <- list("Car/Van/Truck/SUV Passenger", "", "A non-household (someone else's) vehicle", "Work shuttle")
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==1, c("MODE")] <- list("Walk")
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2, c("MODE_TEXT")] <- list("Airplane")
# of 22, 9 were kept as Other
# temp[temp$ResponseId==tr[1],]
rm(tr)
unique(temp$MODE_TEXT)

# Fix PEOPLE
levels(temp$PEOPLE)
summary(temp$PEOPLE)
# a. with LEAVE=="Yes"
table(temp$LEAVE, is.na(temp$PEOPLE))
tr <- unique(temp$ResponseId[temp$LEAVE=="Yes" & is.na(temp$PEOPLE)])
# assumed same if in a trip chain, unless other indicators (school drop-off/pick-up)
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2, c("PEOPLE")] <- list("1")
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==3, c("PEOPLE")] <- list("0 (none)")
# of 32, 1 is all NA, only 1 unable to be determined
# temp[temp$ResponseId==tr[1],]
# View(temp[temp$HHResponseId=="",])
rm(tr)
# b. with is.na(LEAVE)
table(is.na(temp$LEAVE), is.na(temp$PEOPLE))
tr <- unique(temp$ResponseId[is.na(temp$LEAVE) & is.na(temp$PEOPLE)])
# assumed same if in a trip chain, unless other indicators (school drop-off/pick-up)
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO %in% c(2,4), c("PEOPLE")] <- list("0 (none)")
# of 16, 8 unable to be guessed
# temp[temp$ResponseId==tr[1],]
# View(temp[temp$HHResponseId=="",])
rm(tr)
# c. with auto passenger but PEOPLE==0
table(temp$MODE=="Car/Van/Truck/SUV Passenger", temp$PEOPLE)
tr <- unique(temp$ResponseId[temp$MODE=="Car/Van/Truck/SUV Passenger" & temp$PEOPLE=="0 (none)"])
# don't change unless good reason: trip chain (assumed actually driver), other HH member traveling too
# good reasons to keep at 0: work shuttle, cab
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2, c("MODE")] <- list("Car/Van/Truck/SUV Driver")
# of 30, 1 all NAs, several changed, some unable to be guessed
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Fix missing VEHICLE
levels(temp$VEHICLE)
summary(temp$VEHICLE)
tr <- unique(temp$ResponseId[temp$MODE %in% c("Car/Van/Truck/SUV Driver", "Car/Van/Truck/SUV Passenger") & is.na(temp$VEHICLE)])
# assume same as trip chain, or similar other days
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2, c("VEHICLE")] <- list("Primary household vehicle")
# of 15, 1 mostly NAs, 1 unable to be guessed
# temp[temp$ResponseId==tr[1],]
# View(temp[temp$HHResponseId=="",])
rm(tr)

# Fix VEHICLE_TEXT
unique(temp$VEHICLE_TEXT)
table(temp$VEHICLE, temp$VEHICLE_TEXT=="")
tr <- unique(temp$ResponseId[temp$VEHICLE=="Other household vehicle (please describe)" & temp$VEHICLE_TEXT==""])
# assumed based on trip chain or similar days
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2, c("VEHICLE_TEXT")] <- list("Pickup truck")
# of 52, 1 all NAs, 24 unable to be guessed
# temp[temp$ResponseId==tr[1],]
# View(temp[temp$HHResponseId=="",])
rm(tr)

# Fix non-matching vehicle names
# decided not to do this. 
# Also decided not to try to match
# "primary" and "secondary" vehicles
# in PLACE with data from VEH
# because too many missing or unable to guess. 
# See: "format_diary_veh.R"

# Fix NOTRIPS, NOTRIPS_TEXT
levels(temp$NOTRIPS)
summary(temp$NOTRIPS)
table(is.na(temp$NOTRIPS), temp$PLANO==1 & temp$LEAVE=="No")
# a response for all instances
unique(temp$NOTRIPS_TEXT)
# tr <- unique(temp$ResponseId[temp$NOTRIPS_TEXT!=""])
tr <- unique(temp$ResponseId[temp$NOTRIPS=="Other (please specify)"])
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==1, c("NOTRIPS")] <- list("No reason to travel")
# of 32, 1 all NAs, 6 out-of-town, 1 at work all day, 4 other or unknown reasons
#        1 didn't think they needed to log travel to work and back home
#        16 maybe misunderstood questions and said they were at home sleeping (R_2TtaXBoqy1gAVut)
# temp[temp$ResponseId==tr[1],]
# View(temp[temp$HHResponseId=="",])
rm(tr)

# Fix LEAVE
levels(temp$LEAVE)
summary(temp$LEAVE)
tr <- unique(temp$ResponseId[is.na(temp$LEAVE)])
# assume Yes if middle of a sequence of trips, first trip but missing PLANOs
# don't edit if missing PLANOs (in the middle) or NA on last trip
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2, c("LEAVE")] <- list("Yes")
# of 53, 49 changed, 2 to be changed later, 2 unable to changed
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Rename
places <- temp
rm(temp)

# Inspect
names(places)
str(places, list.len=ncol(places))
summary(places)

########################################
# Edit places some more

# Rename
temp <- places

# Inspect comments in SPECIAL and COMMENTS
# tr <- unique(diary$ResponseId[diary$SPECIAL!=""])
# View(diary[diary$SPECIAL!="",])
# tr <- unique(diary$ResponseId[diary$COMMENTS!=""])
# View(diary[diary$COMMENTS!="",c(names(diary)[1:6],"COMMENTS")])
# diary[diary$ResponseId=="",]
# temp[temp$ResponseId=="",]
# View(temp[temp$HHResponseId=="",])

# Edits from SPECIAL
# - NOTE: code removed for confidentiality
# - example: # R_0123456789abcde # edit places to add missing trip
# - example: temp <- rbind(temp, list("DIARY12", "R_0123456789abcde", "Patrick", "2019-01-30", "R_0123456789abcde", 2L, "Home", "", "", "", NA, NA, NA, "Home activities", "", "No", NA, NA, NA, NA, "", NA, NA, "", NA, ""))

# Edits from COMMENTS
# - NOTE: code removed for confidentiality
# - example: # R_0123456789abcde # edit places to fix wrong PLACE for second place
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2L, c("PLACE", "PLACE_NAME", "ACTIVITY")] <- list("Other place", "someone else's home", "Drop off or pick up passenger(s)")

# Edits from Joshua Ward
# - NOTE: code removed for confidentiality
# - example: # HH: R_0123456789abcde # DIARY12 merge? (R_0123456789abcde and R_0123456789abcde)
# - example: # actually just remove the duplicates
# - example: diary[diary$ResponseId=="R_0123456789abcde", c("REMOVE")] <- list("duplicate")
# - example: diary[diary$ResponseId=="R_0123456789abcde", c("REMOVE")] <- list("duplicate")

# Check start/end not at home
# a. first place != Home
table(temp$PLANO==1 & temp$PLACE!="Home")
tr1 <- unique(temp$ResponseId[temp$PLANO==1 & temp$PLACE!="Home"])
# b. last place != Home
table(temp$LEAVE=="No" & temp$PLACE!="Home")
tr2 <- unique(temp$ResponseId[temp$LEAVE=="No" & temp$PLACE!="Home"])
# combine
tr <- unique(c(tr1, tr2))
rm(tr1, tr2)
# assumed okay if an someone else's home, hospital, job in the morning, etc. 
# used to check if out-of-town (out of Northern Utah region for entire day)
# -- must be beyond greater logan and salt lake area
# -- must be gone for the entire day
# of 133, 1 all NAs, 1 mostly NAs
# temp[temp$ResponseId==tr[1],]
rm(tr)
# c. last place LEAVE != "No"
tr1 <- unique(temp$ResponseId[temp$LEAVE=="No"])
tr2 <- unique(temp$ResponseId[temp$LEAVE=="Yes"])
tr <- unique(tr2[!(tr2 %in% tr1)])
rm(tr1, tr2)
# most seem to have missing last trips
# of 22, 1 mostly NAs
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Missing PLANO in the middle
tempa <- temp[order(temp$HHResponseId, temp$DIARY, temp$ResponseId, temp$PLANO),]
tempa$PLANONEXT <- 0L
tempa$PLANONEXT[1:(nrow(tempa)-1)] <- tempa$PLANO[2:nrow(tempa)]
tempa$PLANODIFF <- tempa$PLANONEXT - tempa$PLANO
summary(tempa$PLANODIFF)
table(tempa$PLANODIFF)
tr <- unique(tempa$ResponseId[tempa$PLANODIFF > 1])
rm(tempa)
# many seem to be missing just one trip in the middle
# of 11, 1 almost all NAs
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Add more remove reasons
table(diary$REMOVE)
# Out-of-town all-day
# - NOTE: code removed for confidentiality
# - example: tr_out <- c("R_0123456789abcde")
# Missing places
# - NOTE: code removed for confidentiality
# - example: tr_mis <- c("R_0123456789abcde")
# Skips places, may be incomplete
# - NOTE: code removed for confidentiality
# - example: tr_skp <- c("R_0123456789abcde")
# append to diary
diary$REMOVE[diary$ResponseId %in% tr_out] <- ifelse(diary$REMOVE[diary$ResponseId %in% tr_out]=="", "out-of-town", diary$REMOVE[diary$ResponseId %in% tr_out])
diary$REMOVE[diary$ResponseId %in% tr_mis] <- ifelse(diary$REMOVE[diary$ResponseId %in% tr_mis]=="", "missing place", diary$REMOVE[diary$ResponseId %in% tr_mis])
diary$REMOVE[diary$ResponseId %in% tr_skp] <- ifelse(diary$REMOVE[diary$ResponseId %in% tr_skp]=="", "skipped place", diary$REMOVE[diary$ResponseId %in% tr_skp])
rm(tr_out, tr_mis, tr_skp)
table(diary$REMOVE)

# Find other reasons to remove trips
# missing diary information
summary(diary)
diary[diary$HHResponseId=="",]
diary[is.na(diary$PERWHO),]
diary[diary$PERNAME=="",]
diary[is.na(diary$DATE),]
# - NOTE: code removed for confidentiality
# - example: diary$REMOVE[diary$ResponseId=="R_0123456789abcde"] <- c("unknown date")
# missing place information
summary(temp)
diary[diary$ResponseId %in% temp$ResponseId[is.na(temp$PLACE)],]
diary[diary$ResponseId %in% temp$ResponseId[is.na(temp$ACTIVITY)],]
# - NOTE: code removed for confidentiality
# - example: tr <- c("R_0123456789abcde")
# - example: diary$REMOVE[diary$ResponseId %in% tr] <- c("unknown activity")
# - example: rm(tr)
diary[diary$ResponseId %in% temp$ResponseId[is.na(temp$LEAVE)],]
diary[diary$ResponseId %in% temp$ResponseId[is.na(temp$MODE) & temp$LEAVE=="Yes"],]
# - NOTE: code removed for confidentiality
# - example: diary$REMOVE[diary$ResponseId=="R_0123456789abcde"] <- c("unknown mode")
# inspect
table(diary$REMOVE)

# Rename
places <- temp
rm(temp)

# Inspect
names(places)
str(places, list.len=ncol(places))
summary(places)

########################################
# Add locations to places

# Load RData files
HH <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "HH.rds"))
PER <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PER.rds"))

# Inspect
names(HH); names(PER)
summary(HH[,c("HLOC_STREET", "HLOC_CITY")])
summary(PER[,c("SCHOOL_NAME", "SCHOOL_STREET", "SCHOOL_CITY", "WORK_NAME", "WORK_STREET", "WORK_CITY")])

# Rename
temp <- places

# Add Home, Work, and School locations
for (i in 1:nrow(HH)) {
  tHH <- HH[i,]
  tPER <- PER[PER$HHCODE==tHH$HHCODE,]
  for (j in 1:nrow(tPER)) {
    tPERj <- tPER[j,]
    temp[temp$HHResponseId==tHH$ResponseId & temp$PERNAME==tPERj$NAME & !is.na(temp$PLACE) & temp$PLACE=="Home",c("PLACE_NAME", "PLACE_STREET", "PLACE_CITY")] <- list("Home", tHH$HLOC_STREET, tHH$HLOC_CITY)
    temp[temp$HHResponseId==tHH$ResponseId & temp$PERNAME==tPERj$NAME & !is.na(temp$PLACE) & temp$PLACE=="Primary job",c("PLACE_NAME", "PLACE_STREET", "PLACE_CITY")] <- list(tPERj$WORK_NAME, tPERj$WORK_STREET, tPERj$WORK_CITY)
    temp[temp$HHResponseId==tHH$ResponseId & temp$PERNAME==tPERj$NAME & !is.na(temp$PLACE) & temp$PLACE=="School",c("PLACE_NAME", "PLACE_STREET", "PLACE_CITY")] <- list(tPERj$SCHOOL_NAME, tPERj$SCHOOL_STREET, tPERj$SCHOOL_CITY)
    rm(tPERj)
  }; rm(j)
  rm(tHH, tPER)
}; rm(i)

# Edit places that are drop-offs/pick-ups at a school
# intented PLACE=="School" to be for someone's own school (to attend class)
# but many people used it for pick-up/drop-off passengers to/from their school
table(temp$PLACE)
# a. PLACE=="School" & STUDENT2!="Yes"
# tr <- unique(temp$ResponseId[temp$PLACE=="School"]) # 501
tr1 <- unique(temp$HHResponseId[temp$PLACE=="School"])
tr2 <- unique(PER$ResponseId[PER$STUDENT2=="Yes"])
tr3 <- tr1[!(tr1 %in% tr2)]
tr <- unique(temp$ResponseId[(temp$HHResponseId %in% tr3) & (temp$PLACE=="School")]) # 191
rm(tr1, tr2, tr3)
# change to drop-off/pick-up if short activity duration and (usually) change # people
# change to errand/appointment if long activity duration and looks like meeting
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==4L, c("PLACE", "PLACE_NAME", "ACTIVITY")] <- list("Other place", "school", "Drop off or pick up passenger(s)")
# of 191, 12 seemed okay (long duration at school), 2 unable to be guessed/changed 
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Remove
rm(HH, PER)

# Rename
places <- temp
rm(temp)

# Inspect
names(places)
str(places, list.len=ncol(places))
summary(places)

########################################
# Edit times in places

# Rename
temp <- places

# Add start day and end day times
temp[temp$PLANO==1, c("ARR_HOUR", "ARR_MIN", "ARR_AMPM")] <- list(3L, 0L, "AM")
temp[!is.na(temp$LEAVE) & temp$LEAVE=="No", c("DEP_HOUR", "DEP_MIN", "DEP_AMPM")] <- list(3L, 0L, "AM")
summary(temp[temp$PLANO==1, c("ARR_HOUR", "ARR_MIN", "ARR_AMPM")])
summary(temp[!is.na(temp$LEAVE) & temp$LEAVE=="No", c("DEP_HOUR", "DEP_MIN", "DEP_AMPM")])

# Add times for change mode
table(temp$ACTIVITY=="Change mode", is.na(temp$DEP_HOUR)); table(temp$ACTIVITY=="Change mode", is.na(temp$DEP_MIN)); table(temp$ACTIVITY=="Change mode", is.na(temp$DEP_AMPM))
tr <-  unique(temp$ResponseId[temp$ACTIVITY=="Change mode" & is.na(temp$DEP_HOUR)])
# if bus stop: if getting on: arrival + 5 minues (usually assume 5 minutes wait time, except 0 if schedule or other times)
# if bus stop: if getting off: same time as arrival (assume 0 minutes wait time)
# if parking location: if changing mode: same time as arrival (assume 0 minutes wait time)
# if parking location: if doing activity, change PLACE & ACTIVITY, look at similar travel times
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2L, c("DEP_HOUR", "DEP_MIN", "DEP_AMPM")] <- temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2L, c("ARR_HOUR", "ARR_MIN", "ARR_AMPM")]
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2L, c("PLACE", "ACTIVITY", "DEP_HOUR", "DEP_MIN", "DEP_AMPM")] <- list("Other place", "Shopping (groceries, clothing, convenience store, etc)", 10L, 00L, "AM")
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2L, c("PLACE", "ACTIVITY")] <- list("Other place", "Drop off or pick up passenger(s)")
# of 166, 1 all NAs, 2 unable to guess
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Fix missing ARR_, DEP_
summary(temp$ARR_HOUR); summary(temp$ARR_MIN); summary(temp$ARR_AMPM)
summary(temp$DEP_HOUR); summary(temp$DEP_MIN); summary(temp$DEP_AMPM)
# lots of missing values
tr <-  unique(temp$ResponseId[is.na(temp$ARR_HOUR) | is.na(temp$ARR_MIN) | is.na(temp$ARR_AMPM) | is.na(temp$DEP_HOUR) | is.na(temp$DEP_MIN) | is.na(temp$DEP_AMPM)])
# missing AM/PM is easy to fix in context of other trips
# missing minutes is often 0L, but sometimes not
# assume similar travel time if possible
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2L, c("ARR_AMPM")] <- list("PM")
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2L, c("DEP_AMPM")] <- list("AM")
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==4L, c("DEP_HOUR", "DEP_MIN", "DEP_AMPM")] <- list(3L, 30L, "PM")
# of 166, 21 unable to guessed, 4 other edit
# temp[temp$ResponseId==tr[1],]
rm(tr)

# Check/add reasons to remove
# .. from investigating missing time
# unable to fix because missing trips
# - NOTE: code removed for confidentiality
# - example: myids <- c("R_0123456789abcde")
# - example: diary$REMOVE[diary$ResponseId %in% myids] # all good
# - example: rm(myids)
# unable to fix because many missing times
# - NOTE: code removed for confidentiality
# - example: diary$REMOVE[diary$ResponseId=="R_0123456789abcde"] <- "unknown times"
# - example: myids <- c("R_0123456789abcde")
# - example: diary$REMOVE[diary$ResponseId %in% myids] # all good
# - example: rm(myids)

# Other edits to trips
# from investigating missing time
# - NOTE: code removed for confidentiality
# - example: temp <- temp[!(temp$ResponseId=="R_0123456789abcde" & temp$PLANO==4L),] # remove place 4 (loop trip walk)
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==5L, c("PLANO")] <- list(4L)
# - example: temp <- temp[!(temp$ResponseId=="R_0123456789abcde" & temp$PLANO==6L),] # remove place 6 (duplicate)
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==7L, c("PLANO")] <- list(6L)
# from investigating school
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO %in% c(3L:6L), c("PLANO")] <- list(4L:7L) # add place home between school 2/3
# - example: temp <- rbind(temp, list("DIARY12", "R_0123456789abcde", "Patrick", "2019-01-30", "R_0123456789abcde", 3L, "Home", "Home", "Main St & Center St", "Logan", 9L, 00L, "AM", "Home activities", "", "Yes", 12L, 00L, "PM", "Car/Van/Truck/SUV Driver", "", 2L, "Primary household vehicle", "", NA, ""))
# from investigating change mode
# - NOTE: code removed for confidentiality

# Create time (initially)
temp$ARR_TIME <- paste(temp$DATE, paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME <- as.POSIXct(temp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$ARR_TIME2 <- paste((temp$DATE+1), paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME2 <- as.POSIXct(temp$ARR_TIME2, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$DEP_TIME <- paste((temp$DATE), paste(temp$DEP_HOUR, temp$DEP_MIN, sep=":"), temp$DEP_AMPM, sep=" ")
temp$DEP_TIME <- as.POSIXct(temp$DEP_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$DEP_TIME2 <- paste((temp$DATE+1), paste(temp$DEP_HOUR, temp$DEP_MIN, sep=":"), temp$DEP_AMPM, sep=" ")
temp$DEP_TIME2 <- as.POSIXct(temp$DEP_TIME2, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$timecomp <- paste((temp$DATE), paste(3L, 0L, sep=":"), "AM", sep=" ")
temp$timecomp <- as.POSIXct(temp$timecomp, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$ARR_TIME[which(temp$ARR_TIME < temp$timecomp)] <- temp$ARR_TIME2[which(temp$ARR_TIME < temp$timecomp)]
temp$DEP_TIME[which(temp$DEP_TIME <= temp$timecomp)] <- temp$DEP_TIME2[which(temp$DEP_TIME <= temp$timecomp)]
temp[,c("ARR_TIME2", "DEP_TIME2", "timecomp")] <- NULL
summary(temp$ARR_TIME)
summary(temp$DEP_TIME)
temp$ACT_TIME <- difftime(temp$DEP_TIME, temp$ARR_TIME, units="mins")
summary(as.numeric(temp$ACT_TIME))

# Check activity time logic
# a. NAs
tr <- unique(temp$ResponseId[is.na(temp$ACT_TIME)])
diary$REMOVE[diary$ResponseId %in% tr]
table(temp$DEP_MIN)
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2L, c("DEP_HOUR", "DEP_MIN")] <- list(8L, 0L)
# of 28, 22 are incomplete/missing, 6 others fixed b/c had DEP_MIN=60
# temp[temp$ResponseId==tr[1],]
rm(tr)
# b. Negative
tr <- unique(temp$ResponseId[temp$ACT_TIME < 0])
# mostly issues with AM/PM
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==2L, c("DEP_AMPM")] <- list("PM")
# of 44, 1 all NAs, others fixed
# temp[temp$ResponseId==tr[1],]
rm(tr)
# c. Zero
table(temp$ACTIVITY,temp$ACT_TIME==0)
tr <- unique(temp$ResponseId[temp$ACT_TIME==0 & !(temp$ACTIVITY %in% c("Change mode", "Drop off or pick up passenger(s)"))])
# assume most are okay
# of 76, 1 all NAs
# temp[temp$ResponseId==tr[1],]
rm(tr)
# d. Long change mode activities
table(temp$ACT_TIME[temp$ACTIVITY=="Change mode"])
tr <- unique(temp$ResponseId[temp$ACTIVITY=="Change mode" & temp$ACT_TIME > 15])
# okay if flights, rental car, etc.
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==6L, c("DEP_MIN")] <- list(0L)
# of 8, 1 all NAs
# temp[temp$ResponseId==tr[1],]
rm(tr)
# e. Long drop-off/pick-up activities
table(temp$ACT_TIME[temp$ACTIVITY=="Drop off or pick up passenger(s)"])
tr <- unique(temp$ResponseId[temp$ACTIVITY=="Drop off or pick up passenger(s)" & temp$ACT_TIME > 20])
tr <- unique(temp$ResponseId[temp$ACTIVITY=="Drop off or pick up passenger(s)" & temp$ACT_TIME > 120])
# okay if at school,  daycare, family/friends home, other places for a little while
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==4L, c("DEP_AMPM")] <- list("PM")
# of 42/10, 1 all NAs, fixed some
# temp[temp$ResponseId==tr[1],]
rm(tr)
# f. Short work activities
table(temp$ACT_TIME[temp$ACTIVITY=="Work activities"])
tr <- unique(temp$ResponseId[temp$ACTIVITY=="Work activities" & temp$ACT_TIME < 15])
# a few edits based on other days' patterns
# - NOTE: code removed for confidentiality
# - example: temp <- temp[!(temp$ResponseId=="R_0123456789abcde" & temp$PLANO==4L),] # duplicate place 4
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO==5L, c("PLANO")] <- list(4L)
# of 37, 1 all NAs, fixed some
# temp[temp$ResponseId==tr[1],]
rm(tr)
# g. Short school activities
table(temp$ACT_TIME[temp$ACTIVITY=="School activities"])
tr <- unique(temp$ResponseId[temp$ACTIVITY=="School activities" & temp$ACT_TIME < 15])
# HH <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "HH.rds"))
# PER <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PER.rds"))
# change short school trips to pickup/dropoff if seems apparant (e.g., not a student and have children, or change # people)
# - NOTE: code removed for confidentiality
# - example: temp[temp$ResponseId=="R_0123456789abcde" & temp$PLANO %in% c(2L,4L), c("PLACE", "ACTIVITY")] <- list("Other place", "Drop off or pick up passenger(s)")
# of 46, 1 all NAs, 1 okay, 4 unable to be fixed
# temp[temp$ResponseId==tr[1],]
# View(temp[temp$HHResponseId=="",])
# HH[HH$ResponseId=="",]
# PER[PER$ResponseId=="",]
# rm(HH, PER)
rm(tr)

# Create time (again)
temp$ARR_TIME <- paste(temp$DATE, paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME <- as.POSIXct(temp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$ARR_TIME2 <- paste((temp$DATE+1), paste(temp$ARR_HOUR, temp$ARR_MIN, sep=":"), temp$ARR_AMPM, sep=" ")
temp$ARR_TIME2 <- as.POSIXct(temp$ARR_TIME2, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$DEP_TIME <- paste((temp$DATE), paste(temp$DEP_HOUR, temp$DEP_MIN, sep=":"), temp$DEP_AMPM, sep=" ")
temp$DEP_TIME <- as.POSIXct(temp$DEP_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$DEP_TIME2 <- paste((temp$DATE+1), paste(temp$DEP_HOUR, temp$DEP_MIN, sep=":"), temp$DEP_AMPM, sep=" ")
temp$DEP_TIME2 <- as.POSIXct(temp$DEP_TIME2, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$timecomp <- paste((temp$DATE), paste(3L, 0L, sep=":"), "AM", sep=" ")
temp$timecomp <- as.POSIXct(temp$timecomp, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
temp$ARR_TIME[which(temp$ARR_TIME < temp$timecomp)] <- temp$ARR_TIME2[which(temp$ARR_TIME < temp$timecomp)]
temp$DEP_TIME[which(temp$DEP_TIME <= temp$timecomp)] <- temp$DEP_TIME2[which(temp$DEP_TIME <= temp$timecomp)]
temp[,c("ARR_TIME2", "DEP_TIME2", "timecomp")] <- NULL
summary(temp$ARR_TIME)
summary(temp$DEP_TIME)
temp$ACT_TIME <- difftime(temp$DEP_TIME, temp$ARR_TIME, units="mins")
summary(as.numeric(temp$ACT_TIME))

# Rename
places <- temp
rm(temp)

# Fix columns
places$DEP_HOUR <- as.integer(places$DEP_HOUR)
places$DEP_MIN <- as.integer(places$DEP_MIN)

# Inspect
names(places)
str(places, list.len=ncol(places))
summary(places)

########################################
# Assemble trips data

# Rename
tempp <- places

# Initialize
mycols0 <- c("DIARY", "HHResponseId", "PERNAME", "DATE", "ResponseId")
mycols1a <- c("PLANO", "ACTIVITY", "DEP_TIME")
mycols1b <- c("MODE", "PEOPLE", "VEHICLE")
mycols2 <- c("PLANO", "ACTIVITY", "ARR_TIME")

# Initialize
trr <- unique(places$ResponseId)
i <- trr[1]
t0 <- places[places$ResponseId==i,]
t1 <- t0[1:(nrow(t0)-1),c(mycols0, mycols1a, mycols1b)]
names(t1)[names(t1) %in% c("PLANO", "ACTIVITY")] <- c("PLANO1", "ACTIVITY1")
t1 <- cbind(t1, t0[2:nrow(t0),c(mycols2)])
names(t1)[names(t1) %in% c("PLANO", "ACTIVITY")] <- c("PLANO2", "ACTIVITY2")
temp <- t1[0,]
rm(trr, i, t0, t1)

# For loop
for (i in unique(places$ResponseId)) {
  t0 <- places[places$ResponseId==i,]
  t0 <- t0[order(t0$PLANO),]
  if (nrow(t0)>=2) {
    t1 <- t0[1:(nrow(t0)-1),c(mycols0, mycols1a, mycols1b)]
    names(t1)[names(t1) %in% c("PLANO", "ACTIVITY")] <- c("PLANO1", "ACTIVITY1")
    t1 <- cbind(t1, t0[2:nrow(t0),c(mycols2)])
    names(t1)[names(t1) %in% c("PLANO", "ACTIVITY")] <- c("PLANO2", "ACTIVITY2")
    temp <- rbind(temp, t1)
    rm(t1)
  }
  rm(t0)
}; rm(i)

# Inspect
table(table(temp$ResponseId)); table(table(places$ResponseId))
summary(temp$MODE)

# Create time
temp$TRIP_TIME <- difftime(temp$ARR_TIME, temp$DEP_TIME, units="mins")
summary(as.numeric(temp$TRIP_TIME))

# Fix travel times
# a. NAs
summary(is.na(temp$TRIP_TIME))
tr <- unique(temp$ResponseId[is.na(temp$TRIP_TIME)])
diary$REMOVE[diary$ResponseId %in% tr]
# of 21, all are marked for removal
rm(tr)
# b. Negative
summary(temp$TRIP_TIME < 0)
tr <- unique(temp$ResponseId[temp$TRIP_TIME < 0])
# some AM/PM issues, some switched between subsequent events, or off by an hour
# - NOTE: code removed for confidentiality
# - example: tempp[tempp$ResponseId=="R_0123456789abcde" & tempp$PLANO==4L, c("DEP_MIN")] <- list(5L)
# - example: tempp[tempp$ResponseId=="R_0123456789abcde" & tempp$PLANO==5L, c("ARR_MIN")] <- list(25L)
# of 101, 1 all NAs, 1 some NAs
# temp[temp$ResponseId==tr[1],]
# tempp[tempp$ResponseId==tr[1],]
rm(tr)
# c. Zero
summary(temp$TRIP_TIME==0)
tr <- unique(temp$ResponseId[temp$TRIP_TIME==0])
# of 151, assume all are okay
rm(tr)
# d. Long
table(temp$TRIP_TIME)
tr <- unique(temp$ResponseId[temp$TRIP_TIME >= 180])
# okay if a long distance, add missing trip if similar to another day
# - NOTE: code removed for confidentiality
# - example: tempp[tempp$ResponseId=="R_0123456789abcde" & tempp$PLANO %in% c(3L:6L), c("PLANO")] <- c(4L:7L) # add place home between other 2/3
# - example: tempp <- rbind(tempp, list("DIARY12", "R_0123456789abcde", "Patrick", "2019-01-30", "R_0123456789abcde", 3L, "Home", "Home", "Main St & Center St", "Logan", 8L, 00L, "AM", "Home activities", "", "Yes", 12L, 00L, "PM", "Car/Van/Truck/SUV Driver", "", "0 (none)", "Primary household vehicle", "", NA, "", NA, NA, NA))
# of 68, 1 all NAs, 19 fixed, 22 unable to be fixed
# temp[temp$ResponseId==tr[1],]
# tempp[tempp$ResponseId==tr[1],]
rm(tr)

# Add remove reasons
# couldn't fix --> probably missing a trip
# - NOTE: code removed for confidentiality
# - example: tr_mis <- c("R_0123456789abcde")
# - example: diary$REMOVE[diary$ResponseId %in% tr_mis] <- "maybe missing place"
# - example: rm(tr_mis)

# Create time (again)
tempp$ARR_TIME <- paste(tempp$DATE, paste(tempp$ARR_HOUR, tempp$ARR_MIN, sep=":"), tempp$ARR_AMPM, sep=" ")
tempp$ARR_TIME <- as.POSIXct(tempp$ARR_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
tempp$ARR_TIME2 <- paste((tempp$DATE+1), paste(tempp$ARR_HOUR, tempp$ARR_MIN, sep=":"), tempp$ARR_AMPM, sep=" ")
tempp$ARR_TIME2 <- as.POSIXct(tempp$ARR_TIME2, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
tempp$DEP_TIME <- paste((tempp$DATE), paste(tempp$DEP_HOUR, tempp$DEP_MIN, sep=":"), tempp$DEP_AMPM, sep=" ")
tempp$DEP_TIME <- as.POSIXct(tempp$DEP_TIME, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
tempp$DEP_TIME2 <- paste((tempp$DATE+1), paste(tempp$DEP_HOUR, tempp$DEP_MIN, sep=":"), tempp$DEP_AMPM, sep=" ")
tempp$DEP_TIME2 <- as.POSIXct(tempp$DEP_TIME2, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
tempp$timecomp <- paste((tempp$DATE), paste(3L, 0L, sep=":"), "AM", sep=" ")
tempp$timecomp <- as.POSIXct(tempp$timecomp, format="%Y-%m-%d %I:%M %p", tz="America/Denver")
tempp$ARR_TIME[which(tempp$ARR_TIME < tempp$timecomp)] <- tempp$ARR_TIME2[which(tempp$ARR_TIME < tempp$timecomp)]
tempp$DEP_TIME[which(tempp$DEP_TIME <= tempp$timecomp)] <- tempp$DEP_TIME2[which(tempp$DEP_TIME <= tempp$timecomp)]
tempp[,c("ARR_TIME2", "DEP_TIME2", "timecomp")] <- NULL
summary(tempp$ARR_TIME)
summary(tempp$DEP_TIME)
tempp$ACT_TIME <- difftime(tempp$DEP_TIME, tempp$ARR_TIME, units="mins")
summary(as.numeric(tempp$ACT_TIME))

# Rename
places <- tempp
rm(tempp)

# Re-create trip
# initialize
trr <- unique(places$ResponseId)
i <- trr[1]
t0 <- places[places$ResponseId==i,]
t1 <- t0[1:(nrow(t0)-1),c(mycols0, mycols1a, mycols1b)]
names(t1)[names(t1) %in% c("PLANO", "ACTIVITY")] <- c("PLANO1", "ACTIVITY1")
t1 <- cbind(t1, t0[2:nrow(t0),c(mycols2)])
names(t1)[names(t1) %in% c("PLANO", "ACTIVITY")] <- c("PLANO2", "ACTIVITY2")
temp <- t1[0,]
rm(trr, i, t0, t1)
# for loop
for (i in unique(places$ResponseId)) {
  t0 <- places[places$ResponseId==i,]
  t0 <- t0[order(t0$PLANO),]
  if (nrow(t0)>=2) {
    t1 <- t0[1:(nrow(t0)-1),c(mycols0, mycols1a, mycols1b)]
    names(t1)[names(t1) %in% c("PLANO", "ACTIVITY")] <- c("PLANO1", "ACTIVITY1")
    t1 <- cbind(t1, t0[2:nrow(t0),c(mycols2)])
    names(t1)[names(t1) %in% c("PLANO", "ACTIVITY")] <- c("PLANO2", "ACTIVITY2")
    temp <- rbind(temp, t1)
    rm(t1)
  }
  rm(t0)
}; rm(i)
# create time
temp$TRIP_TIME <- difftime(temp$ARR_TIME, temp$DEP_TIME, units="mins")
summary(as.numeric(temp$TRIP_TIME))

# Add trip distances
temp$TRIP_DIST <- 0
# add later when geocoding

# Rename
trips <- temp
rm(temp, mycols0, mycols1a, mycols1b, mycols2)

# Inspect
names(trips)
str(trips, list.len=ncol(trips))
summary(trips)

########################################
# Add HH and PER codes

# Load formatted HH and PER files
HH <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "HH.rds"))
PER <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PER.rds"))

# Inspect
str(HH, list.len=ncol(HH))
str(PER, list.len=ncol(PER))
str(diary, list.len=ncol(diary))
str(places, list.len=ncol(places))
str(trips, list.len=ncol(trips))

# Merge diary
t1 <- merge(diary, HH[,c("ResponseId", "HHCODE")], by.x="HHResponseId", by.y="ResponseId", all.x=T, all.y=F)
t2 <- merge(t1, PER[,c("HHCODE", "PERCODE", "NAME")], by.x=c("HHCODE", "PERNAME"), by.y=c("HHCODE", "NAME"), all.x=T, all.y=F)
DIARY <- t2
rm(t1, t2)

# Reorganize DIARY
mycols <- c("ResponseId", "HHResponseId", "HHCODE", "PERCODE", "PERNAME", "PERWHO", "DIARY", "DATE", "DATE_ORIG", 
            "SPECIAL", grep("RATE", names(DIARY), value=T), grep("IDLE", names(DIARY), value=T), "COMMENTS", "REMOVE")
DIARY <- DIARY[,mycols]
rm(mycols)
DIARY <- DIARY[order(DIARY$HHCODE, DIARY$PERCODE, DIARY$DATE),]
row.names(DIARY) <- NULL

# Merge places
t1 <- merge(places, HH[,c("ResponseId", "HHCODE")], by.x="HHResponseId", by.y="ResponseId", all.x=T, all.y=F)
t2 <- merge(t1, PER[,c("HHCODE", "PERCODE", "NAME")], by.x=c("HHCODE", "PERNAME"), by.y=c("HHCODE", "NAME"), all.x=T, all.y=F)
PLACE <- t2
rm(t1, t2)

# Reorganize PLACE
mycols <- c("ResponseId", "HHResponseId", "HHCODE", "PERCODE", "PERNAME", "DIARY", "DATE", "PLANO", grep("PLACE", names(PLACE), value=T), 
            grep("ARR", names(PLACE), value=T), "ACTIVITY", "ACTIVITY_TEXT", "LEAVE", "NOTRIPS", "NOTRIPS_TEXT", 
            grep("DEP", names(PLACE), value=T), "ACT_TIME", "MODE", "MODE_TEXT", "PEOPLE", "VEHICLE", "VEHICLE_TEXT")
PLACE <- PLACE[,mycols]
rm(mycols)
PLACE <- PLACE[order(PLACE$HHCODE, PLACE$PERCODE, PLACE$DATE, PLACE$PLANO),]
row.names(PLACE) <- NULL

# Merge trips
t1 <- merge(trips, HH[,c("ResponseId", "HHCODE")], by.x="HHResponseId", by.y="ResponseId", all.x=T, all.y=F)
t2 <- merge(t1, PER[,c("HHCODE", "PERCODE", "NAME")], by.x=c("HHCODE", "PERNAME"), by.y=c("HHCODE", "NAME"), all.x=T, all.y=F)
TRIP <- t2
rm(t1, t2)

# Reorganize TRIP
mycols <- c("ResponseId", "HHResponseId", "HHCODE", "PERCODE", "PERNAME", "DIARY", "DATE", 
            "PLANO1", "ACTIVITY1", "DEP_TIME", "MODE", "PEOPLE", "VEHICLE", 
            "PLANO2", "ACTIVITY2", "ARR_TIME", "TRIP_TIME", "TRIP_DIST")
TRIP <- TRIP[,mycols]
rm(mycols)
TRIP <- TRIP[order(TRIP$HHCODE, TRIP$PERCODE, TRIP$DATE, TRIP$PLANO1),]
row.names(TRIP) <- NULL

# Inspect DIARY
names(DIARY)
str(DIARY, list.len=ncol(DIARY))
summary(DIARY)

# Inspect PLACE
names(PLACE)
str(PLACE, list.len=ncol(PLACE))
summary(PLACE)

# Inspect TRIP
names(TRIP)
str(TRIP, list.len=ncol(TRIP))
summary(TRIP)

# Cleanup
rm(HH, PER)
rm(diary, places_wide, places, trips)

########################################
# Save

# Save
t_folder <- "Data 3a Formatted"
saveRDS(meta_diary, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_diary.rds"))
write.csv(meta_diary, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_diary.csv"), row.names=F)
saveRDS(DIARY, file=file.path("Data", "Survey 2019 Winter", t_folder, "DIARY.rds"))
write.csv(DIARY, file=file.path("Data", "Survey 2019 Winter", t_folder, "DIARY.csv"), row.names=F)
saveRDS(PLACE, file=file.path("Data", "Survey 2019 Winter", t_folder, "PLACE.rds"))
write.csv(PLACE, file=file.path("Data", "Survey 2019 Winter", t_folder, "PLACE.csv"), row.names=F)
saveRDS(TRIP, file=file.path("Data", "Survey 2019 Winter", t_folder, "TRIP.rds"))
write.csv(TRIP, file=file.path("Data", "Survey 2019 Winter", t_folder, "TRIP.csv"), row.names=F)
rm(t_folder)

########################################
# Cleanup

# Remove
rm(diary12, diary3, diary4, diary5, diary6)
rm(temp12, temp3, temp4, temp5, temp6)
rm(meta_diary, DIARY, PLACE, TRIP)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################