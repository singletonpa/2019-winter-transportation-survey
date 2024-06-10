########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     anonymize.R
# About:    Script to anonymize data
########################################

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
library("sf")

########################################
# Load data

# Read rds data files from Data 3a Formatted
t_folder <- "Data 3a Formatted"
contact <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "contact.rds"))
meta_initial <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_initial.rds"))
VEH <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "VEH.rds"))
meta_diary <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_diary.rds"))
DIARY <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "DIARY.rds"))
meta_final <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_final.rds"))
PER_final <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "PER_final.rds"))
rm(t_folder)

# Read rds data files from Data 3b Finished
t_folder <- "Data 3b Finished"
HH <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "HH.rds"))
PER <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "PER.rds"))
PLACE <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "PLACE.rds"))
TRIP <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "TRIP.rds"))
rm(t_folder)

########################################
# Anonymize

# Contact (initial)
str(contact); summary(contact)
contact$EMAIL <- ""
contact$ADD_STREET <- ""
contact$ADD_ZIP <- substr(contact$ADD_ZIP, 1, 5)

# Metadata (initial)
str(meta_initial); summary(meta_initial)
meta_initial$IPAddress <- ""
meta_initial$RecipientLastName <- ""
meta_initial$RecipientFirstName <- ""
meta_initial$RecipientEmail <- ""
meta_initial$ExternalReference <- ""
meta_initial$LocationLatitude <- NA
meta_initial$LocationLongitude <- NA

# Metadata (diary)
str(meta_diary); summary(meta_diary)
meta_diary$IPAddress <- ""
meta_diary$RecipientLastName <- ""
meta_diary$RecipientFirstName <- ""
meta_diary$RecipientEmail <- ""
meta_diary$ExternalReference <- ""
meta_diary$LocationLatitude <- NA
meta_diary$LocationLongitude <- NA

# Metadata (final)
str(meta_final); summary(meta_final)
meta_final$IPAddress <- ""
meta_final$RecipientLastName <- ""
meta_final$RecipientFirstName <- ""
meta_final$RecipientEmail <- ""
meta_final$ExternalReference <- ""
meta_final$LocationLatitude <- NA
meta_final$LocationLongitude <- NA

# Household (initial)
str(HH); summary(HH)
HH$HLOC_STREET <- ""
HH$HLOC_CITY <- ""
HH$HLOC_ZIP <- ""
HH$HTYPE_TEXT <- ""
HH$COMMENTS <- ""
HH$HLOC_LAT <- NA
HH$HLOC_LNG <- NA
HH$HLOC_ADD <- ""
HH$HLOC_PID <- ""

# Person (initial)
str(PER); summary(PER)
PER$NAME <- ""
PER$RACE_7_TEXT <- ""
PER$GEND_TEXT <- ""
PER$SCHOOL_NAME <- ""
PER$SCHOOL_STREET <- ""
PER$SCHOOL_CITY <- ""
PER$SMODE_8_TEXT <- ""
PER$WORK_NAME <- ""
PER$WORK_STREET <- ""
PER$WORK_CITY <- ""
PER$WMODE_7_TEXT <- ""
PER$WORK_LAT <- NA
PER$WORK_LNG <- NA
PER$WORK_ADD <- ""
PER$WORK_PID <- ""
PER$SCHOOL_LAT <- NA
PER$SCHOOL_LNG <- NA
PER$SCHOOL_ADD <- ""
PER$SCHOOL_PID <- ""

# Vehicle (initial)
str(VEH); summary(VEH)
# VEH$TYPE_TEXT <- ""
# VEH$MAKE <- ""
# VEH$MODEL <- ""

# Diary (diary)
str(DIARY); summary(DIARY)
DIARY$PERNAME <- ""
DIARY$SPECIAL <- ""
DIARY$COMMENTS <- ""

# Place (diary)
str(PLACE); summary(PLACE)
PLACE$PERNAME <- ""
PLACE$PLACE_NAME <- ""
PLACE$PLACE_STREET <- ""
PLACE$PLACE_CITY <- ""
PLACE$PLOC0 <- ""
PLACE$PLOC <- ""
PLACE$PLOC_LAT <- NA
PLACE$PLOC_LNG <- NA
PLACE$PLOC_ADD <- ""
PLACE$PLOC_PID <- ""
PLACE$ACTIVITY_TEXT <- ""
PLACE$NOTRIPS_TEXT <- ""
# PLACE$MODE_TEXT <- ""
# PLACE$VEHICLE_TEXT <- ""

# Trip (diary)
str(TRIP); summary(TRIP)
TRIP$PERNAME <- ""
TRIP$PLOC1 <- ""
TRIP$PLOC1_PID <- ""
TRIP$PLOC1_LAT <- NA
TRIP$PLOC1_LNG <- NA
TRIP$PLOC2 <- ""
TRIP$PLOC2_PID <- ""
TRIP$PLOC2_LAT <- NA
TRIP$PLOC2_LNG <- NA

# Person (final)
str(PER_final); summary(PER_final)
PER_final$PERNAME <- ""
PER_final$CHANGES_TEXT <- ""
PER_final$IDLING_6_TEXT <- ""
PER_final$BEHCHG_ME_8_TEXT <- ""
PER_final$BEHCHG_POP_8_TEXT <- ""
PER_final$OTHER_IDEAS <- ""
PER_final$COMMENTS <- ""

########################################
# Remove some rows

# Inspect reasons for removal
table(DIARY$REMOVE)
DIARY[DIARY$REMOVE=="withdraw",]
rem_hh <- DIARY$HHResponseId[DIARY$REMOVE=="withdraw"]
rem_di <- DIARY$ResponseId[DIARY$REMOVE %in% c("withdraw", "duplicate")]

# Remove withdrawn households
HH <- HH[!(HH$ResponseId %in% rem_hh),]
row.names(HH) <- NULL
PER <- PER[!(PER$ResponseId %in% rem_hh),]
row.names(PER) <- NULL
VEH <- VEH[!(VEH$ResponseId %in% rem_hh),]
row.names(VEH) <- NULL
contact <- contact[!(contact$ResponseId %in% rem_hh),]
row.names(contact) <- NULL
meta_initial <- meta_initial[!(meta_initial$ResponseId %in% rem_hh),]
row.names(meta_initial) <- NULL

# Remove duplicate diaries
DIARY <- DIARY[!(DIARY$ResponseId %in% rem_di),]
row.names(DIARY) <- NULL
PLACE <- PLACE[!(PLACE$ResponseId %in% rem_di),]
row.names(PLACE) <- NULL
TRIP <- TRIP[!(TRIP$ResponseId %in% rem_di),]
row.names(TRIP) <- NULL
meta_diary <- meta_diary[!(meta_diary$ResponseId %in% rem_di),]
row.names(meta_diary) <- NULL

# Cleanup
rm(rem_hh, rem_di)

########################################
# Save anonymized

# Save
t_folder <- "Data 4 Anonymized"
saveRDS(contact, file=file.path("Data", "Survey 2019 Winter", t_folder, "contact.rds"))
write.csv(contact, file=file.path("Data", "Survey 2019 Winter", t_folder, "contact.csv"), row.names=F)
saveRDS(meta_initial, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_initial.rds"))
write.csv(meta_initial, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_initial.csv"), row.names=F)
saveRDS(HH, file=file.path("Data", "Survey 2019 Winter", t_folder, "HH.rds"))
write.csv(HH, file=file.path("Data", "Survey 2019 Winter", t_folder, "HH.csv"), row.names=F)
saveRDS(PER, file=file.path("Data", "Survey 2019 Winter", t_folder, "PER.rds"))
write.csv(PER, file=file.path("Data", "Survey 2019 Winter", t_folder, "PER.csv"), row.names=F)
saveRDS(VEH, file=file.path("Data", "Survey 2019 Winter", t_folder, "VEH.rds"))
write.csv(VEH, file=file.path("Data", "Survey 2019 Winter", t_folder, "VEH.csv"), row.names=F)
saveRDS(meta_diary, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_diary.rds"))
write.csv(meta_diary, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_diary.csv"), row.names=F)
saveRDS(DIARY, file=file.path("Data", "Survey 2019 Winter", t_folder, "DIARY.rds"))
write.csv(DIARY, file=file.path("Data", "Survey 2019 Winter", t_folder, "DIARY.csv"), row.names=F)
saveRDS(PLACE, file=file.path("Data", "Survey 2019 Winter", t_folder, "PLACE.rds"))
write.csv(PLACE, file=file.path("Data", "Survey 2019 Winter", t_folder, "PLACE.csv"), row.names=F)
saveRDS(TRIP, file=file.path("Data", "Survey 2019 Winter", t_folder, "TRIP.rds"))
write.csv(TRIP, file=file.path("Data", "Survey 2019 Winter", t_folder, "TRIP.csv"), row.names=F)
saveRDS(meta_final, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_final.rds"))
write.csv(meta_final, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_final.csv"), row.names=F)
saveRDS(PER_final, file=file.path("Data", "Survey 2019 Winter", t_folder, "PER_final.rds"))
write.csv(PER_final, file=file.path("Data", "Survey 2019 Winter", t_folder, "PER_final.csv"), row.names=F)
rm(t_folder)

########################################
# Cleanup

# Remove
rm(contact, meta_initial)
rm(HH, PER, VEH)
rm(meta_diary)
rm(DIARY, PLACE, TRIP)
rm(meta_final)
rm(PER_final)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################