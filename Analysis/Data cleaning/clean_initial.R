########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     clean_initial.R
# About:    Script to clean initial survey
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
                                "2019 Winter Transportation Study -- Sign-up & Initial Survey_May 23, 2019_15.50.csv"), 
                 stringsAsFactors=F)
temp <- temp[-c(1:2),]
# names
tempN <- read.csv(file=file.path("Data", "Survey 2019 Winter", "Data 0 Raw", 
                                 "2019 Winter Transportation Study -- Sign-up & Initial Survey_May 23, 2019_15.50.csv"), 
                  stringsAsFactors=F)
# temp_names <- temp_names[1:2,]
temp_names <- data.frame(NAME1=as.character(names(tempN)))
temp_names$NAME1 <- as.character(temp_names$NAME1)
temp_names$NAME2 <- as.character(tempN[1,])
temp_names$NAME3 <- as.character(tempN[2,])
rm(tempN)

# Inspect
names(temp)
str(temp)

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
names(temp)[which(names(temp)=="Q2")] <- "CONSENT"
temp$CONSENT <- factor(temp$CONSENT, levels=c("Accept", "Decline"))
names(temp)[which(names(temp)=="Q5")] <- "CHECKAGE"
temp$CHECKAGE <- factor(temp$CHECKAGE, levels=c("Yes, I am 18 years old or older.", "No, I am 17 years old or younger."))
names(temp)[which(names(temp)=="Q6")] <- "CHECKHOME"
temp$CHECKHOME <- factor(temp$CHECKHOME, levels=c("Yes, I live in Cache County, UT.", "No, I live outside of Cache County, UT."))
names(temp)[which(names(temp)=="Q9")] <- "EMAIL"
names(temp)[grepl("Q10", names(temp))] <- paste("ADD", c("STREET", "CITY", "STATE", "ZIP"), sep="_")
names(temp)[grepl("Q12_", names(temp))] <- gsub("Q12", "T1", names(temp)[grepl("Q12_", names(temp))])
for (i in grep("T1_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Start 2
# none

# Household
names(temp)[grepl("Q16", names(temp))] <- paste("HLOC", c("STREET", "CITY", "ZIP"), sep="_")
names(temp)[which(names(temp)=="Q17")] <- "HTYPE"
temp$HTYPE <- factor(temp$HTYPE, levels=c("Mobile home or trailer", "Single-family house, detached from any other house", "Single-family house, attached to other houses (row house)", "Building with 2 apartments/condos (duplex)", "Building with 3 or 4 apartments/condos", "Building with 5 to 9 apartments/condos", "Building with 10 to 19 apartments/condos", "Building with 20 or more apartments/condos", "Other (please specify)"))
names(temp)[which(names(temp)=="Q17_9_TEXT")] <- "HTYPE_TEXT"
temp$HTYPE[temp$HTYPE_TEXT=="House with 3 separate apartments "] <- "Building with 3 or 4 apartments/condos"
temp$HTYPE[grepl("classic house but with two apartments", temp$HTYPE_TEXT)] <- "Building with 2 apartments/condos (duplex)"
temp$HTYPE[temp$HTYPE_TEXT=="Mother in Law House detached"] <- "Single-family house, detached from any other house"
names(temp)[which(names(temp)=="Q18")] <- "HTENURE"
temp$HTENURE <- factor(temp$HTENURE, levels=c("Owned or mortgaged", "Rented"))
names(temp)[which(names(temp)=="Q19")] <- "HLIVED"
temp$HLIVED <- factor(temp$HLIVED, levels=c("Less than 1 year", "1 to 2 years", "3 to 5 years", "6 to 10 years", "11 or more years"))
names(temp)[grepl("Q20_", names(temp))] <- gsub("Q20", "T2", names(temp)[grepl("Q20_", names(temp))])
for (i in grep("T2_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)
names(temp)[which(names(temp)=="Q21")] <- "HHINC"
temp$HHINC <- factor(temp$HHINC, levels=c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 or more", "Do not know", "Prefer not to answer"))
names(temp)[which(names(temp)=="Q22")] <- "HHKIDS"
temp$HHKIDS <- factor(temp$HHKIDS, levels=c("0 (none)", "1", "2", "3", "4", "5+"))
names(temp)[grepl("Q23_", names(temp))] <- gsub("Q23", "T3", names(temp)[grepl("Q23_", names(temp))])
for (i in grep("T3_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# People
temp1 <- temp[,1:(which(names(temp)=="X1_Q25")-1)]
temp2 <- temp[,which(names(temp)=="Q51"):ncol(temp)]
tempP <- temp[,which(names(temp)=="X1_Q25"):(which(names(temp)=="Q51")-1)]
names(tempP)
names(tempP)[grepl("Q25", names(tempP))] <- gsub("Q25", "PNAME", names(tempP)[grepl("Q25", names(tempP))])
names(tempP)[grepl("Q26_", names(tempP))] <- gsub("Q26", "T4", names(tempP)[grepl("Q26_", names(tempP))])
names(tempP)[grepl("Q27", names(tempP))] <- gsub("Q27", "PAGE", names(tempP)[grepl("Q27", names(tempP))])
# names(tempP)[grepl("Q28_7", names(tempP))] # PRACE_7_TEXT
# names(tempP)[grepl("Q28", names(tempP))] # PRACE
names(tempP)[grepl("Q29_3", names(tempP))] <- gsub("_3", "", names(tempP)[grepl("Q29_3", names(tempP))])
names(tempP)[grepl("Q29", names(tempP))] <- gsub("Q29", "PGEND", names(tempP)[grepl("Q29", names(tempP))])
names(tempP)[grepl("Q30", names(tempP))] <- gsub("Q30", "PEDUC", names(tempP)[grepl("Q30", names(tempP))])
names(tempP)[grepl("Q31_", names(tempP))] <- gsub("Q31", "T5", names(tempP)[grepl("Q31_", names(tempP))])
names(tempP)[grepl("Q32", names(tempP))] <- gsub("Q32", "PSTUDENT", names(tempP)[grepl("Q32", names(tempP))])
names(tempP)[grepl("Q33_1", names(tempP))] <- gsub("Q33_1", "PSCHOOL_NAME", names(tempP)[grepl("Q33_1", names(tempP))])
names(tempP)[grepl("Q33_2", names(tempP))] <- gsub("Q33_2", "PSCHOOL_STREET", names(tempP)[grepl("Q33_2", names(tempP))])
names(tempP)[grepl("Q33_3", names(tempP))] <- gsub("Q33_3", "PSCHOOL_CITY", names(tempP)[grepl("Q33_3", names(tempP))])
# names(tempP)[grepl("Q34_8", names(tempP))] # SMODE_8_TEXT
# names(tempP)[grepl("Q34", names(tempP))]   # SMODE
names(tempP)[grepl("Q35_", names(tempP))] <- gsub("Q35", "T6", names(tempP)[grepl("Q35_", names(tempP))])
names(tempP)[grepl("Q36", names(tempP))] <- gsub("Q36", "PWORKER", names(tempP)[grepl("Q36", names(tempP))])
names(tempP)[grepl("Q37_1", names(tempP))] <- gsub("Q37_1", "PWDAYS_COMMUTE", names(tempP)[grepl("Q37_1", names(tempP))])
names(tempP)[grepl("Q37_2", names(tempP))] <- gsub("Q37_2", "PWDAYS_HOME", names(tempP)[grepl("Q37_2", names(tempP))])
names(tempP)[grepl("Q38_1", names(tempP))] <- gsub("Q38_1", "PWHOURS", names(tempP)[grepl("Q38_1", names(tempP))])
names(tempP)[grepl("Q39", names(tempP))] <- gsub("Q39", "PWFLEX", names(tempP)[grepl("Q39", names(tempP))])
names(tempP)[grepl("Q40_1", names(tempP))] <- gsub("Q40_1", "PWORK_NAME", names(tempP)[grepl("Q40_1", names(tempP))])
names(tempP)[grepl("Q40_2", names(tempP))] <- gsub("Q40_2", "PWORK_STREET", names(tempP)[grepl("Q40_2", names(tempP))])
names(tempP)[grepl("Q40_3", names(tempP))] <- gsub("Q40_3", "PWORK_CITY", names(tempP)[grepl("Q40_3", names(tempP))])
# names(tempP)[grepl("Q41_7", names(tempP))] # WMODE_7_TEXT
# names(tempP)[grepl("Q41", names(tempP))]   # WMODE
names(tempP)[grepl("Q42_", names(tempP))] <- gsub("Q42", "T7", names(tempP)[grepl("Q42_", names(tempP))])
names(tempP)[grepl("Q43", names(tempP))] <- gsub("Q43", "PDRVLIC", names(tempP)[grepl("Q43", names(tempP))])
names(tempP)[grepl("Q44_1", names(tempP))] <- gsub("Q44_1", "PKNOW_BIKE", names(tempP)[grepl("Q44_1", names(tempP))])
names(tempP)[grepl("Q44_2", names(tempP))] <- gsub("Q44_2", "PKNOW_AUTO", names(tempP)[grepl("Q44_2", names(tempP))])
names(tempP)[grepl("Q44_3", names(tempP))] <- gsub("Q44_3", "PKNOW_TRAN", names(tempP)[grepl("Q44_3", names(tempP))])
# names(tempP)[grepl("Q45", names(tempP))] # PLIMIT
names(tempP)[grepl("Q46_", names(tempP))] <- gsub("Q46", "T8", names(tempP)[grepl("Q46_", names(tempP))])
names(tempP)[grepl("Q48", names(tempP))] <- gsub("Q48", "PMORE", names(tempP)[grepl("Q48", names(tempP))])
names(tempP)[grepl("Q49_", names(tempP))] <- gsub("Q49", "T9", names(tempP)[grepl("Q49_", names(tempP))])
tempP0 <- data.frame(one=cbind(tempP, one=1)[,"one"])
for (j in 1:10) {
  Xx <- paste0("X", j, "_")
  tempPx <- tempP[,grepl(Xx, names(tempP))]
  # tempPx[,paste0(Xx, "PNAME")]
  tempPx[,paste0(Xx, "PNAME")] <- trimws(tempPx[,paste0(Xx, "PNAME")])
  if (j == 1) {
    # - NOTE: code removed for confidentiality
    # - example: tempPx[c(1),paste0(Xx, "PNAME")] <- c("Patrick")
  }
  if (j == 2) {
    # - NOTE: code removed for confidentiality
  }
  for (i in grep("T4_", names(tempPx), value=T)) { tempPx[,i] <- as.numeric(tempPx[,i]) }; rm(i)
  tempPx[,paste0(Xx, "PAGE")] <- factor(tempPx[,paste0(Xx, "PAGE")], levels=c("18 to 19 years", "20 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years", "55 to 64 years", "65 to 74 years", "75 to 84 years", "85 years and over", "Prefer not to answer"))
  tempPx1 <- tempPx[,1:which(names(tempPx)==paste0(Xx, "PAGE"))]
  tempPx2 <- tempPx[,which(names(tempPx)==paste0(Xx, "PGEND")):ncol(tempPx)]
  tempPx1[,paste0(Xx, "PRACE_1")] <- grepl("White", tempPx[,paste0(Xx, "Q28")])
  tempPx1[,paste0(Xx, "PRACE_2")] <- grepl("Hispanic or Latino", tempPx[,paste0(Xx, "Q28")])
  tempPx1[,paste0(Xx, "PRACE_3")] <- grepl("Asian", tempPx[,paste0(Xx, "Q28")])
  tempPx1[,paste0(Xx, "PRACE_4")] <- grepl("Black or African American", tempPx[,paste0(Xx, "Q28")])
  tempPx1[,paste0(Xx, "PRACE_5")] <- grepl("American Indian or Alaska Native", tempPx[,paste0(Xx, "Q28")])
  tempPx1[,paste0(Xx, "PRACE_6")] <- grepl("Native Hawaiian or other Pacific Islander", tempPx[,paste0(Xx, "Q28")])
  tempPx1[,paste0(Xx, "PRACE_7")] <- grepl("please specify", tempPx[,paste0(Xx, "Q28")])
  tempPx1[,paste0(Xx, "PRACE_7_TEXT")] <- tempPx[,paste0(Xx, "Q28_7_TEXT")]
  tempPx1[,paste0(Xx, "PRACE_8")] <- grepl("Prefer not to answer", tempPx[,paste0(Xx, "Q28")])
  if (j == 1) {
    # - NOTE: code removed for confidentiality
    # - example: tempPx1[which(tempPx1[,paste0(Xx, "PRACE_7_TEXT")]=="White"), paste0(Xx, "PRACE_1")] <- TRUE
    # - example: tempPx1[which(tempPx1[,paste0(Xx, "PRACE_7_TEXT")]=="White"), paste0(Xx, "PRACE_7")] <- FALSE
  }
  tempPx <- cbind(tempPx1, tempPx2)
  rm(tempPx1, tempPx2)
  tempPx[,paste0(Xx, "PGEND")] <- factor(tempPx[,paste0(Xx, "PGEND")], levels=c("Female", "Male", "Other (please specify)", "Prefer not to answer"))
  # tempPx[,paste0(Xx, "PGEND_TEXT")]
  tempPx[,paste0(Xx, "PEDUC")] <- factor(tempPx[,paste0(Xx, "PEDUC")], levels=c("Less than a high school diploma ", "High school diploma or equivalent (e.g. GED)", "Some college, no degree ", "Associate degree (e.g. AA, AS) ", "Bachelor's degree (e.g. BA, BS) ", "Master's degree (e.g. MA, MS, MEng, MEd, MSW, MBA)", "Professional degree beyond a bachelor's degree (e.g. MD, DDS, DVM, LLB, JD)", "Doctorate degree (e.g. PhD, EdD)", "Prefer not to answer"))
  for (i in grep("T5_", names(tempPx), value=T)) { tempPx[,i] <- as.numeric(tempPx[,i]) }; rm(i)
  tempPx[,paste0(Xx, "PSTUDENT")] <- factor(tempPx[,paste0(Xx, "PSTUDENT")], levels=c("Yes, full-time", "Yes, part-time", "No"))
  # tempPx[,paste0(Xx, "PSCHOOL_NAME")]
  # tempPx[,paste0(Xx, "PSCHOOL_STREET")]
  # tempPx[,paste0(Xx, "PSCHOOL_CITY")]
  tempPx1 <- tempPx[,1:which(names(tempPx)==paste0(Xx, "PSCHOOL_CITY"))]
  tempPx2 <- tempPx[,which(names(tempPx)==paste0(Xx, "T6_First.Click")):ncol(tempPx)]
  tempPx1[,paste0(Xx, "SMODE_1")] <- grepl("Walk", tempPx[,paste0(Xx, "Q34")])
  tempPx1[,paste0(Xx, "SMODE_2")] <- grepl("Bicycle", tempPx[,paste0(Xx, "Q34")])
  tempPx1[,paste0(Xx, "SMODE_3")] <- grepl("Car/Van/Truck/SUV Driver", tempPx[,paste0(Xx, "Q34")])
  tempPx1[,paste0(Xx, "SMODE_4")] <- grepl("Car/Van/Truck/SUV Passenger", tempPx[,paste0(Xx, "Q34")])
  tempPx1[,paste0(Xx, "SMODE_5")] <- grepl("Motorcycle/Scooter/Moped", tempPx[,paste0(Xx, "Q34")])
  tempPx1[,paste0(Xx, "SMODE_6")] <- grepl("Local Bus", tempPx[,paste0(Xx, "Q34")])
  tempPx1[,paste0(Xx, "SMODE_7")] <- grepl("School Bus", tempPx[,paste0(Xx, "Q34")])
  tempPx1[,paste0(Xx, "SMODE_8")] <- grepl("please specify", tempPx[,paste0(Xx, "Q34")])
  tempPx1[,paste0(Xx, "SMODE_8_TEXT")] <- tempPx[,paste0(Xx, "Q34_8_TEXT")]
  tempPx1[,paste0(Xx, "SMODE_9")] <- grepl("Typically no travel to/from school", tempPx[,paste0(Xx, "Q34")])
  if (j == 1) {
    # - NOTE: code removed for confidentiality
    # - example: tempPx1[which(tempPx1[,paste0(Xx, "SMODE_8_TEXT")]=="Online"), paste0(Xx, "SMODE_9")] <- TRUE
    # - example: tempPx1[which(tempPx1[,paste0(Xx, "SMODE_8_TEXT")]=="Online"), paste0(Xx, "SMODE_8")] <- FALSE
  }
  tempPx <- cbind(tempPx1, tempPx2)
  rm(tempPx1, tempPx2)
  for (i in grep("T6_", names(tempPx), value=T)) { tempPx[,i] <- as.numeric(tempPx[,i]) }; rm(i)
  tempPx[,paste0(Xx, "PWORKER")] <- factor(tempPx[,paste0(Xx, "PWORKER")], levels=c("Yes", "No"))
  tempPx[,paste0(Xx, "PWDAYS_COMMUTE")] <- as.integer(as.character(tempPx[,paste0(Xx, "PWDAYS_COMMUTE")]))
  tempPx[,paste0(Xx, "PWDAYS_HOME")] <- as.integer(as.character(tempPx[,paste0(Xx, "PWDAYS_HOME")]))
  tempPx[,paste0(Xx, "PWHOURS")] <- as.numeric(as.character(tempPx[,paste0(Xx, "PWHOURS")]))
  tempPx[,paste0(Xx, "PWFLEX")] <- factor(tempPx[,paste0(Xx, "PWFLEX")], levels=c("Very flexible", "Somewhat flexible", "Neither flexible nor inflexible", "Somewhat inflexible", "Very inflexible"))
  # tempPx[,paste0(Xx, "PWORK_NAME")]
  # tempPx[,paste0(Xx, "PWORK_STREET")]
  # tempPx[,paste0(Xx, "PWORK_CITY")]
  tempPx1 <- tempPx[,1:which(names(tempPx)==paste0(Xx, "PWORK_CITY"))]
  tempPx2 <- tempPx[,which(names(tempPx)==paste0(Xx, "T7_First.Click")):ncol(tempPx)]
  tempPx1[,paste0(Xx, "WMODE_1")] <- grepl("Walk", tempPx[,paste0(Xx, "Q41")])
  tempPx1[,paste0(Xx, "WMODE_2")] <- grepl("Bicycle", tempPx[,paste0(Xx, "Q41")])
  tempPx1[,paste0(Xx, "WMODE_3")] <- grepl("Car/Van/Truck/SUV Driver", tempPx[,paste0(Xx, "Q41")])
  tempPx1[,paste0(Xx, "WMODE_4")] <- grepl("Car/Van/Truck/SUV Passenger", tempPx[,paste0(Xx, "Q41")])
  tempPx1[,paste0(Xx, "WMODE_5")] <- grepl("Motorcycle/Scooter/Moped", tempPx[,paste0(Xx, "Q41")])
  tempPx1[,paste0(Xx, "WMODE_6")] <- grepl("Local Bus", tempPx[,paste0(Xx, "Q41")])
  tempPx1[,paste0(Xx, "WMODE_7")] <- grepl("please specify", tempPx[,paste0(Xx, "Q41")])
  tempPx1[,paste0(Xx, "WMODE_7_TEXT")] <- tempPx[,paste0(Xx, "Q41_7_TEXT")]
  tempPx1[,paste0(Xx, "WMODE_8")] <- grepl("Typically no travel to/from work", tempPx[,paste0(Xx, "Q41")])
  if (j == 1) {
    # - NOTE: code removed for confidentiality
    # - example: tempPx1[which(tempPx1[,paste0(Xx, "WMODE_7_TEXT")]=="Work provided shuttle"), paste0(Xx, "WMODE_4")] <- TRUE
    # - example: tempPx1[which(tempPx1[,paste0(Xx, "WMODE_7_TEXT")]=="Work provided shuttle"), paste0(Xx, "WMODE_7")] <- FALSE
  }
  tempPx <- cbind(tempPx1, tempPx2)
  rm(tempPx1, tempPx2)
  for (i in grep("T7_", names(tempPx), value=T)) { tempPx[,i] <- as.numeric(tempPx[,i]) }; rm(i)
  tempPx[,paste0(Xx, "PDRVLIC")] <- factor(tempPx[,paste0(Xx, "PDRVLIC")], levels=c("Yes", "No"))
  tempPx[,paste0(Xx, "PKNOW_BIKE")] <- factor(tempPx[,paste0(Xx, "PKNOW_BIKE")], levels=c("Yes", "Not well", "No"))
  tempPx[,paste0(Xx, "PKNOW_AUTO")] <- factor(tempPx[,paste0(Xx, "PKNOW_AUTO")], levels=c("Yes", "Not well", "No"))
  tempPx[,paste0(Xx, "PKNOW_TRAN")] <- factor(tempPx[,paste0(Xx, "PKNOW_TRAN")], levels=c("Yes", "Not well", "No"))
  tempPx1 <- tempPx[,1:which(names(tempPx)==paste0(Xx, "PKNOW_TRAN"))]
  tempPx2 <- tempPx[,which(names(tempPx)==paste0(Xx, "T8_First.Click")):ncol(tempPx)]
  tempPx1[,paste0(Xx, "PLIMIT_1")] <- grepl("Seeing", tempPx[,paste0(Xx, "Q45")])
  tempPx1[,paste0(Xx, "PLIMIT_2")] <- grepl("Hearing", tempPx[,paste0(Xx, "Q45")])
  tempPx1[,paste0(Xx, "PLIMIT_3")] <- grepl("Sitting", tempPx[,paste0(Xx, "Q45")])
  tempPx1[,paste0(Xx, "PLIMIT_4")] <- grepl("Standing", tempPx[,paste0(Xx, "Q45")])
  tempPx1[,paste0(Xx, "PLIMIT_5")] <- grepl("Climbing stairs", tempPx[,paste0(Xx, "Q45")])
  tempPx1[,paste0(Xx, "PLIMIT_6")] <- grepl("Walking", tempPx[,paste0(Xx, "Q45")])
  tempPx1[,paste0(Xx, "PLIMIT_7")] <- grepl("Riding a bicycle", tempPx[,paste0(Xx, "Q45")])
  tempPx1[,paste0(Xx, "PLIMIT_8")] <- grepl("Driving an automobile", tempPx[,paste0(Xx, "Q45")])
  tempPx1[,paste0(Xx, "PLIMIT_9")] <- grepl("Using public transit", tempPx[,paste0(Xx, "Q45")])
  tempPx1[,paste0(Xx, "PLIMIT_10")] <- grepl("None of the above", tempPx[,paste0(Xx, "Q45")])
  tempPx <- cbind(tempPx1, tempPx2)
  rm(tempPx1, tempPx2)
  for (i in grep("T8_", names(tempPx), value=T)) { tempPx[,i] <- as.numeric(tempPx[,i]) }; rm(i)
  tempPx[,paste0(Xx, "PMORE")] <- factor(tempPx[,paste0(Xx, "PMORE")], levels=c("Yes", "No"))
  for (i in grep("T9_", names(tempPx), value=T)) { tempPx[,i] <- as.numeric(tempPx[,i]) }; rm(i)
  # tempP[,grepl(Xx, names(tempP))] <- tempPx
  tempP0 <- cbind(tempP0, tempPx)
  rm(tempPx, Xx)
}; rm(j)
tempP0$one <- NULL
substr(names(tempP0), 1, 1) <- "P"
temp <- cbind(temp1, tempP0, temp2)
rm(temp1, temp2, tempP, tempP0)

# Transportation
names(temp)[which(names(temp)=="Q51")] <- "HHBIKES"
temp$HHBIKES <- factor(temp$HHBIKES, levels=c("0", "1", "2", "3", "4", "5", "6+"))
names(temp)[which(names(temp)=="Q52")] <- "HHCARS"
temp$HHCARS <- factor(temp$HHCARS, levels=c("Yes", "No"))
names(temp)[grepl("Q54_", names(temp))] <- gsub("Q54", "T10", names(temp)[grepl("Q54_", names(temp))])
for (i in grep("T10_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Vehicles
temp1 <- temp[,1:(which(names(temp)=="X1_Q56")-1)]
temp2 <- temp[,which(names(temp)=="Q62"):ncol(temp)]
tempV <- temp[,which(names(temp)=="X1_Q56"):(which(names(temp)=="Q62")-1)]
names(tempV)
names(tempV)[grepl("Q56", names(tempV))] <- gsub("_3", "", names(tempV)[grepl("Q56", names(tempV))])
names(tempV)[grepl("Q56", names(tempV))] <- gsub("Q56", "VTYPE", names(tempV)[grepl("Q56", names(tempV))])
names(tempV)[grepl("Q57", names(tempV))] <- gsub("Q57", "VPRIMARY", names(tempV)[grepl("Q57", names(tempV))])
names(tempV)[grepl("Q58_1", names(tempV))] <- gsub("Q58_1", "VYEAR", names(tempV)[grepl("Q58_1", names(tempV))])
names(tempV)[grepl("Q58_2", names(tempV))] <- gsub("Q58_2", "VMAKE", names(tempV)[grepl("Q58_2", names(tempV))])
names(tempV)[grepl("Q58_3", names(tempV))] <- gsub("Q58_3", "VMODEL", names(tempV)[grepl("Q58_3", names(tempV))])
names(tempV)[grepl("Q59", names(tempV))] <- gsub("Q59", "VMORE", names(tempV)[grepl("Q59", names(tempV))])
names(tempV)[grepl("Q60_", names(tempV))] <- gsub("Q60", "T11", names(tempV)[grepl("Q60_", names(tempV))])
for (j in 1:10) {
  Xx <- paste0("X", j, "_")
  tempVx <- tempV[,grepl(Xx, names(tempV))]
  tempVx[,paste0(Xx, "VTYPE")] <- factor(tempVx[,paste0(Xx, "VTYPE")], levels=c("Car/Van/Truck/SUV", "Motorcycle/Scooter/Moped", "Other (please describe)"))
  # tempVx[,paste0(Xx, "VTYPE_TEXT")]
  if (j == 1) {
    # - NOTE: code removed for confidentiality
    # - example: tempVx[which(tempVx[,paste0(Xx, "VTYPE_TEXT")]=="Jeep"), paste0(Xx, "VTYPE")] <- "Car/Van/Truck/SUV"
  }
  tempVx[,paste0(Xx, "VPRIMARY")] <- factor(tempVx[,paste0(Xx, "VPRIMARY")], levels=c("Primary household vehicle", "Secondary household vehicle"))
  tempVx[,paste0(Xx, "VYEAR")] <- as.integer(tempVx[,paste0(Xx, "VYEAR")])
  if (j == 1) {
    # - NOTE: code removed for confidentiality
    # - example: tempVx[which(tempVx[,paste0(Xx, "VYEAR")]==2105), paste0(Xx, "VYEAR")] <- 2015
  }
  # tempVx[,paste0(Xx, "VMAKE")]
  # tempVx[,paste0(Xx, "VMODEL")]
  tempVx[,paste0(Xx, "VMORE")] <- factor(tempVx[,paste0(Xx, "VMORE")], levels=c("Yes", "No"))
  for (i in grep("T11_", names(tempVx), value=T)) { tempVx[,i] <- as.numeric(tempVx[,i]) }; rm(i)
  tempV[,grepl(Xx, names(tempV))] <- tempVx
  rm(tempVx, Xx)
}; rm(j)
substr(names(tempV), 1, 1) <- "V"
temp <- cbind(temp1, tempV, temp2)
rm(temp1, temp2, tempV)

# End
names(temp)[which(names(temp)=="Q62")] <- "COMMENTS"
names(temp)[grepl("Q64_", names(temp))] <- gsub("Q64", "T12", names(temp)[grepl("Q64_", names(temp))])
for (i in grep("T12_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

########################################
# Save

# Change names
initial <- temp
initial_names <- temp_names
row.names(initial) <- NULL

# Inspect
names(initial)
str(initial, list.len=ncol(initial))
summary(initial)
str(initial_names)

# Save
t_folder <- "Data 2 Cleaned"
saveRDS(initial, file=file.path("Data", "Survey 2019 Winter", t_folder, "initial.rds"))
write.csv(initial, file=file.path("Data", "Survey 2019 Winter", t_folder, "initial.csv"), row.names=F)
write.csv(initial_names, file=file.path("Data", "Survey 2019 Winter", t_folder, "initial_names.csv"), row.names=F)
rm(t_folder)

########################################
# Cleanup

# Remove
rm(initial, initial_names)
rm(temp, temp_names)
rm(old_temp)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################