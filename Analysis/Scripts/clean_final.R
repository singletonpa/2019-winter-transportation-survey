########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     clean_final.R
# About:    Script to clean final survey
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
  "2019 Winter Transportation Study -- Final Survey_May 23, 2019_15.49.csv"), 
  stringsAsFactors=F)
temp <- temp[-c(1:2),]
# names
tempN <- read.csv(file=file.path("Data", "Survey 2019 Winter", "Data 0 Raw", 
  "2019 Winter Transportation Study -- Final Survey_May 23, 2019_15.49.csv"), 
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
names(temp)[grepl("Q4_", names(temp))] <- gsub("Q4", "T1", names(temp)[grepl("Q4_", names(temp))])
for (i in grep("T1_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Start 2
names(temp)[which(names(temp)=="Q5")] <- "HHResponseId"
temp$HHResponseId <- gsub("â€‹", "", temp$HHResponseId)
temp$HHResponseId <- gsub(": ", "", temp$HHResponseId)
temp$HHResponseId <- gsub(" ", "", temp$HHResponseId)
temp$HHResponseId <- gsub("​", "", temp$HHResponseId)
# - NOTE: code removed for confidentiality
# - example: temp$HHResponseId[which(temp$HHResponseId=="R_2WC1VnOdfakNahi")] <- "R_2WC1VnOdfakNahI"
names(temp)[which(names(temp)=="Q6")] <- "PERNAME"
temp$PERNAME <- trimws(temp$PERNAME)
# - NOTE: code removed for confidentiality
# - example: temp$PERNAME[c(1)] <- c("Patrick")
names(temp)[which(names(temp)=="Q7")] <- "CHANGES_TEXT"
names(temp)[grepl("Q8_", names(temp))] <- gsub("Q8", "T2", names(temp)[grepl("Q8_", names(temp))])
for (i in grep("T2_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Attitudes
names(temp)[grepl("Q77", names(temp))] <- paste("VALUES", c(1:9), sep="_")
temp$VALUES_1 <- factor(temp$VALUES_1, levels=c("Opposed to my values", "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"))
temp$VALUES_2 <- factor(temp$VALUES_2, levels=c("Opposed to my values", "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"))
temp$VALUES_3 <- factor(temp$VALUES_3, levels=c("Opposed to my values", "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"))
temp$VALUES_4 <- factor(temp$VALUES_4, levels=c("Opposed to my values", "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"))
temp$VALUES_5 <- factor(temp$VALUES_5, levels=c("Opposed to my values", "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"))
temp$VALUES_6 <- factor(temp$VALUES_6, levels=c("Opposed to my values", "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"))
temp$VALUES_7 <- factor(temp$VALUES_7, levels=c("Opposed to my values", "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"))
temp$VALUES_8 <- factor(temp$VALUES_8, levels=c("Opposed to my values", "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"))
temp$VALUES_9 <- factor(temp$VALUES_9, levels=c("Opposed to my values", "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"))
names(temp)[grepl("Q17_", names(temp))] <- gsub("Q17", "T3", names(temp)[grepl("Q17_", names(temp))])
for (i in grep("T3_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)
names(temp)[grepl("Q12", names(temp))] <- paste("ATT_ENVR", c(1:8), sep="_")
temp$ATT_ENVR_1 <- factor(temp$ATT_ENVR_1, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_2 <- factor(temp$ATT_ENVR_2, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_3 <- factor(temp$ATT_ENVR_3, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_4 <- factor(temp$ATT_ENVR_4, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_5 <- factor(temp$ATT_ENVR_5, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_6 <- factor(temp$ATT_ENVR_6, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_7 <- factor(temp$ATT_ENVR_7, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_8 <- factor(temp$ATT_ENVR_8, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
names(temp)[grepl("Q13_", names(temp))] <- gsub("Q13", "T4", names(temp)[grepl("Q13_", names(temp))])
for (i in grep("T4_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)
names(temp)[grepl("Q75", names(temp))] <- paste("ATT_ENVR", c(9:16), sep="_")
temp$ATT_ENVR_9 <- factor(temp$ATT_ENVR_9, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_10 <- factor(temp$ATT_ENVR_10, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_11 <- factor(temp$ATT_ENVR_11, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_12 <- factor(temp$ATT_ENVR_12, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_13 <- factor(temp$ATT_ENVR_13, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_14 <- factor(temp$ATT_ENVR_14, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_15 <- factor(temp$ATT_ENVR_15, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_ENVR_16 <- factor(temp$ATT_ENVR_16, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
names(temp)[grepl("Q76_", names(temp))] <- gsub("Q76", "T5", names(temp)[grepl("Q76_", names(temp))])
for (i in grep("T5_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)
names(temp)[grepl("HS1", names(temp))] <- paste("ATT_HEALTH", c(1:8), sep="_")
temp$ATT_HEALTH_1 <- factor(temp$ATT_HEALTH_1, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_HEALTH_2 <- factor(temp$ATT_HEALTH_2, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_HEALTH_3 <- factor(temp$ATT_HEALTH_3, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_HEALTH_4 <- factor(temp$ATT_HEALTH_4, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_HEALTH_5 <- factor(temp$ATT_HEALTH_5, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_HEALTH_6 <- factor(temp$ATT_HEALTH_6, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_HEALTH_7 <- factor(temp$ATT_HEALTH_7, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
temp$ATT_HEALTH_8 <- factor(temp$ATT_HEALTH_8, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
names(temp)[grepl("Q15_", names(temp))] <- gsub("Q15", "T6", names(temp)[grepl("Q15_", names(temp))])
for (i in grep("T6_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Efficacy & norms
names(temp)[grepl("Q19", names(temp))] <- paste("NORMS_ME", c(1:4), sep="_")
names(temp)[grepl("Q20", names(temp))] <- paste("NORMS_PEER", c(1:4), sep="_")
names(temp)[grepl("Q21", names(temp))] <- paste("NORMS_ORGS", c(1:4), sep="_")
temp$NORMS_ME_1 <- factor(temp$NORMS_ME_1, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_ME_2 <- factor(temp$NORMS_ME_2, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_ME_3 <- factor(temp$NORMS_ME_3, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_ME_4 <- factor(temp$NORMS_ME_4, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_PEER_1 <- factor(temp$NORMS_PEER_1, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_PEER_2 <- factor(temp$NORMS_PEER_2, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_PEER_3 <- factor(temp$NORMS_PEER_3, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_PEER_4 <- factor(temp$NORMS_PEER_4, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_ORGS_1 <- factor(temp$NORMS_ORGS_1, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_ORGS_2 <- factor(temp$NORMS_ORGS_2, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_ORGS_3 <- factor(temp$NORMS_ORGS_3, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
temp$NORMS_ORGS_4 <- factor(temp$NORMS_ORGS_4, levels=c("Much less", "Somewhat less", "About the same", "Somewhat more", "Much more", "No opinion"))
names(temp)[grepl("Q22_", names(temp))] <- gsub("Q22", "T7", names(temp)[grepl("Q22_", names(temp))])
for (i in grep("T7_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)
names(temp)[grepl("Q23", names(temp))] <- paste("SELFEFF", c(1:6), sep="_")
temp$SELFEFF_1 <- factor(temp$SELFEFF_1, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "Not applicable"))
temp$SELFEFF_2 <- factor(temp$SELFEFF_2, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "Not applicable"))
temp$SELFEFF_3 <- factor(temp$SELFEFF_3, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "Not applicable"))
temp$SELFEFF_4 <- factor(temp$SELFEFF_4, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "Not applicable"))
temp$SELFEFF_5 <- factor(temp$SELFEFF_5, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "Not applicable"))
temp$SELFEFF_6 <- factor(temp$SELFEFF_6, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "Not applicable"))
names(temp)[grepl("Q24_", names(temp))] <- gsub("Q24", "T8", names(temp)[grepl("Q24_", names(temp))])
for (i in grep("T8_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Awareness
names(temp)[grepl("Q27", names(temp))] <- paste("AQ_IMPACTS", c(1:5), sep="_")
temp$AQ_IMPACTS_1 <- factor(temp$AQ_IMPACTS_1, levels=c("Not at all", "A little", "Moderately", "A lot", "No opinion"))
temp$AQ_IMPACTS_2 <- factor(temp$AQ_IMPACTS_2, levels=c("Not at all", "A little", "Moderately", "A lot", "No opinion"))
temp$AQ_IMPACTS_3 <- factor(temp$AQ_IMPACTS_3, levels=c("Not at all", "A little", "Moderately", "A lot", "No opinion"))
temp$AQ_IMPACTS_4 <- factor(temp$AQ_IMPACTS_4, levels=c("Not at all", "A little", "Moderately", "A lot", "No opinion"))
temp$AQ_IMPACTS_5 <- factor(temp$AQ_IMPACTS_5, levels=c("Not at all", "A little", "Moderately", "A lot", "No opinion"))
names(temp)[grepl("Q28", names(temp))] <- paste("AQ_CAUSES", c(1:5), sep="_")
temp$AQ_CAUSES_1 <- factor(temp$AQ_CAUSES_1, levels=c("Not at all", "A little", "Moderately", "A lot", "No opinion"))
temp$AQ_CAUSES_2 <- factor(temp$AQ_CAUSES_2, levels=c("Not at all", "A little", "Moderately", "A lot", "No opinion"))
temp$AQ_CAUSES_3 <- factor(temp$AQ_CAUSES_3, levels=c("Not at all", "A little", "Moderately", "A lot", "No opinion"))
temp$AQ_CAUSES_4 <- factor(temp$AQ_CAUSES_4, levels=c("Not at all", "A little", "Moderately", "A lot", "No opinion"))
temp$AQ_CAUSES_5 <- factor(temp$AQ_CAUSES_5, levels=c("Not at all", "A little", "Moderately", "A lot", "No opinion"))
names(temp)[grepl("Q29_", names(temp))] <- gsub("Q29", "T9", names(temp)[grepl("Q29_", names(temp))])
for (i in grep("T9_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)
names(temp)[grepl("Q31", names(temp))] <- paste("AQ_ME", c(1:6), sep="_")
names(temp)[grepl("Q32", names(temp))] <- paste("AQ_PEER", c(1:4), sep="_")
names(temp)[grepl("Q33", names(temp))] <- paste("AQ_ORGS", c(1:4), sep="_")
temp$AQ_ME_1 <- factor(temp$AQ_ME_1, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_ME_2 <- factor(temp$AQ_ME_2, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_ME_3 <- factor(temp$AQ_ME_3, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_ME_4 <- factor(temp$AQ_ME_4, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_ME_5 <- factor(temp$AQ_ME_5, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_ME_6 <- factor(temp$AQ_ME_6, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_PEER_1 <- factor(temp$AQ_PEER_1, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_PEER_2 <- factor(temp$AQ_PEER_2, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_PEER_3 <- factor(temp$AQ_PEER_3, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_PEER_4 <- factor(temp$AQ_PEER_4, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_ORGS_1 <- factor(temp$AQ_ORGS_1, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_ORGS_2 <- factor(temp$AQ_ORGS_2, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_ORGS_3 <- factor(temp$AQ_ORGS_3, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
temp$AQ_ORGS_4 <- factor(temp$AQ_ORGS_4, levels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "No opinion"))
names(temp)[grepl("Q34_", names(temp))] <- gsub("Q34", "T10", names(temp)[grepl("Q34_", names(temp))])
for (i in grep("T10_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)
names(temp)[grepl("Q36", names(temp))] <- paste("IDLING", c(1:6,"6_TEXT"), sep="_")
temp$IDLING_1 <- factor(temp$IDLING_1, levels=c("Never", "Rarely", "Sometimes", "Frequently", "Always", "Not applicable"))
temp$IDLING_2 <- factor(temp$IDLING_2, levels=c("Never", "Rarely", "Sometimes", "Frequently", "Always", "Not applicable"))
temp$IDLING_3 <- factor(temp$IDLING_3, levels=c("Never", "Rarely", "Sometimes", "Frequently", "Always", "Not applicable"))
temp$IDLING_4 <- factor(temp$IDLING_4, levels=c("Never", "Rarely", "Sometimes", "Frequently", "Always", "Not applicable"))
temp$IDLING_5 <- factor(temp$IDLING_5, levels=c("Never", "Rarely", "Sometimes", "Frequently", "Always", "Not applicable"))
temp$IDLING_6 <- factor(temp$IDLING_6, levels=c("Never", "Rarely", "Sometimes", "Frequently", "Always", "Not applicable"))
names(temp)[which(names(temp)=="Q37")] <- "IDLING_LOGAN"
temp$IDLING_LOGAN <- factor(temp$IDLING_LOGAN, levels=c(NA, "I have no idea.", "There are no anti-idling ordinances in Logan.", "Idling vehicles must not be left unattended.", "Idling for longer than 2 minutes is prohibited when the temperature is warmer than 0Â°F.", "Idling for any length of time is prohibited during an inversion event."))
names(temp)[grepl("Q38_", names(temp))] <- gsub("Q38", "T11", names(temp)[grepl("Q38_", names(temp))])
for (i in grep("T11_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Behavior change
temp1 <- temp[,1:(which(names(temp)=="Q40")-1)]
temp2 <- temp[,(which(names(temp)=="Q40")+2):ncol(temp)]
temp1$BEHCHG_ME_1 <- grepl("I carpool with others to work or school.", temp$Q40)
temp1$BEHCHG_ME_2 <- grepl("I use public transit for some/all of my trips.", temp$Q40)
temp1$BEHCHG_ME_3 <- grepl("I walk or bicycle for some/all of my trips.", temp$Q40)
temp1$BEHCHG_ME_4 <- grepl("I try to trip chain, grouping my errands into one trip instead of returning home after each one.", temp$Q40)
temp1$BEHCHG_ME_5 <- grepl("I telecommute, working or studying at home.", temp$Q40)
temp1$BEHCHG_ME_6 <- grepl("I skip or postpone making some trips until the air quality is better.", temp$Q40)
temp1$BEHCHG_ME_7 <- grepl("I make sure not to idle or keep my motor vehicle running when parked.", temp$Q40)
temp1$BEHCHG_ME_8 <- grepl("please specify", temp$Q40)
temp1$BEHCHG_ME_8_TEXT <- temp$Q40_8_TEXT
# don't need to change open responses into categories
temp1$BEHCHG_ME_9 <- grepl("None of the above", temp$Q40)
temp1[which(temp1$BEHCHG_ME_8_TEXT %in% c("Stay inside on red air days", "stay inside on red air days")),"BEHCHG_ME_6"] <- TRUE
temp <- cbind(temp1, temp2)
rm(temp1, temp2)
names(temp)[grepl("Q43_", names(temp))] <- gsub("Q43", "T12", names(temp)[grepl("Q43_", names(temp))])
for (i in grep("T12_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)
names(temp)[grepl("Q71", names(temp))] <- paste("BEHCHG_POP", c(1:8,"8_TEXT",9), sep="_")
temp$BEHCHG_POP_1 <- factor(temp$BEHCHG_POP_1, levels=c("<5%", "20%", "35%", "50%", "65%", "80%", ">95%"))
temp$BEHCHG_POP_2 <- factor(temp$BEHCHG_POP_2, levels=c("<5%", "20%", "35%", "50%", "65%", "80%", ">95%"))
temp$BEHCHG_POP_3 <- factor(temp$BEHCHG_POP_3, levels=c("<5%", "20%", "35%", "50%", "65%", "80%", ">95%"))
temp$BEHCHG_POP_4 <- factor(temp$BEHCHG_POP_4, levels=c("<5%", "20%", "35%", "50%", "65%", "80%", ">95%"))
temp$BEHCHG_POP_5 <- factor(temp$BEHCHG_POP_5, levels=c("<5%", "20%", "35%", "50%", "65%", "80%", ">95%"))
temp$BEHCHG_POP_6 <- factor(temp$BEHCHG_POP_6, levels=c("<5%", "20%", "35%", "50%", "65%", "80%", ">95%"))
temp$BEHCHG_POP_7 <- factor(temp$BEHCHG_POP_7, levels=c("<5%", "20%", "35%", "50%", "65%", "80%", ">95%"))
temp$BEHCHG_POP_8 <- factor(temp$BEHCHG_POP_8, levels=c("<5%", "20%", "35%", "50%", "65%", "80%", ">95%"))
temp$BEHCHG_POP_9 <- factor(temp$BEHCHG_POP_9, levels=c("<5%", "20%", "35%", "50%", "65%", "80%", ">95%"))
names(temp)[grepl("Q45_", names(temp))] <- gsub("Q45", "T13", names(temp)[grepl("Q45_", names(temp))])
for (i in grep("T13_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)
temp1 <- temp[,1:(which(names(temp)=="Q47_0_GROUP")-1)]
temp2 <- temp[,(which(names(temp)=="Q47_2_16_RANK")+1):ncol(temp)]
temp1$SUPPORT_1 <- ifelse(grepl("Offering financial incentives for getting rid of older or higher-polluting motor vehicles", temp$Q47_0_GROUP), "I would strongly support this", 
                   ifelse(grepl("Offering financial incentives for getting rid of older or higher-polluting motor vehicles", temp$Q47_1_GROUP), "I might support this", 
                   ifelse(grepl("Offering financial incentives for getting rid of older or higher-polluting motor vehicles", temp$Q47_2_GROUP), "I would never support this", NA)))
temp1$SUPPORT_2 <- ifelse(grepl("Offering discounts on the purchase of hybrid or electric motor vehicles", temp$Q47_0_GROUP), "I would strongly support this", 
                   ifelse(grepl("Offering discounts on the purchase of hybrid or electric motor vehicles", temp$Q47_1_GROUP), "I might support this", 
                   ifelse(grepl("Offering discounts on the purchase of hybrid or electric motor vehicles", temp$Q47_2_GROUP), "I would never support this", NA)))
temp1$SUPPORT_3 <- ifelse(grepl("Providing more information and education about air quality issues and causes", temp$Q47_0_GROUP), "I would strongly support this", 
                   ifelse(grepl("Providing more information and education about air quality issues and causes", temp$Q47_1_GROUP), "I might support this", 
                   ifelse(grepl("Providing more information and education about air quality issues and causes", temp$Q47_2_GROUP), "I would never support this", NA)))
temp1$SUPPORT_4 <- ifelse(grepl("Restricting the development of farm and natural land and concentrating development in existing cities/towns", temp$Q47_0_GROUP), "I would strongly support this", 
                   ifelse(grepl("Restricting the development of farm and natural land and concentrating development in existing cities/towns", temp$Q47_1_GROUP), "I might support this", 
                   ifelse(grepl("Restricting the development of farm and natural land and concentrating development in existing cities/towns", temp$Q47_2_GROUP), "I would never support this", NA)))
temp1$SUPPORT_5 <- ifelse(grepl("Imposing stricter vehicle emissions inspection standards", temp$Q47_0_GROUP), "I would strongly support this", 
                   ifelse(grepl("Imposing stricter vehicle emissions inspection standards", temp$Q47_1_GROUP), "I might support this", 
                   ifelse(grepl("Imposing stricter vehicle emissions inspection standards", temp$Q47_2_GROUP), "I would never support this", NA)))
temp1$SUPPORT_6 <- ifelse(grepl("Increasing fuel taxes to pay for more non-automobile transportation options", temp$Q47_0_GROUP), "I would strongly support this", 
                   ifelse(grepl("Increasing fuel taxes to pay for more non-automobile transportation options", temp$Q47_1_GROUP), "I might support this", 
                   ifelse(grepl("Increasing fuel taxes to pay for more non-automobile transportation options", temp$Q47_2_GROUP), "I would never support this", NA)))
temp1$SUPPORT_7 <- ifelse(grepl("Having local governments sell \"clean air bonds\" to pay for more non-automobile transportation options", temp$Q47_0_GROUP), "I would strongly support this", 
                   ifelse(grepl("Having local governments sell \"clean air bonds\" to pay for more non-automobile transportation options", temp$Q47_1_GROUP), "I might support this", 
                   ifelse(grepl("Having local governments sell \"clean air bonds\" to pay for more non-automobile transportation options", temp$Q47_2_GROUP), "I would never support this", NA)))
temp1$SUPPORT_8 <- ifelse(grepl("Increasing vehicle registration fees to pay for more non-automobile transportation options", temp$Q47_0_GROUP), "I would strongly support this", 
                   ifelse(grepl("Increasing vehicle registration fees to pay for more non-automobile transportation options", temp$Q47_1_GROUP), "I might support this", 
                   ifelse(grepl("Increasing vehicle registration fees to pay for more non-automobile transportation options", temp$Q47_2_GROUP), "I would never support this", NA)))
temp1$SUPPORT_1 <- factor(temp1$SUPPORT_1, levels=c("I would strongly support this", "I might support this", "I would never support this"))
temp1$SUPPORT_2 <- factor(temp1$SUPPORT_2, levels=c("I would strongly support this", "I might support this", "I would never support this"))
temp1$SUPPORT_3 <- factor(temp1$SUPPORT_3, levels=c("I would strongly support this", "I might support this", "I would never support this"))
temp1$SUPPORT_4 <- factor(temp1$SUPPORT_4, levels=c("I would strongly support this", "I might support this", "I would never support this"))
temp1$SUPPORT_5 <- factor(temp1$SUPPORT_5, levels=c("I would strongly support this", "I might support this", "I would never support this"))
temp1$SUPPORT_6 <- factor(temp1$SUPPORT_6, levels=c("I would strongly support this", "I might support this", "I would never support this"))
temp1$SUPPORT_7 <- factor(temp1$SUPPORT_7, levels=c("I would strongly support this", "I might support this", "I would never support this"))
temp1$SUPPORT_8 <- factor(temp1$SUPPORT_8, levels=c("I would strongly support this", "I might support this", "I would never support this"))
temp <- cbind(temp1, temp2)
rm(temp1, temp2)
names(temp)[which(names(temp)=="Q50")] <- "OTHER_IDEAS"
names(temp)[grepl("Q51_", names(temp))] <- gsub("Q51", "T14", names(temp)[grepl("Q51_", names(temp))])
for (i in grep("T14_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# Organizations
names(temp)[grepl("Q57", names(temp))] <- paste("ORGPROG", c(1:6), sep="_")
temp$ORGPROG_1 <- factor(temp$ORGPROG_1, levels=c("Never heard of it", "Heard of it, but not familiar with it", "Familiar with it"))
temp$ORGPROG_2 <- factor(temp$ORGPROG_2, levels=c("Never heard of it", "Heard of it, but not familiar with it", "Familiar with it"))
temp$ORGPROG_3 <- factor(temp$ORGPROG_3, levels=c("Never heard of it", "Heard of it, but not familiar with it", "Familiar with it"))
temp$ORGPROG_4 <- factor(temp$ORGPROG_4, levels=c("Never heard of it", "Heard of it, but not familiar with it", "Familiar with it"))
temp$ORGPROG_5 <- factor(temp$ORGPROG_5, levels=c("Never heard of it", "Heard of it, but not familiar with it", "Familiar with it"))
temp$ORGPROG_6 <- factor(temp$ORGPROG_6, levels=c("Never heard of it", "Heard of it, but not familiar with it", "Familiar with it"))
names(temp)[grepl("Q63_", names(temp))] <- gsub("Q63", "T15", names(temp)[grepl("Q63_", names(temp))])
for (i in grep("T15_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

# End
names(temp)[which(names(temp)=="Q67")] <- "COMMENTS"
names(temp)[grepl("Q69_", names(temp))] <- gsub("Q69", "T16", names(temp)[grepl("Q69_", names(temp))])
for (i in grep("T16_", names(temp), value=T)) { temp[,i] <- as.numeric(temp[,i]) }; rm(i)

########################################
# Check HHResponseId, PERNAME

# Load formatted HH and PER files
HH <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "HH.rds"))
PER <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PER.rds"))

# Inspect
str(HH)
str(PER)

# Check HHResponseId
table(temp$HHResponseId %in% HH$ResponseId) # 8
tempa <- temp[which(!(temp$HHResponseId %in% HH$ResponseId)),] # blanks/incompletes
rm(tempa)

# Check PERNAME
table(temp$PERNAME %in% PER$NAME) # 8
tempa <- temp[which(!(temp$PERNAME %in% PER$NAME)),] # blanks/incompletes
rm(tempa)

# Check HHResponseId & PERNAME
temp$HHPER <- paste(temp$HHResponseId, temp$PERNAME, sep=" ")
PER$HHPER <- paste(PER$ResponseId, PER$NAME, sep=" ")
table(temp$HHPER %in% PER$HHPER) # 8
tempa <- temp[which(!(temp$HHPER %in% PER$HHPER)),] # blanks/incompletes
rm(tempa)
temp$HHPER <- NULL
PER$HHPER <- NULL

# Cleanup
rm(HH, PER)

########################################
# Save

# Change names
final <- temp
final_names <- temp_names
row.names(final) <- NULL

# Inspect
names(final)
str(final)
summary(final)
str(final_names)

# Save
t_folder <- "Data 2 Cleaned"
saveRDS(final, file=file.path("Data", "Survey 2019 Winter", t_folder, "final.rds"))
write.csv(final, file=file.path("Data", "Survey 2019 Winter", t_folder, "final.csv"), row.names=F)
write.csv(final_names, file=file.path("Data", "Survey 2019 Winter", t_folder, "final_names.csv"), row.names=F)
rm(t_folder)

########################################
# Cleanup

# Remove
rm(final, final_names)
rm(temp, temp_names)
rm(old_temp)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################