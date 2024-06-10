########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     format_final.R
# About:    Script to format final survey
########################################

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
# library("")

########################################
# Load data

# Read rds data file
final <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 2 Cleaned", "final.rds"))

# Backup
temp <- final

# Inspect
names(temp)
str(temp, list.len=ncol(temp))

########################################
# Assemble metadata

# Initialize
mycols <- c("ResponseId", "StartDate", "EndDate", "Status", "IPAddress", 
            "Progress", "Duration", "Finished", "RecordedDate", 
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
mycols <- c(mycols, grep("T8_", names(temp), value=T))
mycols <- c(mycols, grep("T9_", names(temp), value=T))
mycols <- c(mycols, grep("T10_", names(temp), value=T))
mycols <- c(mycols, grep("T11_", names(temp), value=T))
mycols <- c(mycols, grep("T12_", names(temp), value=T))
mycols <- c(mycols, grep("T13_", names(temp), value=T))
mycols <- c(mycols, grep("T14_", names(temp), value=T))
mycols <- c(mycols, grep("T15_", names(temp), value=T))
mycols <- c(mycols, grep("T16_", names(temp), value=T))
meta_final <- data.frame(temp[,mycols])
rm(mycols)

# Inspect
names(meta_final)
str(meta_final)
# summary(meta_final)

########################################
# Assemble person data

# Initialize
mycols <- c("ResponseId", "HHResponseId", "PERNAME", "CHANGES_TEXT")
mycols <- c(mycols, grep("VALUES_", names(temp), value=T))
mycols <- c(mycols, grep("ATT_", names(temp), value=T))
mycols <- c(mycols, grep("NORMS_", names(temp), value=T))
mycols <- c(mycols, grep("SELFEFF_", names(temp), value=T))
mycols <- c(mycols, grep("AQ_", names(temp), value=T))
mycols <- c(mycols, grep("IDLING_", names(temp), value=T))
mycols <- c(mycols, grep("BEHCHG_", names(temp), value=T))
mycols <- c(mycols, grep("SUPPORT_", names(temp), value=T))
mycols <- c(mycols, c("OTHER_IDEAS"))
mycols <- c(mycols, grep("ORGPROG_", names(temp), value=T))
mycols <- c(mycols, c("COMMENTS"))
PER_final <- data.frame(temp[,c(mycols)])
rm(mycols)

# Remove empty, fix NAs, uniform responses
# - NOTE: code removed for confidentiality
# - example: t1 <- c("", "none", "No", "No changes", "na", "N/A")
# - example: PER_final$CHANGES_TEXT[which(PER_final$CHANGES_TEXT %in% t1)] <- NA
# - example: rm(t1)
t1 <- grep("VALUES", names(PER_final), value=T)
table(rowSums(PER_final[,t1]=="Opposed to my values", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="Opposed to my values", na.rm=T)>0) # 13
# PER_final[t2,t1][PER_final[t2,t1]=="Opposed to my values"] <- NA
# for (i in t1) {
#   PER_final[,i] <- droplevels(PER_final[,i])
# }; rm(i)
rm(t2, t1)
t1 <- grep("ATT_ENVR", names(PER_final), value=T)
table(rowSums(is.na(PER_final[,t1]), na.rm=T))
rm(t1)
t1 <- grep("ATT_HEALTH", names(PER_final), value=T)
table(rowSums(is.na(PER_final[,t1]), na.rm=T))
rm(t1)
t1 <- grep("NORMS_ME", names(PER_final), value=T)
table(rowSums(PER_final[,t1]=="No opinion", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="No opinion", na.rm=T)>0) # 65
# PER_final[t2,t1][PER_final[t2,t1]=="No opinion"] <- NA
# for (i in t1) {
#   PER_final[,i] <- droplevels(PER_final[,i])
# }; rm(i)
rm(t2, t1)
t1 <- grep("NORMS_PEER", names(PER_final), value=T)
table(rowSums(PER_final[,t1]=="No opinion", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="No opinion", na.rm=T)>0) # 111
# PER_final[t2,t1][PER_final[t2,t1]=="No opinion"] <- NA
# for (i in t1) {
#   PER_final[,i] <- droplevels(PER_final[,i])
# }; rm(i)
rm(t2, t1)
t1 <- grep("NORMS_ORGS", names(PER_final), value=T)
table(rowSums(PER_final[,t1]=="No opinion", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="No opinion", na.rm=T)>0) # 46
# PER_final[t2,t1][PER_final[t2,t1]=="No opinion"] <- NA
# for (i in t1) {
#   PER_final[,i] <- droplevels(PER_final[,i])
# }; rm(i)
rm(t2, t1)
t1 <- grep("SELFEFF", names(PER_final), value=T)
table(rowSums(PER_final[,t1]=="Not applicable", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="Not applicable", na.rm=T)>0) # 6
PER_final[t2,t1][PER_final[t2,t1]=="Not applicable"] <- NA
for (i in t1) {
  PER_final[,i] <- droplevels(PER_final[,i])
}; rm(i)
rm(t2, t1)
t1 <- grep("AQ_IMPACTS", names(PER_final), value=T)
table(rowSums(PER_final[,t1]=="No opinion", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="No opinion", na.rm=T)>0) # 57
# PER_final[t2,t1][PER_final[t2,t1]=="No opinion"] <- NA
# for (i in t1) {
#   PER_final[,i] <- droplevels(PER_final[,i])
# }; rm(i)
rm(t2, t1)
t1 <- grep("AQ_CAUSES", names(PER_final), value=T)
table(rowSums(PER_final[,t1]=="No opinion", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="No opinion", na.rm=T)>0) # 27
# PER_final[t2,t1][PER_final[t2,t1]=="No opinion"] <- NA
# for (i in t1) {
#   PER_final[,i] <- droplevels(PER_final[,i])
# }; rm(i)
rm(t2, t1)
t1 <- grep("AQ_ME", names(PER_final), value=T)
table(rowSums(PER_final[,t1]=="No opinion", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="No opinion", na.rm=T)>0) # 21
# PER_final[t2,t1][PER_final[t2,t1]=="No opinion"] <- NA
# for (i in t1) {
#   PER_final[,i] <- droplevels(PER_final[,i])
# }; rm(i)
rm(t2, t1)
t1 <- grep("AQ_PEER", names(PER_final), value=T)
table(rowSums(PER_final[,t1]=="No opinion", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="No opinion", na.rm=T)>0) # 48
# PER_final[t2,t1][PER_final[t2,t1]=="No opinion"] <- NA
# for (i in t1) {
#   PER_final[,i] <- droplevels(PER_final[,i])
# }; rm(i)
rm(t2, t1)
t1 <- grep("AQ_ORGS", names(PER_final), value=T)
table(rowSums(PER_final[,t1]=="No opinion", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="No opinion", na.rm=T)>0) # 35
# PER_final[t2,t1][PER_final[t2,t1]=="No opinion"] <- NA
# for (i in t1) {
#   PER_final[,i] <- droplevels(PER_final[,i])
# }; rm(i)
rm(t2, t1)
t1 <- paste0("IDLING_", c(1:6))
table(rowSums(PER_final[,t1]=="Not applicable", na.rm=T))
t2 <- which(rowSums(PER_final[,t1]=="Not applicable", na.rm=T)>0) # 114
PER_final[t2,t1][PER_final[t2,t1]=="Not applicable"] <- NA
for (i in t1) {
  PER_final[,i] <- droplevels(PER_final[,i])
}; rm(i)
rm(t2, t1)
PER_final$IDLING_6_TEXT[which(PER_final$IDLING_6_TEXT=="")] <- NA
t1 <- paste0("BEHCHG_ME_", c(1:9))
table(rowSums(PER_final[,t1]))
t2 <- which(rowSums(PER_final[,t1])==0) # 13
PER_final[t2,t1] <- NA
rm(t2, t1)
PER_final$BEHCHG_ME_8_TEXT[which(PER_final$BEHCHG_ME_8_TEXT=="")] <- NA
t1 <- paste0("BEHCHG_POP_", c(1:9))
table(rowSums(is.na(PER_final[,t1]), na.rm=T))
rm(t1)
PER_final$BEHCHG_POP_8_TEXT[which(PER_final$BEHCHG_POP_8_TEXT=="")] <- NA
t1 <- grep("SUPPORT_", names(PER_final), value=T)
table(rowSums(is.na(PER_final[,t1]), na.rm=T))
t2 <- which(rowSums(is.na(PER_final[,t1]), na.rm=T) %in% c(1,2,3,4)) # 18
# PER_final[t2,t1][is.na(PER_final[t2,t1])] <- "I might support this"
rm(t2, t1)
PER_final$OTHER_IDEAS[which(PER_final$OTHER_IDEAS %in% c("", ".", "N/A", "None", "None at this time"))] <- NA
t1 <- grep("ORGPROG_", names(PER_final), value=T)
table(rowSums(is.na(PER_final[,t1]), na.rm=T))
t2 <- which(rowSums(is.na(PER_final[,t1]), na.rm=T)==1) # 6
PER_final[t2,t1][is.na(PER_final[t2,t1])] <- "Never heard of it"
rm(t2, t1)

##### Construct
## egoistic, altriustic, and biospheric values
summary(PER_final[,grep("VALUES_", names(PER_final), value=T)])
# based on revised New Ecological Paradigm scale (Dunlap, Van Liere, Mertig, & Jones, 2000)
PER_final$VALUES_EGO <- 0
PER_final$VALUES_ALT <- 0
PER_final$VALUES_BIO <- 0
for (i in 1:nrow(PER_final)) {
  PER_final$VALUES_EGO[i] <- mean((as.numeric(PER_final[i,paste0("VALUES_", c(1:3))])-2), na.rm=T)
  PER_final$VALUES_ALT[i] <- mean((as.numeric(PER_final[i,paste0("VALUES_", c(4:6))])-2), na.rm=T)
  PER_final$VALUES_BIO[i] <- mean((as.numeric(PER_final[i,paste0("VALUES_", c(7:9))])-2), na.rm=T)
}; rm(i)
summary(PER_final$VALUES_EGO)
summary(PER_final$VALUES_ALT)
summary(PER_final$VALUES_BIO)
cov2cor(var(PER_final[,c("VALUES_EGO", "VALUES_ALT", "VALUES_BIO")], na.rm=T))
## environmental attitudes
summary(PER_final[,grep("ATT_ENVR_", names(PER_final), value=T)])
# based on revised New Ecological Paradigm scale (Dunlap, Van Liere, Mertig, & Jones, 2000)
PER_final$ATT_ENVR <- 0
for (i in 1:nrow(PER_final)) {
  PER_final$ATT_ENVR[i] <- mean(c((as.numeric(PER_final[i,paste0("ATT_ENVR_", seq(1,15,2))])-3)/2, (as.numeric(PER_final[i,paste0("ATT_ENVR_", seq(2,14,2))])-3)/(-2)), na.rm=T)
}; rm(i)
summary(PER_final$ATT_ENVR)
## techno-environmental value
summary(PER_final$ATT_ENVR_16)
PER_final$ATT_ENVR_TECH <- (as.numeric(PER_final[,"ATT_ENVR_16"])-3)/2
summary(PER_final$ATT_ENVR_TECH)
## health attitudes
summary(PER_final[,grep("ATT_HEALTH_", names(PER_final), value=T)])
# based on "health conciousness" questions in HealthStyles survey (Dutta-Bergman, 2004)
PER_final$ATT_HEALTH <- 0
for (i in 1:nrow(PER_final)) {
  PER_final$ATT_HEALTH[i] <- mean((as.numeric(PER_final[i,paste0("ATT_HEALTH_", c(1:5))])-3)/2, na.rm=T)
}; rm(i)
summary(PER_final$ATT_HEALTH)
## norms for non-auto modes, self on self/others
summary(PER_final[,grep("NORMS_ME_", names(PER_final), value=T)])
PER_final$NORMS_ME_SELF <- 0
PER_final$NORMS_ME_SELF_NO <- ifelse(PER_final$NORMS_ME_1=="No opinion" & PER_final$NORMS_ME_3=="No opinion", T, F)
PER_final$NORMS_ME_OTHERS <- 0
PER_final$NORMS_ME_OTHERS_NO <- ifelse(PER_final$NORMS_ME_2=="No opinion" & PER_final$NORMS_ME_4=="No opinion", T, F)
for (i in 1:nrow(PER_final)) {
  PER_final$NORMS_ME_SELF[i] <- mean(c(ifelse(PER_final$NORMS_ME_1[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_ME_1[i])-3)/(-2)), 
                                       ifelse(PER_final$NORMS_ME_3[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_ME_3[i])-3)/2)), na.rm=T)
  PER_final$NORMS_ME_OTHERS[i] <- mean(c(ifelse(PER_final$NORMS_ME_2[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_ME_2[i])-3)/(-2)), 
                                         ifelse(PER_final$NORMS_ME_4[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_ME_4[i])-3)/2)), na.rm=T)
}; rm(i)
summary(PER_final[,c("NORMS_ME_SELF", "NORMS_ME_SELF_NO", "NORMS_ME_OTHERS", "NORMS_ME_OTHERS_NO")])
## norms for non-auto modes, family/friends on self/others
summary(PER_final[,grep("NORMS_PEER_", names(PER_final), value=T)])
PER_final$NORMS_PEER_SELF <- 0
PER_final$NORMS_PEER_SELF_NO <- ifelse(PER_final$NORMS_PEER_1=="No opinion" & PER_final$NORMS_PEER_3=="No opinion", T, F)
PER_final$NORMS_PEER_OTHERS <- 0
PER_final$NORMS_PEER_OTHERS_NO <- ifelse(PER_final$NORMS_PEER_2=="No opinion" & PER_final$NORMS_PEER_4=="No opinion", T, F)
for (i in 1:nrow(PER_final)) {
  PER_final$NORMS_PEER_SELF[i] <- mean(c(ifelse(PER_final$NORMS_PEER_1[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_PEER_1[i])-3)/(-2)), 
                                         ifelse(PER_final$NORMS_PEER_3[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_PEER_3[i])-3)/2)), na.rm=T)
  PER_final$NORMS_PEER_OTHERS[i] <- mean(c(ifelse(PER_final$NORMS_PEER_2[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_PEER_2[i])-3)/(-2)), 
                                           ifelse(PER_final$NORMS_PEER_4[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_PEER_4[i])-3)/2)), na.rm=T)
}; rm(i)
summary(PER_final[,c("NORMS_PEER_SELF", "NORMS_PEER_SELF_NO", "NORMS_PEER_OTHERS", "NORMS_PEER_OTHERS_NO")])
## norms for non-auto modes, organizations on self/others
summary(PER_final[,grep("NORMS_ORGS_", names(PER_final), value=T)])
PER_final$NORMS_ORGS_SELF <- 0
PER_final$NORMS_ORGS_SELF_NO <- ifelse(PER_final$NORMS_ORGS_1=="No opinion" & PER_final$NORMS_ORGS_3=="No opinion", T, F)
PER_final$NORMS_ORGS_OTHERS <- 0
PER_final$NORMS_ORGS_OTHERS_NO <- ifelse(PER_final$NORMS_ORGS_2=="No opinion" & PER_final$NORMS_ORGS_4=="No opinion", T, F)
for (i in 1:nrow(PER_final)) {
  PER_final$NORMS_ORGS_SELF[i] <- mean(c(ifelse(PER_final$NORMS_ORGS_1[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_ORGS_1[i])-3)/(-2)), 
                                         ifelse(PER_final$NORMS_ORGS_3[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_ORGS_3[i])-3)/2)), na.rm=T)
  PER_final$NORMS_ORGS_OTHERS[i] <- mean(c(ifelse(PER_final$NORMS_ORGS_2[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_ORGS_2[i])-3)/(-2)), 
                                           ifelse(PER_final$NORMS_ORGS_4[i]=="No opinion", 0, (as.numeric(PER_final$NORMS_ORGS_4[i])-3)/2)), na.rm=T)
}; rm(i)
summary(PER_final[,c("NORMS_ORGS_SELF", "NORMS_ORGS_SELF_NO", "NORMS_ORGS_OTHERS", "NORMS_ORGS_OTHERS_NO")])
cov2cor(var(PER_final[,c("NORMS_ME_SELF", "NORMS_ME_OTHERS", "NORMS_PEER_SELF", "NORMS_PEER_OTHERS", "NORMS_ORGS_SELF", "NORMS_ORGS_OTHERS")], na.rm=T))
## self-efficacy for non-auto modes
summary(PER_final[,grep("SELFEFF", names(PER_final), value=T)])
table(rowSums(is.na(PER_final[,grep("SELFEFF_", names(PER_final), value=T)]), na.rm=T))
PER_final$SELFEFF <- 0
for (i in 1:nrow(PER_final)) {
  PER_final$SELFEFF[i] <- mean((as.numeric(PER_final[i,grep("SELFEFF_", names(PER_final), value=T)])-3)/2, na.rm=T)
}; rm(i)
summary(PER_final$SELFEFF)
## awareness of air quality impacts
summary(PER_final[,grep("AQ_IMPACTS_", names(PER_final), value=T)])
## awareness of air quality causes
summary(PER_final[,grep("AQ_CAUSES_", names(PER_final), value=T)])
## air quality attitudes, self
summary(PER_final[,grep("AQ_ME_", names(PER_final), value=T)])
## air quality attitudes, friends/family
summary(PER_final[,grep("AQ_PEER_", names(PER_final), value=T)])
## air quality attitudes, organizations
summary(PER_final[,grep("AQ_ORGS_", names(PER_final), value=T)])
## idling frequency
summary(PER_final[,paste0("IDLING_", c(1:6))])
table(rowSums(is.na(PER_final[,paste0("IDLING_", c(1:6))]), na.rm=T))
PER_final$IDLING <- 0
for (i in 1:nrow(PER_final)) {
  PER_final$IDLING[i] <- mean((as.numeric(PER_final[i,paste0("IDLING_", c(1:6))])-1)/4, na.rm=T)
}; rm(i)
summary(PER_final$IDLING)
## idling knowledge
PER_final$IDLING_LOGAN2 <- PER_final$IDLING_LOGAN
summary(PER_final$IDLING_LOGAN2)
levels(PER_final$IDLING_LOGAN2) <- c("Don't know", "Wrong", "Wrong", "Correct", "Wrong")
PER_final$IDLING_LOGAN2 <- relevel(PER_final$IDLING_LOGAN2, ref="Don't know")
summary(PER_final$IDLING_LOGAN2)
## behavior change, self
summary(PER_final[,grep("BEHCHG_ME", names(PER_final), value=T)])
summary(PER_final[,paste0("BEHCHG_ME_", c(1:7))])
PER_final$BEHCHG_ME <- 0
for (i in paste0("BEHCHG_ME_", c(1:7))) {
  PER_final$BEHCHG_ME <- PER_final$BEHCHG_ME + PER_final[,i]
}; rm(i)
PER_final$BEHCHG_ME_MODE <- (PER_final$BEHCHG_ME_1 + PER_final$BEHCHG_ME_2 + PER_final$BEHCHG_ME_3)>0
PER_final$BEHCHG_ME_TRIP <- (PER_final$BEHCHG_ME_4 + PER_final$BEHCHG_ME_5 + PER_final$BEHCHG_ME_6)>0
PER_final$BEHCHG_ME_IDLE <- PER_final$BEHCHG_ME_7
summary(PER_final[,c("BEHCHG_ME", "BEHCHG_ME_MODE", "BEHCHG_ME_TRIP", "BEHCHG_ME_IDLE")])
cov2cor(var(PER_final[,c("BEHCHG_ME", "BEHCHG_ME_MODE", "BEHCHG_ME_TRIP", "BEHCHG_ME_IDLE")], na.rm=T))
## behavior change, others
summary(PER_final[,grep("BEHCHG_POP_", names(PER_final), value=T)])
summary(PER_final[,paste0("BEHCHG_POP_", c(1:7))])
PER_final$BEHCHG_POP <- 0
PER_final$BEHCHG_POP_MODE <- 0
PER_final$BEHCHG_POP_TRIP <- 0
PER_final$BEHCHG_POP_IDLE <- 0
temp1 <- PER_final[,paste0("BEHCHG_POP_", c(1:7))]
for (i in paste0("BEHCHG_POP_", c(1:7))) {
  levels(temp1[,i]) <- c("0.05", "0.20", "0.35", "0.50", "0.65", "0.80", "0.95")
  temp1[,i] <- as.numeric(as.character(temp1[,i]))
}; rm(i)
summary(temp1)
for (i in 1:nrow(PER_final)) {
  PER_final$BEHCHG_POP[i] <- mean((as.numeric(temp1[i,paste0("BEHCHG_POP_", c(1:7))])), na.rm=T)
  PER_final$BEHCHG_POP_MODE[i] <- mean((as.numeric(temp1[i,paste0("BEHCHG_POP_", c(1:3))])), na.rm=T)
  PER_final$BEHCHG_POP_TRIP[i] <- mean((as.numeric(temp1[i,paste0("BEHCHG_POP_", c(4:6))])), na.rm=T)
  PER_final$BEHCHG_POP_IDLE[i] <- mean((as.numeric(temp1[i,paste0("BEHCHG_POP_", c(7))])), na.rm=T)
}; rm(i)
rm(temp1)
summary(PER_final[,c("BEHCHG_POP", "BEHCHG_POP_MODE", "BEHCHG_POP_TRIP", "BEHCHG_POP_IDLE")])
cov2cor(var(PER_final[,c("BEHCHG_POP", "BEHCHG_POP_MODE", "BEHCHG_POP_TRIP", "BEHCHG_POP_IDLE")], na.rm=T))
## support for AQ strategies
summary(PER_final[,grep("SUPPORT_", names(PER_final), value=T)])
PER_final$SUPPORT <- 0
for (i in 1:nrow(PER_final)) {
  PER_final$SUPPORT[i] <- mean((as.numeric(PER_final[i,grep("SUPPORT_", names(PER_final), value=T)])-2)*(-1), na.rm=T)
}; rm(i)
summary(PER_final$SUPPORT)
## awareness of organizations
summary(PER_final[,grep("ORGPROG", names(PER_final), value=T)])
PER_final$ORGPROG <- 0
for (i in grep("ORGPROG_", names(PER_final), value=T)) {
  PER_final$ORGPROG <- PER_final$ORGPROG + (as.numeric(PER_final[,i])-1)/2
}; rm(i)
## text responses
# PER_final$CHANGES_TEXT[!is.na(PER_final$CHANGES_TEXT)]
# PER_final$OTHER_IDEAS[!is.na(PER_final$OTHER_IDEAS)]
# PER_final$COMMENTS[PER_final$COMMENTS!=""]
# table(PER_final$IDLING_6_TEXT)
# table(PER_final$BEHCHG_ME_8_TEXT)
# table(PER_final$BEHCHG_POP_8_TEXT)

########################################
# Add HH and PER codes

# Load formatted HH and PER files
HH <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "HH.rds"))
PER <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PER.rds"))

# Inspect
str(HH)
str(PER)
str(PER_final)

# Merge
t1 <- merge(PER_final, HH[,c("ResponseId", "HHCODE")], by.x="HHResponseId", by.y="ResponseId", all.x=T, all.y=F)
t2 <- merge(t1, PER[,c("HHCODE", "PERCODE", "NAME")], by.x=c("HHCODE", "PERNAME"), by.y=c("HHCODE", "NAME"), all.x=T, all.y=F)
PER_final <- t2
rm(t1, t2)

# Cleanup
rm(HH, PER)

# Reorganize
mycols <- c("ResponseId", "HHResponseId", "HHCODE", "PERCODE", "PERNAME", "CHANGES_TEXT", 
            grep("VALUES", names(PER_final), value=T), grep("ATT_ENVR", names(PER_final), value=T), grep("ATT_HEALTH", names(PER_final), value=T), 
            grep("NORMS_ME", names(PER_final), value=T), grep("NORMS_PEER", names(PER_final), value=T), grep("NORMS_ORGS", names(PER_final), value=T), 
            grep("SELFEFF", names(PER_final), value=T), grep("AQ_IMPACTS", names(PER_final), value=T), grep("AQ_CAUSES", names(PER_final), value=T), 
            grep("AQ_ME", names(PER_final), value=T), grep("AQ_PEER", names(PER_final), value=T), grep("AQ_ORGS", names(PER_final), value=T), 
            grep("IDLING", names(PER_final), value=T), grep("BEHCHG_ME", names(PER_final), value=T), grep("BEHCHG_POP", names(PER_final), value=T), 
            grep("SUPPORT", names(PER_final), value=T), "OTHER_IDEAS", grep("ORGPROG", names(PER_final), value=T), "COMMENTS")
PER_final <- PER_final[,mycols]
rm(mycols)
PER_final <- PER_final[order(PER_final$HHCODE, PER_final$PERCODE),]
row.names(PER_final) <- NULL

# Inspect
names(PER_final)
str(PER_final, list.len=ncol(PER_final))
summary(PER_final)

########################################
# Save

# Save
t_folder <- "Data 3a Formatted"
saveRDS(meta_final, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_final.rds"))
write.csv(meta_final, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_final.csv"), row.names=F)
saveRDS(PER_final, file=file.path("Data", "Survey 2019 Winter", t_folder, "PER_final.rds"))
write.csv(PER_final, file=file.path("Data", "Survey 2019 Winter", t_folder, "PER_final.csv"), row.names=F)
rm(t_folder)

########################################
# Cleanup

# Remove
rm(final, temp)
rm(meta_final)
rm(PER_final)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################