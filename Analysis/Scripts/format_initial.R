########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     format_initial.R
# About:    Script to format initial survey
########################################

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
# library("")

########################################
# Load data

# Read rds data file
initial <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 2 Cleaned", "initial.rds"))

# Backup
temp <- initial

# Inspect
names(temp)
str(temp)

########################################
# Assemble metadata

# Initialize
mycols <- c("ResponseId", "StartDate", "EndDate", "Status", "IPAddress", 
            "Progress", "Duration", "Finished", "RecordedDate", 
            "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", 
            "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage")
mycols <- c(mycols, c("CONSENT", "CHECKAGE", "CHECKHOME"))
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
meta_initial <- data.frame(temp[,mycols])
rm(mycols)

# Inspect
names(meta_initial)
str(meta_initial)
# summary(meta_initial)

########################################
# Assemble contact data

# Initialize
mycols <- c("ResponseId")
mycols <- c(mycols, c("EMAIL", "ADD_STREET", "ADD_CITY", "ADD_STATE", "ADD_ZIP"))
contact <- data.frame(temp[,mycols])
rm(mycols)

# Inspect
names(contact)
str(contact)
summary(contact)

########################################
# Assemble household data

# Initialize
mycols <- c("ResponseId")
mycols <- c(mycols, c("HLOC_STREET", "HLOC_CITY", "HLOC_ZIP", "HTYPE", "HTYPE_TEXT", "HTENURE", "HLIVED", "HHINC", "HHKIDS"))
mycols <- c(mycols, c("HHBIKES", "HHCARS"))
mycols <- c(mycols, c("COMMENTS"))
HH <- data.frame(temp[,mycols])
rm(mycols)

# Construct
HH$HHCODE <- 1000L + as.integer(row.names(HH))
HH$HTYPE2 <- HH$HTYPE
levels(HH$HTYPE2) <- c(rep("Single-family", 3), rep("Multi-family", 5), NA)
HH$HTYPE2 <- relevel(HH$HTYPE2, ref="Single-family")
HH$HLIVED2 <- HH$HLIVED
levels(HH$HLIVED2) <- c(rep("0 to 5 years",3), rep("6 or more years",2))
HH$HLIVED2 <- relevel(HH$HLIVED2, ref="0 to 5 years")
HH$HHINC2 <- HH$HHINC
levels(HH$HHINC2) <- c(rep("Less than $25,000", 3), rep("$25,000 to $49,999", 2), "$50,000 to $74,999", "$75,000 to $99,999", rep("$100,000 or more", 2), rep("Unknown", 2))
HH$HHINC2 <- relevel(HH$HHINC2, ref="$50,000 to $74,999")
HH$HHINC3 <- HH$HHINC
levels(HH$HHINC3) <- c(rep("Less than $35,000", 4), rep("$35,000 to $74,999", 2), rep("$75,000 or more", 3), rep("Unknown", 2))
HH$HHINC3 <- relevel(HH$HHINC3, ref="$35,000 to $74,999")
HH$HHKIDS2 <- HH$HHKIDS
levels(HH$HHKIDS2) <- c("0", "1", "2", "3", "4", "5")
HH$HHKIDS2 <- as.integer(as.character(HH$HHKIDS2))
HH$HHNPER <- 0L
HH$HHBIKES2 <- HH$HHBIKES
levels(HH$HHBIKES2) <- c("0", "1", "2", "3", "4", "5", "6")
HH$HHBIKES2 <- as.integer(as.character(HH$HHBIKES2))
HH$HHNVEH <- 0L

# Reorganize
mycols <- c("ResponseId", "HHCODE", "HLOC_STREET", "HLOC_CITY", "HLOC_ZIP", 
            "HTYPE", "HTYPE2", "HTYPE_TEXT", "HTENURE", "HLIVED", "HLIVED2", 
            "HHINC", "HHINC2", "HHINC3", "HHKIDS", "HHKIDS2", "HHNPER", 
            "HHBIKES", "HHBIKES2", "HHCARS", "HHNVEH", "COMMENTS")
HH <- HH[,mycols]
rm(mycols)

# Inspect
names(HH)
str(HH)
summary(HH)

########################################
# Assemble person data

# Initialize
mycols <- c("ResponseId")
mycols1 <- c("PNAME", "PAGE", "PRACE_1", "PRACE_2", "PRACE_3", "PRACE_4", "PRACE_5", "PRACE_6", "PRACE_7", "PRACE_7_TEXT", "PRACE_8", 
             "PGEND", "PGEND_TEXT", "PEDUC", "PSTUDENT", "PSCHOOL_NAME", "PSCHOOL_STREET", "PSCHOOL_CITY", 
             "SMODE_1", "SMODE_2", "SMODE_3", "SMODE_4", "SMODE_5", "SMODE_6", "SMODE_7", "SMODE_8", "SMODE_8_TEXT", "SMODE_9", 
             "PWORKER", "PWDAYS_COMMUTE", "PWDAYS_HOME", "PWHOURS", "PWFLEX", "PWORK_NAME", "PWORK_STREET", "PWORK_CITY", 
             "WMODE_1", "WMODE_2", "WMODE_3", "WMODE_4", "WMODE_5", "WMODE_6", "WMODE_7", "WMODE_7_TEXT", "WMODE_8", 
             "PDRVLIC", "PKNOW_BIKE", "PKNOW_AUTO", "PKNOW_TRAN", "PLIMIT_1", "PLIMIT_2", "PLIMIT_3", "PLIMIT_4", 
             "PLIMIT_5", "PLIMIT_6", "PLIMIT_7", "PLIMIT_8", "PLIMIT_9", "PLIMIT_10", "PMORE")
PER <- data.frame(temp[,c(mycols, paste("P1", mycols1, sep="_"))])
names(PER) <- c(mycols, mycols1)
PER$PERCODE <- 1
for (j in 2:10) {
  Xx <- paste0("P", j)
  tPER <- data.frame(temp[,c(mycols, paste(Xx, mycols1, sep="_"))])
  names(tPER) <- c(mycols, mycols1)
  tPER$PERCODE <- j
  PER <- rbind(PER, tPER)
  rm(tPER, Xx)
}; rm(j)
rm(mycols, mycols1)

# Remove empty, fix NAs
PER <- PER[PER$PNAME!="" | !is.na(PER$PAGE) | !is.na(PER$PGEND) | !is.na(PER$PEDUC) | !is.na(PER$PSTUDENT) | PER$PSCHOOL_NAME!="" | PER$PSCHOOL_STREET!="" | PER$PSCHOOL_CITY!="" | !is.na(PER$PWORKER) | !is.na(PER$PWDAYS_COMMUTE) | !is.na(PER$PWDAYS_HOME) | !is.na(PER$PWHOURS) | !is.na(PER$PWFLEX) | PER$PWORK_NAME!="" | PER$PWORK_STREET!="" | PER$PWORK_CITY!="" | !is.na(PER$PDRVLIC) | !is.na(PER$PKNOW_BIKE) | !is.na(PER$PKNOW_AUTO) | !is.na(PER$PKNOW_TRAN) | !is.na(PER$PMORE),]
# - NOTE: code removed for confidentiality
# - example: incomplete, okay: R_0123456789abcde 1
PER[is.na(PER$PGEND),"PGEND"] <- "Prefer not to answer"
# - NOTE: code removed for confidentiality
# - example: PER[PER$ResponseId=="R_0123456789abcde" & PER$PERCODE==1, "PWORKER"] <- "Yes"

# Construct
PER <- merge(PER, HH[,c("ResponseId", "HHCODE")], by="ResponseId", all.x=T, all.y=F)
PER$PAGE2 <- PER$PAGE
levels(PER$PAGE2) <- c(rep("18 to 24 years", 2), "25 to 34 years", "35 to 44 years", "45 to 54 years", "55 to 64 years", rep("65 years and over", 3), NA)
PER$PAGE3 <- PER$PAGE2
levels(PER$PAGE3) <- c(rep("18 to 34 years", 2), rep("35 to 54 years",2), rep("55 years and over", 2))
PER$PAGE2 <- relevel(PER$PAGE2, ref="18 to 24 years")
PER$PAGE3 <- relevel(PER$PAGE3, ref="18 to 34 years")
PER$PRACE1 <- ifelse(PER$PRACE_8==T, NA, 
              ifelse(PER$PRACE_2==T | PER$PRACE_3==T | PER$PRACE_4==T | PER$PRACE_5==T | PER$PRACE_6==T | PER$PRACE_7==T, "Non-white/Multiple", 
              ifelse(PER$PRACE_1==T, "White-alone", NA)))
PER$PRACE1 <- factor(PER$PRACE1, levels=c("White-alone", "Non-white/Multiple"))
PER$PGEND2 <- PER$PGEND
levels(PER$PGEND2) <- c("Female", "Male", NA, NA)
PER$PGEND2 <- relevel(PER$PGEND2, ref="Male")
PER$PEDUC2 <- PER$PEDUC
levels(PER$PEDUC2) <- c(rep("High school or less", 2), rep("Some college/Associate",2), "Bachelor degree", rep("Higher than bachelor",3), NA)
PER$PEDUC3 <- PER$PEDUC2
levels(PER$PEDUC3) <- c(rep("Less than bachelor", 2), rep("Bachelor or higher", 2))
PER$PEDUC2 <- relevel(PER$PEDUC2, ref="Bachelor degree")
PER$PEDUC3 <- relevel(PER$PEDUC3, ref="Bachelor or higher")
PER$PSTUDENT2 <- PER$PSTUDENT
levels(PER$PSTUDENT2) <- c(rep("Yes", 2), "No")
PER$PSTUDENT2 <- relevel(PER$PSTUDENT2, ref="No")
PER$SMODE_WABI <- ifelse(PER$SMODE_1==T | PER$SMODE_2==T, T, F)
PER$SMODE_AUTO <- ifelse(PER$SMODE_3==T | PER$SMODE_4==T | PER$SMODE_5==T, T, F)
PER$SMODE_TRAN <- ifelse(PER$SMODE_6==T | PER$SMODE_7==T, T, F)
PER$SMODE_HOME <- ifelse(PER$SMODE_9==T, T, F)
PER$SMODE_WABITRAN <- ifelse(PER$SMODE_WABI==T | PER$SMODE_TRAN==T, T, F)
PER$SMODE1 <- ifelse(PER$SMODE_HOME==T, "No travel", ifelse(PER$SMODE_WABITRAN==T & PER$SMODE_AUTO==T, "Multimodal", 
              ifelse(PER$SMODE_WABITRAN==T, "Walk/Bike/Bus", ifelse(PER$SMODE_AUTO==T, "Auto only", NA))))
PER$SMODE1 <- factor(PER$SMODE1, levels=c("Auto only", "Multimodal", "Walk/Bike/Bus", "No travel"))
PER$PWDAYS_COMMUTE2 <- ifelse(PER$PWDAYS_COMMUTE<5, "0 to 4", 
                       ifelse(PER$PWDAYS_COMMUTE==5, "5", "6 to 7"))
PER$PWDAYS_COMMUTE2 <- factor(PER$PWDAYS_COMMUTE2, levels=c("5", "0 to 4", "6 to 7"))
PER$PWDAYS_HOME2 <- ifelse(PER$PWDAYS_HOME>0, "Yes", "No")
PER$PWDAYS_HOME2 <- factor(PER$PWDAYS_HOME2, levels=c("No", "Yes"))
PER$PWHOURS2 <- ifelse(PER$PWHOURS<20, "1 to 19", 
                ifelse(PER$PWHOURS<40, "20 to 39", 
                ifelse(PER$PWHOURS==40, "40", "41 or more")))
PER$PWHOURS2 <- factor(PER$PWHOURS2, levels=c("40", "1 to 19", "20 to 39", "41 or more"))
PER$PWFLEX2 <- PER$PWFLEX
levels(PER$PWFLEX2) <- c(rep("Flexible", 2), rep("Neither/Inflexible", 3))
PER$PWFLEX2 <- relevel(PER$PWFLEX2, ref="Neither/Inflexible")
PER$PWFLEX3 <- PER$PWFLEX
levels(PER$PWFLEX3) <- c(rep("Neither/Flexible", 3), rep("Inflexible", 2))
PER$PWFLEX3 <- relevel(PER$PWFLEX3, ref="Neither/Flexible")
PER$WMODE_WABI <- ifelse(PER$WMODE_1==T | PER$WMODE_2==T, T, F)
PER$WMODE_AUTO <- ifelse(PER$WMODE_3==T | PER$WMODE_4==T | PER$WMODE_5==T, T, F)
PER$WMODE_TRAN <- ifelse(PER$WMODE_6==T, T, F)
PER$WMODE_HOME <- ifelse(PER$WMODE_8==T, T, F)
PER$WMODE_WABITRAN <- ifelse(PER$WMODE_WABI==T | PER$WMODE_TRAN==T, T, F)
PER$WMODE1 <- ifelse(PER$WMODE_HOME==T, "No travel", ifelse(PER$WMODE_WABITRAN==T & PER$WMODE_AUTO==T, "Multimodal", 
              ifelse(PER$WMODE_WABITRAN==T, "Walk/Bike/Bus", ifelse(PER$WMODE_AUTO==T, "Auto only", NA))))
PER$WMODE1 <- factor(PER$WMODE1, levels=c("Auto only", "Multimodal", "Walk/Bike/Bus", "No travel"))
PER$PKNOW_BIKE2 <- PER$PKNOW_BIKE
levels(PER$PKNOW_BIKE2) <- c("Yes", rep("Not well/No",2))
PER$PKNOW_BIKE2 <- relevel(PER$PKNOW_BIKE2, ref="Yes")
PER$PKNOW_AUTO2 <- PER$PKNOW_AUTO
levels(PER$PKNOW_AUTO2) <- c("Yes", rep("Not well/No",2))
PER$PKNOW_AUTO2 <- relevel(PER$PKNOW_AUTO2, ref="Yes")
PER$PKNOW_TRAN2 <- PER$PKNOW_TRAN
levels(PER$PKNOW_TRAN2) <- c("Yes", rep("Not well/No",2))
PER$PKNOW_TRAN2 <- relevel(PER$PKNOW_TRAN2, ref="Yes")
PER$PLIMIT1 <- ifelse(PER$PLIMIT_1==T | PER$PLIMIT_2==T | PER$PLIMIT_3==T | PER$PLIMIT_4==T | PER$PLIMIT_5==T | PER$PLIMIT_6==T | PER$PLIMIT_7==T | PER$PLIMIT_8==T | PER$PLIMIT_9==T, 
                      "Yes", ifelse(PER$PLIMIT_10==T, "No", NA))
PER$PLIMIT1 <- factor(PER$PLIMIT1, levels=c("No", "Yes"))
PER$PERCODE <- as.integer(PER$PERCODE)

# Reorganize
mycols <- c("ResponseId", "HHCODE", "PERCODE", "PNAME", "PAGE", "PAGE2", "PAGE3", 
            "PRACE_1", "PRACE_2", "PRACE_3", "PRACE_4", "PRACE_5", "PRACE_6", "PRACE_7", "PRACE_7_TEXT", "PRACE_8", "PRACE1", 
            "PGEND", "PGEND2", "PGEND_TEXT", "PEDUC", "PEDUC2", "PEDUC3", "PSTUDENT", "PSTUDENT2", 
            "PSCHOOL_NAME", "PSCHOOL_STREET", "PSCHOOL_CITY", 
            "SMODE_1", "SMODE_2", "SMODE_3", "SMODE_4", "SMODE_5", "SMODE_6", "SMODE_7", "SMODE_8", "SMODE_8_TEXT", "SMODE_9", 
            "SMODE_WABI", "SMODE_AUTO", "SMODE_TRAN", "SMODE_HOME", "SMODE_WABITRAN", "SMODE1", 
            "PWORKER", "PWDAYS_COMMUTE", "PWDAYS_COMMUTE2", "PWDAYS_HOME", "PWDAYS_HOME2", "PWHOURS", "PWHOURS2", 
            "PWFLEX", "PWFLEX2", "PWFLEX3", "PWORK_NAME", "PWORK_STREET", "PWORK_CITY", 
            "WMODE_1", "WMODE_2", "WMODE_3", "WMODE_4", "WMODE_5", "WMODE_6", "WMODE_7", "WMODE_7_TEXT", "WMODE_8", 
            "WMODE_WABI", "WMODE_AUTO", "WMODE_TRAN", "WMODE_HOME", "WMODE_WABITRAN", "WMODE1", 
            "PDRVLIC", "PKNOW_BIKE", "PKNOW_BIKE2", "PKNOW_AUTO", "PKNOW_AUTO2", "PKNOW_TRAN", "PKNOW_TRAN2", 
            "PLIMIT_1", "PLIMIT_2", "PLIMIT_3", "PLIMIT_4", "PLIMIT_5", "PLIMIT_6", "PLIMIT_7", "PLIMIT_8", "PLIMIT_9", "PLIMIT_10", 
            "PLIMIT1", "PMORE")
PER <- PER[,mycols]
rm(mycols)
PER <- PER[order(PER$HHCODE, PER$PERCODE),]
row.names(PER) <- NULL
names(PER)[4:(which(names(PER)=="SMODE_1")-1)] <- substring(names(PER)[4:(which(names(PER)=="SMODE_1")-1)], 2)
names(PER)[(which(names(PER)=="SMODE1")+1):(which(names(PER)=="WMODE_1")-1)] <- substring(names(PER)[(which(names(PER)=="SMODE1")+1):(which(names(PER)=="WMODE_1")-1)], 2)
names(PER)[(which(names(PER)=="WMODE1")+1):ncol(PER)] <- substring(names(PER)[(which(names(PER)=="WMODE1")+1):ncol(PER)], 2)

# Inspect
names(PER)
str(PER)
summary(PER)

########################################
# Assemble vehicle data

# Initialize
mycols <- c("ResponseId")
mycols1 <- c("VTYPE", "VTYPE_TEXT", "VPRIMARY", "VYEAR", "VMAKE", "VMODEL", "VMORE")
VEH <- data.frame(temp[,c(mycols, paste("V1", mycols1, sep="_"))])
names(VEH) <- c(mycols, mycols1)
VEH$VEHCODE <- 1
for (j in 2:10) {
  Xx <- paste0("V", j)
  tVEH <- data.frame(temp[,c(mycols, paste(Xx, mycols1, sep="_"))])
  names(tVEH) <- c(mycols, mycols1)
  tVEH$VEHCODE <- j
  VEH <- rbind(VEH, tVEH)
  rm(tVEH, Xx)
}; rm(j)
rm(mycols, mycols1)

# Remove empty, fix NAs
VEH <- VEH[!is.na(VEH$VTYPE) | !is.na(VEH$VPRIMARY) | !is.na(VEH$VYEAR) | VEH$VMAKE!="" | VEH$VMODEL!="" | !is.na(VEH$VMORE),]
# - NOTE: code removed for confidentiality
# - example: VEH[VEH$ResponseId=="R_0123456789abcde" & is.na(VEH$VPRIMARY),"VPRIMARY"] <- "Secondary household vehicle"
# - example: VEH[VEH$ResponseId=="R_0123456789abcde" & is.na(VEH$VMORE),"VMORE"] <- "No"

# Construct
VEH <- merge(VEH, HH[,c("ResponseId", "HHCODE")], by="ResponseId", all.x=T, all.y=F)
VEH$VTYPE2 <- VEH$VTYPE
levels(VEH$VTYPE2) <- c("Car/Van/Truck/SUV", "Motorcycle/Scooter/Moped", NA)
VEH$VTYPE2 <- relevel(VEH$VTYPE2, ref="Car/Van/Truck/SUV")
VEH$VYEAR <- as.integer(VEH$VYEAR)
VEH$VAGE <- 2019L - VEH$VYEAR
VEH$VEHCODE <- as.integer(VEH$VEHCODE)

# Fix VMAKE
VEH$VMAKE <- trimws(VEH$VMAKE)
VEH$VMAKE <- tools::toTitleCase(VEH$VMAKE)
# - NOTE: code removed for confidentiality
# - example: VEH[VEH$VMAKE %in% c("Chevy","Cheverolet"), c("VMAKE")] <- list("Chevrolet")
# inspect
# sort(table(VEH$VMAKE), decreasing=T)
# sort(unique(VEH$VMAKE))
# VEH[VEH$VMAKE=="",]

# Fix VMODEL
VEH$VMODEL <- trimws(VEH$VMODEL)
VEH$VMODEL <- tools::toTitleCase(VEH$VMODEL)
# - NOTE: code removed for confidentiality
# - example: VEH[VEH$VMAKE=="Chevrolet" & VEH$VMODEL=="Silerado", c("VMODEL")] <- list("Silverado")
# inspect
sort(table(VEH$VMODEL), decreasing=T)
sort(unique(VEH$VMODEL))
# for (i in sort(unique(VEH$VMAKE))) {
#   print(i); print("")
#   tv <- VEH[VEH$VMAKE==i,]
#   print(sort(table(tv$VMODEL), decreasing=T))
#   print("")
#   print(sort(unique(tv$VMODEL)))
#   readline("Press [enter] to continue")
#   rm(tv)
# }; rm(i)

# Reorganize
mycols <- c("ResponseId", "HHCODE", "VEHCODE", "VTYPE", "VTYPE2", "VTYPE_TEXT", 
            "VPRIMARY", "VYEAR", "VAGE", "VMAKE", "VMODEL", "VMORE")
VEH <- VEH[,mycols]
rm(mycols)
VEH <- VEH[order(VEH$HHCODE, VEH$VEHCODE),]
row.names(VEH) <- NULL
names(VEH)[4:ncol(VEH)] <- substring(names(VEH)[4:ncol(VEH)], 2)

# Inspect
names(VEH)
str(VEH)
summary(VEH)

########################################
# Add missing person to PER file

# Load final survey data
final <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 2 Cleaned", "final.rds"))

# Compare HH and PER
tfinal <- final
tPER <- PER
tfinal$HHPER <- paste(tfinal$HHResponseId, tfinal$PERNAME, sep=" ")
tPER$HHPER <- paste(tPER$ResponseId, tPER$NAME, sep=" ")
table(tfinal$HHPER %in% tPER$HHPER) # 9
tempa <- tfinal[which(!(tfinal$HHPER %in% tPER$HHPER)),] # blanks/incompletes, one person not in PER
# - NOTE: code removed for confidentiality
# - example: HHResponseId = "R_0123456789abcde", PERNAME = "Patrick"
rm(tfinal, tPER, tempa, final)

# Add missing person to PER
# - NOTE: code removed for confidentiality
# - example: PER[PER$ResponseId == "R_0123456789abcde",] 
# - example: # make PERCODE 1
# - example: PER <- rbind(PER, rep(NA, ncol(PER)))
# - example: PER[nrow(PER), c("ResponseId", "HHCODE", "PERCODE", "NAME", "GEND", "GEND2")] <- list("R_0123456789abcde", 1001L, 1L, "Patrick", "Male", "Male")
# - example: PER <- PER[order(PER$HHCODE, PER$PERCODE),]
# - example: row.names(PER) <- NULL
# - example: PER[PER$ResponseId == "R_0123456789abcde",] 

########################################
# Work on codes and HH summaries

# Add info to HH about NPER and NVEH
tPER <- data.frame(table(PER$ResponseId))
tVEH <- data.frame(table(VEH$ResponseId))
ttPER <- merge(HH[,c("ResponseId", "HHCODE")], tPER, by.x="ResponseId", by.y="Var1", all=T)
ttVEH <- merge(HH[,c("ResponseId", "HHCODE")], tVEH, by.x="ResponseId", by.y="Var1", all=T)
ttPER <- ttPER[order(ttPER$HHCODE),]
ttVEH <- ttVEH[order(ttVEH$HHCODE),]
summary(order(ttPER$HHCODE)==order(HH$HHCODE))
summary(order(ttVEH$HHCODE)==order(HH$HHCODE))
ttPER$Freq[is.na(ttPER$Freq)] <- 0
ttVEH$Freq[is.na(ttVEH$Freq)] <- 0
HH$HHNPER <- as.integer(ttPER$Freq)
HH$HHNVEH <- as.integer(ttVEH$Freq)
rm(tPER, tVEH, ttPER, ttVEH)

########################################
# Save

# Save
t_folder <- "Data 3a Formatted"
saveRDS(meta_initial, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_initial.rds"))
write.csv(meta_initial, file=file.path("Data", "Survey 2019 Winter", t_folder, "meta_initial.csv"), row.names=F)
saveRDS(contact, file=file.path("Data", "Survey 2019 Winter", t_folder, "contact.rds"))
write.csv(contact, file=file.path("Data", "Survey 2019 Winter", t_folder, "contact.csv"), row.names=F)
saveRDS(HH, file=file.path("Data", "Survey 2019 Winter", t_folder, "HH.rds"))
write.csv(HH, file=file.path("Data", "Survey 2019 Winter", t_folder, "HH.csv"), row.names=F)
saveRDS(PER, file=file.path("Data", "Survey 2019 Winter", t_folder, "PER.rds"))
write.csv(PER, file=file.path("Data", "Survey 2019 Winter", t_folder, "PER.csv"), row.names=F)
saveRDS(VEH, file=file.path("Data", "Survey 2019 Winter", t_folder, "VEH.rds"))
write.csv(VEH, file=file.path("Data", "Survey 2019 Winter", t_folder, "VEH.csv"), row.names=F)
rm(t_folder)

########################################
# Cleanup

# Remove
rm(initial, temp)
rm(meta_initial, contact)
rm(HH, PER, VEH)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################