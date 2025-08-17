########################################
# Project:  MPC-559 Travel behavior air quality
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
#           Fariba Soltani (fariba.soltani@usu.edu)
#           Mahyar Vahedi Saheli (mahyar.vahedi@usu.edu)
# File:     travel-behavior.R
# About:    Regression models of travel behavior and activity participation
########################################

########################################
# Notes

# Open R project first, then open this R script

# Major edits
# 2023-03-07 PS created based on script by FS
# 2023-07-11 PS updated
# 2023-07-20 PS updated
# 2023-08-11 PS & FS updated: add interaction between AQ and n'hood type
# 2023-08-22 FS updated
# 2023-10-01 PS updated
# 2025-02-19 PS/FS updated: add weighting
# 2025-03-09 PS/FS updated
# 2025-06-03 PS updated: tried panel models
# 2025-06-22 PS updated: added weather variables
# 2025-08-17 PS update: cleaned up script

# Load packages
library("readxl")
library("sf")
library("mapview")
library("survey")
library("anesrake")
library("fastDummies")
library("pglm")

########################################
# Load data

# Load data
HH <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "HH.rds"))
PER <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "PER.rds"))
VEH <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "VEH.rds"))
DIARY <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "DIARY.rds"))
PLACE <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "PLACE.rds"))
TRIP <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "TRIP.rds"))
PER_final <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "PER_final.rds"))
AQ <- read.csv(file.path("Data", "Other", "AQ_2019_Cache.csv"))
BG1 <- read_xlsx(file.path("Data", "Other", "mybg_new.xlsx"), 1)
BG2 <- read_xlsx(file.path("Data", "Other", "block_groups.xlsx"), 1)
CBGUT <- st_read(file.path("Data", "Other", "tl_2019_49_bg", "tl_2019_49_bg.shp"))
CBGID <- st_read(file.path("Data", "Other", "tl_2019_16_bg", "tl_2019_16_bg.shp"))

# Remove rows
table(DIARY$REMOVE)
trid <- DIARY$ResponseId[DIARY$REMOVE!=""]
diary <- DIARY[!(DIARY$ResponseId %in% trid),]
place <- PLACE[!(PLACE$ResponseId %in% trid),]
trip <- TRIP[!(TRIP$ResponseId %in% trid),]
rm(trid)

########################################
# Select dependent variables

# Categorize places by activity
summary(place$ACTIVITY)
# 1. Mandatory
act1 <-  c("Work activities", 
           "School activities", 
           "Work- or school-related activities")
place$ACT_MAND <- place$ACTIVITY %in% act1
# 2. Middle category (semi-mandatory, semi-discretionary)
act2 <- c("Civic or religious activities", 
          "Drop off or pick up passenger(s)", 
          "Other errands or appointments (bank, professional office, doctor/dentist, etc.)", 
          "Service private vehicle (gas, oil, repairs, etc.)")
place$ACT_SEMI <- place$ACTIVITY %in% act2
# 3. Discretionary
act3 <- c("Eat meal at restaurant", 
          "Social or entertainment activities (friends/relatives, movie, etc.)", 
          "Outdoor or indoor exercise (sports, jogging, bicycling, walking dog, gym, etc.)", 
          "Shopping (groceries, clothing, convenience store, etc)")
place$ACT_DISC <- place$ACTIVITY %in% act3
# aggregate
temp1 <- aggregate(cbind(ACT_MAND, ACT_SEMI, ACT_DISC) ~ HHCODE + PERCODE + DATE, data=place, FUN=sum)
temp1$ACT_TOT <- temp1$ACT_MAND + temp1$ACT_SEMI + temp1$ACT_DISC
# remove
rm(act1, act2, act3)

# Categorize trips by mode
summary(trip$MODE)
# 1. Active
mode1 <- c("Walk", "Bicycle")
trip$MODE_ACT <- trip$MODE %in% mode1
# 2. Public
mode2 <- c("School Bus", "Local Bus (CVTD or Aggie Shuttle)")
trip$MODE_PUB <- trip$MODE %in% mode2
# 3. Private
mode3 <- c("Car/Van/Truck/SUV Driver", "Car/Van/Truck/SUV Passenger", "Motorcycle/Scooter/Moped")
trip$MODE_PRI <- trip$MODE %in% mode3
# aggregate
temp2 <- aggregate(cbind(MODE_ACT, MODE_PUB, MODE_PRI) ~ HHCODE + PERCODE + DATE, data=trip, FUN=sum)
temp2$MODE_TOT <- temp2$MODE_ACT + temp2$MODE_PUB + temp2$MODE_PRI
# remove
rm(mode1, mode2, mode3)

# Categorize travel distance by mode
# travel distance
trip$DIST_ACT <- trip$DIST * trip$MODE_ACT
trip$DIST_PUB <- trip$DIST * trip$MODE_PUB
trip$DIST_PRI <- trip$DIST * trip$MODE_PRI
# aggregate
temp3 <- aggregate(cbind(DIST_ACT, DIST_PUB, DIST_PRI) ~ HHCODE + PERCODE + DATE, data=trip, FUN=sum)
temp3$DIST_TOT <- temp3$DIST_ACT + temp3$DIST_PUB + temp3$DIST_PRI

# Categorize travel time by mode
# travel time
trip$TIME_ACT <- trip$TRIP_TIME * trip$MODE_ACT
trip$TIME_PUB <- trip$TRIP_TIME * trip$MODE_PUB
trip$TIME_PRI <- trip$TRIP_TIME * trip$MODE_PRI
# aggregate
temp4 <- aggregate(cbind(TIME_ACT, TIME_PUB, TIME_PRI) ~ HHCODE + PERCODE + DATE, data=trip, FUN=sum)
temp4$TIME_TOT <- temp4$TIME_ACT + temp4$TIME_PUB + temp4$TIME_PRI

# Leave home or not
temp5 <- aggregate(cbind(I(LEAVE=="No" & PLANO==1)) ~ HHCODE + PERCODE + DATE, data=place, FUN=sum)
names(temp5)[which(names(temp5)=="V1")] <- "STAYHOME"
temp5$STAYHOME <- as.logical(temp5$STAYHOME)

# Merge aggregations
mydf <- merge(temp1, temp2, by=c("HHCODE", "PERCODE", "DATE"), all=T)
mydf <- merge(mydf, temp3, by=c("HHCODE", "PERCODE", "DATE"), all=T)
mydf <- merge(mydf, temp4, by=c("HHCODE", "PERCODE", "DATE"), all=T)
mydf <- merge(mydf, temp5, by=c("HHCODE", "PERCODE", "DATE"), all=T)
rm(temp1, temp2, temp3, temp4, temp5)

# Inspect
table(mydf$STAYHOME, mydf$ACT_TOT==0)
table(mydf$STAYHOME, is.na(mydf$MODE_TOT))
table(mydf$STAYHOME, is.na(mydf$DIST_TOT))
table(mydf$STAYHOME, is.na(mydf$TIME_TOT))
mydf[mydf$STAYHOME==T,c("ACT_TOT", "ACT_MAND", "ACT_SEMI", "ACT_DISC")] <- NA
table(mydf$STAYHOME, is.na(mydf$ACT_TOT))

# Fix NAs
mydf$num_nas <- rowSums(is.na(mydf))
table(mydf$num_nas)
table(mydf$STAYHOME, mydf$num_nas)
# mydf[mydf$num_nas==4,]  #  4 = distance error (remove)
# mydf[mydf$num_nas==16,] # 16 = no travel (replace with 0)
# for (i in 1:nrow(mydf)) {
#   if (mydf[i,"num_nas"] == 16) {
#     mydf[i,][is.na(mydf[i,])] <- 0L
#   }
# }; rm(i)
mydf <- mydf[mydf$num_nas!=4,]
row.names(mydf) <- NULL
mydf$num_nas <- NULL

# Inspect, adjust for modes
# no mode use
mydf$NOMO_ACT <- mydf$MODE_ACT==0
mydf$NOMO_PUB <- mydf$MODE_PUB==0
mydf$NOMO_PRI <- mydf$MODE_PRI==0
# check
table(mydf$NOMO_ACT, mydf$DIST_ACT==0)
table(mydf$NOMO_ACT, mydf$TIME_ACT==0)
table(mydf$NOMO_PUB, mydf$DIST_PUB==0)
table(mydf$NOMO_PUB, mydf$TIME_PUB==0)
table(mydf$NOMO_PRI, mydf$DIST_PRI==0)
table(mydf$NOMO_PRI, mydf$TIME_PRI==0)
# fix NAs
mydf[!is.na(mydf$NOMO_ACT) & mydf$NOMO_ACT==T, c("MODE_ACT", "DIST_ACT", "TIME_ACT")] <- NA
mydf[!is.na(mydf$NOMO_PUB) & mydf$NOMO_PUB==T, c("MODE_PUB", "DIST_PUB", "TIME_PUB")] <- NA
mydf[!is.na(mydf$NOMO_PRI) & mydf$NOMO_PRI==T, c("MODE_PRI", "DIST_PRI", "TIME_PRI")] <- NA
# check
table(mydf$NOMO_ACT, is.na(mydf$DIST_ACT))
table(mydf$NOMO_ACT, is.na(mydf$TIME_ACT))
table(mydf$NOMO_PUB, is.na(mydf$DIST_PUB))
table(mydf$NOMO_PUB, is.na(mydf$TIME_PUB))
table(mydf$NOMO_PRI, is.na(mydf$DIST_PRI))
table(mydf$NOMO_PRI, is.na(mydf$TIME_PRI))

# Inspect DVs
mydvs <- c("ACT_TOT",  "ACT_MAND", "ACT_SEMI", "ACT_DISC", 
           "MODE_TOT", "MODE_ACT", "MODE_PUB", "MODE_PRI", 
           "DIST_TOT", "DIST_ACT", "DIST_PUB", "DIST_PRI", 
           "TIME_TOT", "TIME_ACT", "TIME_PUB", "TIME_PRI", 
           "STAYHOME", "NOMO_ACT", "NOMO_PUB", "NOMO_PRI")
summary(mydf[,mydvs])

########################################
# Select independent variables

# Inspect
str(HH, list.len=ncol(HH))
str(PER, list.len=ncol(PER))
str(PER_final, list.len=ncol(PER_final))
str(diary, list.len=ncol(diary))

# Merge with HH, PER, PER_final, DIARY information
hh <- HH[,c("HHCODE", "HTYPE2", "HTENURE", "HHINC3", "HHKIDS2", "HHNPER", "HHBIKES2", "HHNVEH", "HLOC_GEOID")]
per <- PER[,c("HHCODE", "PERCODE", "AGE3", "RACE1", "GEND2", "EDUC3", "STUDENT2", "WORKER", "DRVLIC")]
perf <- PER_final[,c("HHCODE", "PERCODE")] # nothing for now
diary2 <- diary[,c("HHCODE", "PERCODE", "DATE", "RATE_TRAFFIC", "RATE_WEATHER", "RATE_AIRQUAL")]
# format
names(hh)[which(names(hh)=="HLOC_GEOID")] <- "GEOID10"
hh <- hh[!duplicated(hh),]
per <- per[!duplicated(per),]
perf <- perf[!duplicated(perf),]
diary2 <- diary2[!duplicated(diary2),]
# merge
mydf <- merge(mydf, hh, by=c("HHCODE"), all.x=T, all.y=F)
mydf <- merge(mydf, per, by=c("HHCODE", "PERCODE"), all.x=T, all.y=F)
mydf <- merge(mydf, perf, by=c("HHCODE", "PERCODE"), all.x=T, all.y=F)
mydf <- merge(mydf, diary2, by=c("HHCODE", "PERCODE", "DATE"), all.x=T, all.y=F)
# remove
rm(hh, per, perf, diary2)

# Merge with air quality
# average AQI because multiple observations each day
AQdf <- aggregate(DAILY_AQI_VALUE ~ Date, FUN=mean, data=AQ)
# format
names(AQdf) <- c("DATE", "AQI")
AQdf$DATE <- as.Date(AQdf$DATE, format="%m/%d/%Y", tz="America/Denver")
AQdf$AQICAT <- cut(AQdf$AQI, breaks=c(0,50,100,150))
levels(AQdf$AQICAT) <- c("Green", "Yellow", "Orange")
summary(AQdf)
# merge
mydf <- merge(mydf, AQdf, by=c("DATE"), all.x=T, all.y=F)
# remove
rm(AQdf)

# Merge with neighborhood type
# format BG
BG <- BG2[1:18,c("BG1", "NTYPE")]
BG$GEOID10 <- as.character(BG$BG1)
BG$BG1 <- NULL
# inspect
table(mydf$GEOID10)
table(BG$NTYPE)
sort(unique(mydf$GEOID10))[sort(unique(mydf$GEOID10)) %in% BG$GEOID10]
sort(unique(mydf$GEOID10))[!(sort(unique(mydf$GEOID10)) %in% BG$GEOID10)]
BG$GEOID10[BG$GEOID10 %in% sort(unique(mydf$GEOID10))]
BG$GEOID10[!(BG$GEOID10 %in% sort(unique(mydf$GEOID10)))]
# combine
temp <- as.data.frame(table(mydf$GEOID10))
names(temp) <- c("GEOID10", "Freq")
tbg <- merge(BG, temp, by="GEOID10", all.x=T, all.y=T)
tbg <- merge(tbg, BG1[,c("GEOID10", "NTYPE2", "NTYPE3")], all.x=T, all.y=F)
rm(temp)
# format CBG
CBG <- rbind(CBGUT, CBGID)
CBG <- CBG[CBG$GEOID %in% tbg$GEOID10,]
cbg <- merge(CBG, tbg, by.x="GEOID", by.y="GEOID10")
# maps
mapview(cbg, zcol="NTYPE2")
mapview(cbg, zcol="NTYPE3")
# merge
ttbg <- st_drop_geometry(cbg[,c("GEOID", "NTYPE2", "NTYPE3")])
ttbg$NTYPE2 <- factor(ttbg$NTYPE2, levels=c("URBAN0", "SUBRUR"))
ttbg$NTYPE3 <- factor(ttbg$NTYPE3, levels=c("URBAN1", "URBAN2", "SUBRUR"))
mydf <- merge(mydf, ttbg, by.x=c("GEOID10"), by.y=c("GEOID"), all.x=T, all.y=F)
# map as figure
cbg1 <- cbg[!is.na(cbg$Freq),]
cbg2 <- cbg[cbg$NTYPE2=="URBAN0",]
# mapview(cbg1, zcol="Freq", color=NULL) + 
#   mapview(cbg2, col.regions=NULL, color="black")
# mapview(cbg1, zcol="Freq", color="black", cex="URBAN0")
cbg1$NT <- factor(cbg1$NTYPE2, levels=c("SUBRUR","URBAN0"), labels=c("Suburban/Rural","Urban"))
mapview(cbg1, zcol="NT", layer.name="Neighborhood Type")
rm(cbg1, cbg2)
# remove
rm(BG, tbg, CBG, cbg, ttbg)

# Reorganize mydf
mydf <- mydf[,c("HHCODE", "PERCODE", "DATE", "GEOID10", names(mydf)[5:ncol(mydf)])]
mydf <- mydf[order(mydf$HHCODE, mydf$PERCODE, mydf$DATE),]
rownames(mydf) <- NULL

# Inspect IVs
mydf$RATE_AIRQUAL2 <- as.integer(mydf$RATE_AIRQUAL)
myivs <- c("HTYPE2", "HTENURE", "HHINC3", "HHKIDS2", "HHNPER", "HHBIKES2", "HHNVEH", 
           "AGE3", "RACE1", "GEND2", "EDUC3", "STUDENT2", "WORKER", "DRVLIC", 
           "RATE_AIRQUAL", "RATE_AIRQUAL2", "AQI", "AQICAT", "NTYPE2", "NTYPE3")
summary(mydf[,myivs])

# Remove
rm(HH, PER, VEH, DIARY, PLACE, TRIP, PER_final)
rm(AQ, BG1, BG2, CBGUT, CBGID)
rm(diary, place, trip, myivs)

########################################
# Model preparation

# Select IVs
# - removed HTENURE b/c correlated (+0.78) w/ HTYPE2
# - removed RATE_AIRQUAL b/c not focus of this study
# - removed AQICAT b/c using AQI instead
# - removed NTYPE3 b/c using NTYPE2 instead
myivs2 <- c("HTYPE2", "HHINC3", "HHKIDS2", "HHNPER", "HHBIKES2", "HHNVEH", 
            "AGE3", "RACE1", "GEND2", "EDUC3", "STUDENT2", "WORKER", # "DRVLIC", 
            "AQI", "AQICAT", "RATE_AIRQUAL", "NTYPE2")

# Remove cases with missing observations on DVs or IVs
# mydf2 <- mydf[,c(mydvs, myivs2)]
mydf2 <- mydf[,c("DATE", "HHCODE", "PERCODE", mydvs, myivs2)]
mydf2 <- mydf2[complete.cases(mydf2[,myivs2]),]
mydf2$ID <- paste(mydf2$HHCODE, mydf2$PERCODE, sep="_")

# Remove
rm(mydvs, myivs2)

########################################
# Extract data for weighting

# Extract household data
mydfhh <- mydf2[,c("HHCODE", "HTYPE2", "HHINC3", "HHKIDS2", "HHNPER", "HHBIKES2", "HHNVEH", "NTYPE2")]
mydfhh <- mydfhh[!duplicated(mydfhh),]

# Extract person data
mydfper <- mydf2[,c("HHCODE", "PERCODE", "AGE3", "RACE1", "GEND2", "EDUC3", "STUDENT2", "WORKER", "ID")]
mydfper <- mydfper[!duplicated(mydfper),]

# Extract household-person data
mydfhhper <- mydf2[,c("HHCODE", "PERCODE", "HTYPE2", "HHINC3", "HHKIDS2", "HHNPER", "HHBIKES2", "HHNVEH", 
                      "AGE3", "RACE1", "GEND2", "EDUC3", "STUDENT2", "WORKER", "NTYPE2", "ID")]
mydfhhper <- mydfhhper[!duplicated(mydfhhper),]

########################################
# Weighting

# Check levels of weighting variables
levels(mydf2$EDUC3)
levels(mydf2$NTYPE2)
myip <- list(EDUC3=c("Bachelor or higher"=0.303, "Less than bachelor"=0.697), 
             NTYPE2=c("URBAN0"=.400, "SUBRUR"=.600))
levels(mydf2$HTYPE2)
levels(mydf2$HHINC3)
0.279*(400-24)/400
0.346*(400-24)/400
0.375*(400-24)/400
levels(mydf2$AGE3)
levels(mydf2$RACE1)
levels(mydf2$GEND2)
levels(mydf2$STUDENT2)
levels(mydf2$WORKER)

# Weighting by...
# select control totals from population stats in Table 2
myip <- list(HTYPE2=c("Single-family"=0.739, "Multi-family"=0.261), 
             HHINC3=c("$35,000 to $74,999"=0.325, "Less than $35,000"=0.262, "$75,000 or more"=0.353, "Unknown"=0.060), 
             NTYPE2=c("URBAN0"=.400, "SUBRUR"=.600), 
             AGE3=c("18 to 34 years"=0.474, "35 to 54 years"=0.283, "55 years and over"=0.243), 
             RACE1=c("White-alone"=0.853, "Non-white/Multiple"=0.147), 
             GEND2=c("Male"=0.497, "Female"=0.503), 
             EDUC3=c("Bachelor or higher"=0.303, "Less than bachelor"=0.697), 
             STUDENT2=c("No"=0.785, "Yes"=0.215), 
             WORKER=c("Yes"=0.674, "No"=0.326))

# Weight dataset
tw <- anesrake(inputter=myip, dataframe=mydfhhper, caseid=mydfhhper$ID)
t1 <- data.frame(Weight=tw$weightvec, ID=tw$caseid)
mydfw <- merge(mydf2, t1, by="ID")
mydfw0 <- merge(mydfhhper, t1, by="ID")
hist(t1$Weight)
boxplot(t1$Weight, horizontal=T)
summary(t1$Weight)

# Remove
rm(mydfhh, mydfper, mydfhhper)
rm(myip, tw, t1)

########################################
# Add weather variables

# Load weather data
weath <- readRDS(file.path("Data", "Other", "cache_weather.rds"))
summary(weath)

# Merge
mydfw <- merge(mydfw, weath, by.x="DATE", by.y="Date", all.x=T, all.y=F)

# Descriptive statistics
table(mydfw$PRCP_CAT3)
prop.table(table(mydfw$PRCP_CAT3))
mean(mydfw$TMAX_DIFF)
sd(mydfw$TMAX_DIFF)

# Remove
rm(weath)

########################################
# Sample sizes

# Full data
length(unique(mydf$HHCODE))
length(unique(paste(mydf$PERCODE, mydf$HHCODE)))
# 403 adults from 230 households

# Model data
length(unique(mydfw$HHCODE))
length(unique(paste(mydfw$PERCODE, mydfw$HHCODE)))
# 390 adults from 223 households

########################################
# Descriptive statistics of IVs

# Define independent variables at HH-PER level
myivs <- c("HTYPE2", "HHINC3", "HHKIDS2", "HHNPER", "HHBIKES2", "HHNVEH", "NTYPE2", 
           "AGE3", "RACE1", "GEND2", "EDUC3", "STUDENT2", "WORKER")

# Subset data for descriptive statistics
mydfiv <- mydfw0[,names(mydfw0) %in% myivs]
mydfiva <- mydfiv[,which(unlist(lapply(mydfiv, class)) %in% c("integer", "numeric"))]
mydfivb <- mydfiv[,which(unlist(lapply(mydfiv, class)) %in% c("logical", "factor"))]
mydfivb <- dummy_cols(mydfivb, ignore_na=T, remove_selected_columns=T)
mydfiva$Weight <- mydfw0$Weight
mydfivb$Weight <- mydfw0$Weight

# Calculate descriptive statistics
tabdes <- data.frame(Type=character(), Var=character(), N=integer(), 
                     Freq=integer(), Perc=numeric(), Mean=numeric(), SD=numeric(), 
                     W.Freq=numeric(), W.Perc=numeric(), W.Mean=numeric())
for (i in names(mydfiva)) {
  td <- list("IV", i, sum(!is.na(mydfiva[,i])), 
             NA, NA, mean(mydfiva[,i], na.rm=T), sd(mydfiva[,i], na.rm=T), 
             NA, NA, weighted.mean(mydfiva[,i], w=mydfiva$Weight, na.rm=T))
  tabdes <- rbind(tabdes, td); rm(td)
}; rm(i)
for (i in names(mydfivb)) {
  td <- list("IV", i, sum(!is.na(mydfivb[,i])), 
             sum(mydfivb[,i], na.rm=T), 100 * sum(mydfivb[,i], na.rm=T) / nrow(mydfivb[!is.na(mydfivb[,i]),]), NA, NA, 
             sum(mydfivb$Weight[mydfivb[,i]==1], na.rm=T), 100 * sum(mydfivb$Weight[mydfivb[,i]==1], na.rm=T) / sum(mydfivb$Weight[!is.na(mydfivb[,i])]), NA)
  tabdes <- rbind(tabdes, td); rm(td)
}; rm(i)
names(tabdes) <- c("Type", "Var", "N", "Freq", "Perc", "Mean", "SD", "W.Freq", "W.Perc", "W.Mean")

# Remove
rm(myivs, mydfiv, mydfiva, mydfivb)

# Define independent variables at TRIP level
myivs <- c("PRCP_CAT3", "TMAX_DIFF", "AQI", "AQICAT", "RATE_AIRQUAL")

# Subset data for descriptive statistics
mydfiv <- mydfw[,names(mydfw) %in% myivs]
mydfiv$RATE_AIRQUAL <- as.integer(mydfiv$RATE_AIRQUAL)
mydfiva <- mydfiv[,which(unlist(lapply(mydfiv, class)) %in% c("integer", "numeric"))]
mydfivb <- mydfiv[,which(unlist(lapply(mydfiv, class)) %in% c("logical", "factor"))]
mydfivb <- dummy_cols(mydfivb, ignore_na=T, remove_selected_columns=T)
mydfiva$Weight <- mydfw$Weight
mydfivb$Weight <- mydfw$Weight

# Calculate descriptive statistics
for (i in names(mydfiva)) {
  td <- list("IV", i, sum(!is.na(mydfiva[,i])), 
             NA, NA, mean(mydfiva[,i], na.rm=T), sd(mydfiva[,i], na.rm=T), 
             NA, NA, weighted.mean(mydfiva[,i], w=mydfiva$Weight, na.rm=T))
  tabdes <- rbind(tabdes, td); rm(td)
}; rm(i)
for (i in names(mydfivb)) {
  td <- list("IV", i, sum(!is.na(mydfivb[,i])), 
             sum(mydfivb[,i], na.rm=T), 100 * sum(mydfivb[,i], na.rm=T) / nrow(mydfivb[!is.na(mydfivb[,i]),]), NA, NA, 
             sum(mydfivb$Weight[mydfivb[,i]==1], na.rm=T), 100 * sum(mydfivb$Weight[mydfivb[,i]==1], na.rm=T) / sum(mydfivb$Weight[!is.na(mydfivb[,i])]), NA)
  tabdes <- rbind(tabdes, td); rm(td)
}; rm(i)

# Remove
rm(myivs, mydfiv, mydfiva, mydfivb)

# Save
write.csv(tabdes, file.path("Analysis", "tabdes_ivs.csv"), row.names=F)

# Remove
rm(tabdes)

########################################
# Descriptive statistics of DVs

# Define dependent variables
mydvs <- c("STAYHOME", "ACT_TOT", "ACT_MAND", "ACT_SEMI", "ACT_DISC", 
           "MODE_TOT", "DIST_TOT", "TIME_TOT", 
           "NOMO_ACT", "NOMO_PUB", "NOMO_PRI", 
           "MODE_ACT", "DIST_ACT", "TIME_ACT", 
           "MODE_PUB", "DIST_PUB", "TIME_PUB", 
           "MODE_PRI", "DIST_PRI", "TIME_PRI")

# Subset data for descriptive statistics
mydfdv <- mydfw[,names(mydfw) %in% mydvs]
mydfdva <- mydfdv[,which(unlist(lapply(mydfdv, class)) %in% c("integer", "numeric"))]
mydfdvb <- mydfdv[,which(unlist(lapply(mydfdv, class)) %in% c("logical", "factor"))]
# mydfdvb <- dummy_cols(mydfdvb, ignore_na=T, remove_selected_columns=T)
mydfdva$Weight <- mydfw$Weight
mydfdvb$Weight <- mydfw$Weight

# Calculate descriptive statistics
tabdes <- data.frame(Type=character(), Var=character(), N=integer(), 
                     Freq=integer(), Perc=numeric(), Mean=numeric(), SD=numeric(), 
                     W.Freq=numeric(), W.Perc=numeric(), W.Mean=numeric())
for (i in names(mydfdva)) {
  td <- list("DV", i, sum(!is.na(mydfdva[,i])), 
             NA, NA, mean(mydfdva[,i], na.rm=T), sd(mydfdva[,i], na.rm=T), 
             NA, NA, weighted.mean(mydfdva[,i], w=mydfdva$Weight, na.rm=T))
  tabdes <- rbind(tabdes, td); rm(td)
}; rm(i)
for (i in names(mydfdvb)) {
  td <- list("DV", i, sum(!is.na(mydfdvb[,i])), 
             sum(mydfdvb[,i], na.rm=T), 100 * sum(mydfdvb[,i], na.rm=T) / nrow(mydfdvb[!is.na(mydfdvb[,i]),]), NA, NA, 
             sum(mydfdvb$Weight[mydfdvb[,i]==1], na.rm=T), 100 * sum(mydfdvb$Weight[mydfdvb[,i]==1], na.rm=T) / sum(mydfdvb$Weight[!is.na(mydfdvb[,i])]), NA)
  tabdes <- rbind(tabdes, td); rm(td)
}; rm(i)
names(tabdes) <- c("Type", "Var", "N", "Freq", "Perc", "Mean", "SD", "W.Freq", "W.Perc", "W.Mean")

# Remove
rm(mydvs, mydfdv, mydfdva, mydfdvb)

# Save
write.csv(tabdes, file.path("Analysis", "tabdes_dvs.csv"), row.names=F)

# Remove
rm(tabdes)

########################################
# Weighted models

# Decide format of air quality
taq <- "continuous"  # M0a
# taq <- "categorical" # M0b
# taq <- "subjective"  # M0c

# Define basic formula (this copied from models.R)
if (taq=="categorical") {
  tf <- formula(DV ~ HTYPE2+HHINC3+HHKIDS2+HHNPER+HHBIKES2+HHNVEH+AGE3+RACE1+GEND2+EDUC3+STUDENT2+WORKER+PRCP_CAT3+TMAX_DIFF+NTYPE2+NTYPE2:AQICAT)
} else if (taq=="subjective") {
  tf <- formula(DV ~ HTYPE2+HHINC3+HHKIDS2+HHNPER+HHBIKES2+HHNVEH+AGE3+RACE1+GEND2+EDUC3+STUDENT2+WORKER+PRCP_CAT3+TMAX_DIFF+NTYPE2+NTYPE2:I(as.integer(RATE_AIRQUAL)))
} else {
  tf <- formula(DV ~ HTYPE2+HHINC3+HHKIDS2+HHNPER+HHBIKES2+HHNVEH+AGE3+RACE1+GEND2+EDUC3+STUDENT2+WORKER+PRCP_CAT3+TMAX_DIFF+NTYPE2+NTYPE2:AQI)
}

# Stay home (overall)
sh <- glm(update(tf, STAYHOME ~ .), family=binomial("logit"), data=mydfw, weights=mydfw$Weight)
# Number of activities (total, by activity)
a0 <- glm(update(tf, ACT_TOT  ~ .), family=quasipoisson, data=mydfw, weights=mydfw$Weight)
a1 <- glm(update(tf, ACT_MAND ~ .), family=quasipoisson, data=mydfw, weights=mydfw$Weight)
a2 <- glm(update(tf, ACT_SEMI ~ .), family=quasipoisson, data=mydfw, weights=mydfw$Weight)
a3 <- glm(update(tf, ACT_DISC ~ .), family=quasipoisson, data=mydfw, weights=mydfw$Weight)
# Number of trips (total)
m0 <- glm(update(tf, MODE_TOT  ~ .), family=quasipoisson, data=mydfw, weights=mydfw$Weight)
# Distance traveled (total)
d0 <- lm(update(tf, log(DIST_TOT+1) ~ .), data=mydfw, weights=mydfw$Weight)
# Travel time (total)
t0 <- lm(update(tf, log(TIME_TOT+1) ~ .), data=mydfw, weights=mydfw$Weight)
# Used mode (by mode)
u1 <- glm(update(tf, I(NOMO_ACT==F) ~ .), family=binomial("logit"), data=mydfw, weights=mydfw$Weight)
u2 <- glm(update(tf, I(NOMO_PUB==F) ~ .), family=binomial("logit"), data=mydfw, weights=mydfw$Weight)
u3 <- glm(update(tf, I(NOMO_PRI==F) ~ .), family=binomial("logit"), data=mydfw, weights=mydfw$Weight)
# Number of trips (by mode)
m1 <- glm(update(tf, MODE_ACT  ~ .), family=quasipoisson, data=mydfw, weights=mydfw$Weight)
m2 <- glm(update(tf, MODE_PUB  ~ .), family=quasipoisson, data=mydfw, weights=mydfw$Weight)
m3 <- glm(update(tf, MODE_PRI  ~ .), family=quasipoisson, data=mydfw, weights=mydfw$Weight)
# Distance traveled (by mode)
d1 <- lm(update(tf, log(DIST_ACT+1) ~ .), data=mydfw, weights=mydfw$Weight)
d2 <- lm(update(tf, log(DIST_PUB+1) ~ .), data=mydfw, weights=mydfw$Weight)
d3 <- lm(update(tf, log(DIST_PRI+1) ~ .), data=mydfw, weights=mydfw$Weight)
# Travel time (by mode)
t1 <- lm(update(tf, log(TIME_ACT+1) ~ .), data=mydfw, weights=mydfw$Weight)
t2 <- lm(update(tf, log(TIME_PUB+1) ~ .), data=mydfw, weights=mydfw$Weight)
t3 <- lm(update(tf, log(TIME_PRI+1) ~ .), data=mydfw, weights=mydfw$Weight)

# Inspect models
summary(sh)
summary(a0)
summary(a1)
summary(a2)
summary(a3)
summary(m0)
summary(d0)
summary(t0)
summary(u1)
summary(u2)
summary(u3)
summary(m1)
summary(m2)
summary(m3)
summary(d1)
summary(d2)
summary(d3)
summary(t1)
summary(t2)
summary(t3)

# Save
mods <- list(mydf=mydfw, tf=tf, sh=sh, a0=a0, a1=a1, a2=a2, a3=a3, 
             m0=m0, d0=d0, t0=t0, u1=u1, u2=u2, u3=u3, 
             m1=m1, m2=m2, m3=m3, d1=d1, d2=d2, d3=d3, t1=t1, t2=t2, t3=t3)
if (taq=="categorical") {
  saveRDS(mods, file.path("Analysis", "mods0b.rds"))
} else if (taq=="subjective") {
  saveRDS(mods, file.path("Analysis", "mods0c.rds"))
} else {
  saveRDS(mods, file.path("Analysis", "mods0a.rds"))
}

# Remove
rm(sh, a0, a1, a2, a3, m0, d0, t0, u1, u2, u3, 
   m1, m2, m3, d1, d2, d3, t1, t2, t3, mods)
rm(taq, tf)

########################################
# Clean-up

# Remove
rm(mydf, mydf2, mydfw0, mydfw)
gc()

########################################
# END
########################################