# desc_stat.R

# load data
HH <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "HH.rds"))
PER <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "PER.rds"))
VEH <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "VEH.rds"))
DIARY <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "DIARY.rds"))
PLACE <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "PLACE.rds"))
TRIP <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "TRIP.rds"))
PER_final <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "PER_final.rds"))
meta_initial <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "meta_initial.rds"))
meta_diary <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "meta_diary.rds"))
meta_final <- readRDS(file.path("Data", "Survey 2019 Winter", "Data 4 Anonymized", "meta_final.rds"))

# filter for incompletes and other removals
rem_initial <- meta_initial$ResponseId[meta_initial$Finished==F]
rem_diary1 <- meta_diary$ResponseId[meta_diary$Finished==F]
rem_diary2 <- DIARY$ResponseId[DIARY$REMOVE!=""]
rem_final <- meta_final$ResponseId[meta_final$Finished==F]
HH <- HH[!(HH$ResponseId %in% rem_initial),]
PER <- PER[!(PER$ResponseId %in% rem_initial),]
VEH <- VEH[!(VEH$ResponseId %in% rem_initial),]
DIARY <- DIARY[!(DIARY$ResponseId %in% rem_diary1),]
DIARY <- DIARY[!(DIARY$ResponseId %in% rem_diary2),]
PLACE <- PLACE[!(PLACE$ResponseId %in% rem_diary1),]
PLACE <- PLACE[!(PLACE$ResponseId %in% rem_diary2),]
TRIP <- TRIP[!(TRIP$ResponseId %in% rem_diary1),]
TRIP <- TRIP[!(TRIP$ResponseId %in% rem_diary2),]
PER_final <- PER_final[!(PER_final$ResponseId %in% rem_final),]
rm(rem_initial, rem_diary1, rem_diary2, rem_final)

# function to calculate descriptive statistics
library("fastDummies")
myds <- function(myname, myvar) {
  if (class(myvar) %in% c("integer", "numeric")) {
    td <- list(myname, length(na.omit(myvar)), NA, NA, mean(myvar, na.rm=T), sd(myvar, na.rm=T))
    td <- as.data.frame(td)
    names(td) <- c("Var", "N", "Freq", "Perc", "Mean", "SD")
  } else if (class(myvar) %in% c("logical")) {
    tdF <- list(paste(myname, "FALSE", sep="_"), length(na.omit(myvar)), sum(myvar==F, na.rm=T), 100 * sum(myvar==F, na.rm=T) / length(na.omit(myvar)), NA, NA)
    tdF <- as.data.frame(tdF)
    names(tdF) <- c("Var", "N", "Freq", "Perc", "Mean", "SD")
    tdT <- list(paste(myname, "TRUE", sep="_"), length(na.omit(myvar)), sum(myvar==T, na.rm=T), 100 * sum(myvar==T, na.rm=T) / length(na.omit(myvar)), NA, NA)
    tdT <- as.data.frame(tdT)
    names(tdT) <- c("Var", "N", "Freq", "Perc", "Mean", "SD")
    td <- rbind(tdF, tdT)
    rm(tdF, tdT)
  } else if (class(myvar) %in% c("factor")) {
    tvar <- data.frame(myvar); names(tvar) <- myname
    temp <- dummy_cols(tvar, ignore_na=T, remove_selected_columns=T)
    td <- list(names(temp)[1], length(na.omit(myvar)), sum(temp[,1], na.rm=T), 100 * sum(temp[,1], na.rm=T) / length(na.omit(myvar)), NA, NA)
    td <- as.data.frame(td)
    names(td) <- c("Var", "N", "Freq", "Perc", "Mean", "SD")
    for (j in 2:ncol(temp)) {
      xtd <- list(names(temp)[j], length(na.omit(myvar)), sum(temp[,j], na.rm=T), 100 * sum(temp[,j], na.rm=T) / length(na.omit(myvar)), NA, NA)
      xtd <- as.data.frame(xtd)
      names(xtd) <- c("Var", "N", "Freq", "Perc", "Mean", "SD")
      td <- rbind(td, xtd)
      rm(xtd)
    }; rm(j)
    rm(tvar, temp)
  } else {
    td <- list(myname, NA, NA, NA, NA, NA)
    td <- as.data.frame(td)
    names(td) <- c("Var", "N", "Freq", "Perc", "Mean", "SD")
  }
  row.names(td) <- NULL
  return(td)
}

# HH
# t1 <- HH[,-(which(unlist(lapply(HH, class))=="character"))]
t1 <- HH[,!(names(HH) %in% names(which(unlist(lapply(HH, class))=="character")))]
t1 <- t1[,-(which(names(t1) %in% c("HHCODE", "HLOC_LAT", "HLOC_LNG")))]
ds <- data.frame(Var=character(), N=integer(), Freq=integer(), Perc=numeric(), Mean=numeric(), SD=numeric())
for (i in 1:ncol(t1)) {
  ds <- rbind(ds, myds(names(t1)[i], t1[,i]))
}; rm(i)
write.csv(ds, file.path("Analysis", "ds_HH.csv"), row.names=F)
rm(t1, ds)

# PER
t1 <- PER[,!(names(PER) %in% names(which(unlist(lapply(PER, class))=="character")))]
t1 <- t1[,-(which(names(t1) %in% c("HHCODE", "PERCODE", "MORE", "WORK_LAT", "WORK_LNG", "SCHOOL_LAT", "SCHOOL_LNG")))]
ds <- data.frame(Var=character(), N=integer(), Freq=integer(), Perc=numeric(), Mean=numeric(), SD=numeric())
for (i in 1:ncol(t1)) {
  ds <- rbind(ds, myds(names(t1)[i], t1[,i]))
}; rm(i)
write.csv(ds, file.path("Analysis", "ds_PER.csv"), row.names=F)
rm(t1, ds)

# VEH
t1 <- VEH[,!(names(VEH) %in% names(which(unlist(lapply(VEH, class))=="character")))]
t1 <- t1[,-(which(names(t1) %in% c("HHCODE", "VEHCODE", "YEAR", "MORE")))]
ds <- data.frame(Var=character(), N=integer(), Freq=integer(), Perc=numeric(), Mean=numeric(), SD=numeric())
for (i in 1:ncol(t1)) {
  ds <- rbind(ds, myds(names(t1)[i], t1[,i]))
}; rm(i)
write.csv(ds, file.path("Analysis", "ds_VEH.csv"), row.names=F)
rm(t1, ds)

# DIARY
t1 <- DIARY[,!(names(DIARY) %in% names(which(unlist(lapply(DIARY, class))=="character")))]
t1 <- t1[,-(which(names(t1) %in% c("HHCODE", "PERCODE", "PERWHO", "DATE", "DATE_ORIG")))]
levels(t1$DIARY) <- c("DIARY12", "DIARY34", "DIARY34", "DIARY56", "DIARY56")
ds <- data.frame(Var=character(), N=integer(), Freq=integer(), Perc=numeric(), Mean=numeric(), SD=numeric())
for (i in 1:ncol(t1)) {
  ds <- rbind(ds, myds(names(t1)[i], t1[,i]))
}; rm(i)
write.csv(ds, file.path("Analysis", "ds_DIARY.csv"), row.names=F)
rm(t1, ds)

# PLACE
t1 <- PLACE[,!(names(PLACE) %in% names(which(unlist(lapply(PLACE, class))=="character")))]
t1 <- t1[,-(which(names(t1) %in% c("HHCODE", "PERCODE", "DIARY", "PLANO", "DATE", "PLOC_LAT", "PLOC_LNG")))]
t1 <- t1[,-(which(names(t1) %in% c("ARR_HOUR", "ARR_MIN", "ARR_AMPM", "ARR_TIME", "DEP_HOUR", "DEP_MIN", "DEP_AMPM", "DEP_TIME")))]
t1 <- t1[,-(which(names(t1) %in% c("MODE", "PEOPLE", "VEHICLE")))]
t1$ACT_TIME <- as.numeric(t1$ACT_TIME)
ds <- data.frame(Var=character(), N=integer(), Freq=integer(), Perc=numeric(), Mean=numeric(), SD=numeric())
for (i in 1:ncol(t1)) {
  ds <- rbind(ds, myds(names(t1)[i], t1[,i]))
}; rm(i)
write.csv(ds, file.path("Analysis", "ds_PLACE.csv"), row.names=F)
rm(t1, ds)

# TRIP
t1 <- TRIP[,!(names(TRIP) %in% names(which(unlist(lapply(TRIP, class))=="character")))]
t1 <- t1[,-(which(names(t1) %in% c("HHCODE", "PERCODE", "DIARY", "DATE")))]
t1 <- t1[,-(which(names(t1) %in% c("PLANO1", "PLACE1", "ACTIVITY1", "PLOC1_LAT", "PLOC1_LNG", "PLOC1_APPROX", "DEP_TIME")))]
t1 <- t1[,-(which(names(t1) %in% c("PLANO2", "PLACE2", "ACTIVITY2", "PLOC2_APPROX", "PLOC2_LAT", "PLOC2_LNG", "ARR_TIME")))]
t1$TRIP_TIME <- as.numeric(t1$TRIP_TIME)
ds <- data.frame(Var=character(), N=integer(), Freq=integer(), Perc=numeric(), Mean=numeric(), SD=numeric())
for (i in 1:ncol(t1)) {
  ds <- rbind(ds, myds(names(t1)[i], t1[,i]))
}; rm(i)
write.csv(ds, file.path("Analysis", "ds_TRIP.csv"), row.names=F)
rm(t1, ds)

# PER_final
t1 <- PER_final[,!(names(PER_final) %in% names(which(unlist(lapply(PER_final, class))=="character")))]
t1 <- t1[,-(which(names(t1) %in% c("HHCODE", "PERCODE")))]
ds <- data.frame(Var=character(), N=integer(), Freq=integer(), Perc=numeric(), Mean=numeric(), SD=numeric())
for (i in 1:ncol(t1)) {
  ds <- rbind(ds, myds(names(t1)[i], t1[,i]))
}; rm(i)
write.csv(ds, file.path("Analysis", "ds_PER_final.csv"), row.names=F)
rm(t1, ds)

# remove
rm(HH, PER, VEH)
rm(DIARY, PLACE, TRIP)
rm(PER_final)
rm(meta_initial, meta_diary, meta_final)
rm(myds)
gc()

# END