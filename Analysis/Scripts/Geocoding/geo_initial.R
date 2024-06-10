########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     geo_initial.R
# About:    Script to geocode initial survey
########################################

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
# library("")

########################################
# Load data

# Load data
load(file.path("Data", "Survey 2019 Winter", "Data 2 Cleaned", "initial.RData"))
load(file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "HH.RData"))
load(file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PER.RData"))

########################################
# Prepare geocoding stuff

# Resources
# https://cran.r-project.org/web/packages/ggmap/
# https://towardsdatascience.com/breaking-down-geocoding-in-r-a-complete-guide-1d0f8acd0d4b

# Load package
# install.packages(ggmap)
library(ggmap)

# Load API key
apikey.geocoding <- scan(file.path("Analysis", "Scripts", "Geocoding", "apikey.geocoding.txt"), what="character")

# Register API key
register_google(key=apikey.geocoding)

# Function to get geocoded information
get_geo <- function(tloc) {
  # geocode
  tgeo <- geocode(location=tloc, output="all", source="google")
  if ("results" %in% names(tgeo)) {
    # assemble results
    tlat <- tgeo$results[[1]]$geometry$location$lat
    tlng <- tgeo$results[[1]]$geometry$location$lng
    tadd <- tgeo$results[[1]]$formatted_address
    tpid <- tgeo$results[[1]]$place_id
    # return
    return(list(tlat, tlng, tadd, tpid))
  } else {
    return(list(NA, NA, NA, NA))
  }
}

# # Test
# tloc <- home$HLOC[1:10]
# tgeo1 <- geocode(location=tloc, output="more", source="google")
# tgeo2 <- geocode(location=tloc, output="all", source="google")
# rm(tgeo1, tgeo2)
# get_geo("Main St & Center St, Logan, 84321")

########################################
# Geocode home locations

# Initialize
names(HH)
home <- HH[,c("ResponseId", "HHCODE", "HLOC_STREET", "HLOC_CITY", "HLOC_ZIP")]
home$HLOC <- paste(home$HLOC_STREET, home$HLOC_CITY, home$HLOC_ZIP, sep=", ")
home[,c("HLOC_LAT", "HLOC_LNG", "HLOC_ADD", "HLOC_PID")] <- NA
home$HLOC_APPROX <- F
str(home)

# # Process
# for (i in 1:nrow(home)) {
#   # get location
#   tloc <- home$HLOC[i]
#   # get geocode
#   temp <- get_geo(tloc)
#   # assign geocode
#   home[i,c("HLOC_LAT", "HLOC_LNG", "HLOC_ADD", "HLOC_PID")] <- temp
#   # remove
#   rm(tloc, temp)
# }; rm(i)
# 
# # Inspect
# View(home[,c("HHCODE", "HLOC", "HLOC_ADD")])

# Missing
# - NOTE: code removed for confidentiality
# - example: assume Logan city hall

# Fix errors
# guesses
# - NOTE: code removed for confidentiality
# - example: home[home$HHCODE==1001, c("HLOC_APPROX", "HLOC")] <- list(T,"city hall, Logan, UT")
# general
# - NOTE: code removed for confidentiality
# - example: home$HLOC[home$HHCODE==1001] <- "Main St & Center St, Logan, 84321"

# # Check
# i <- which(home$HHCODE==1001)
# tloc <- home$HLOC[i]
# temp <- get_geo(tloc)
# tloc; temp[[3]]
# home[i,c("HLOC_LAT", "HLOC_LNG", "HLOC_ADD", "HLOC_PID")] <- temp
# rm(tloc, temp, i)

# Process
for (i in 1:nrow(home)) {
  # get location
  tloc <- home$HLOC[i]
  # get geocode
  temp <- get_geo(tloc)
  # assign geocode
  home[i,c("HLOC_LAT", "HLOC_LNG", "HLOC_ADD", "HLOC_PID")] <- temp
  # remove
  rm(tloc, temp)
}; rm(i)

# Check
# View(home[,c("HHCODE", "HLOC", "HLOC_ADD")])
for (i in 1:nrow(home)) {
  print(home$HHCODE[i])
  print(paste(home$HLOC_STREET[i], home$HLOC_CITY[i], home$HLOC_ZIP[i], sep=", "))
  print(home$HLOC_ADD[i])
  invisible(readline(prompt="Press [enter] to continue"))
}; rm(i)

# Inspect
# home <- readRDS(file.path("Data", "Survey 2019 Winter", "geo_home.rds"))
summary(home)

# Save
saveRDS(home, file.path("Data", "geo_home.rds"))
# geocoded on 2020-10-21

########################################
# Geocode work locations

# Initialize
names(PER)
work <- PER[,c("ResponseId", "HHCODE", "PERCODE", "WORKER", "WORK_NAME", "WORK_STREET", "WORK_CITY")]
work$WORK <- paste(work$WORK_NAME, work$WORK_STREET, work$WORK_CITY, sep=", ")
work <- work[!is.na(work$WORKER) & work$WORKER=="Yes", ]
work[,c("WORK_LAT", "WORK_LNG", "WORK_ADD", "WORK_PID")] <- NA
work$WORK_APPROX <- F
str(work)

# # Process
# for (i in 1:nrow(work)) {
#   # get location
#   tloc <- work$WORK[i]
#   # get geocode
#   temp <- get_geo(tloc)
#   # assign geocode
#   work[i,c("WORK_LAT", "WORK_LNG", "WORK_ADD", "WORK_PID")] <- temp
#   # remove
#   rm(tloc, temp)
# }; rm(i)
# 
# # Inspect
# View(work[,c("HHCODE", "PERCODE", "WORK", "WORK_ADD")])
# View(work[,c("HHCODE", "PERCODE", "WORK", "WORK_ADD", "WORK_LAT", "WORK_LNG")])

# Missing/unknown/multiple
# - NOTE: code removed for confidentiality
# - example: assume home or city hall for most
# HH[HH$HHCODE==1001,]
# PER[PER$HHCODE==1001 & PER$PERCODE==1,]
# PER[PER$WMODE_HOME==T,c("HHCODE", "PERCODE", "WORK_NAME")]
# tplace <- PLACE[PLACE$HHCODE==1001 & PLACE$PERCODE==1,]

# Fix errors
# guesses
# - NOTE: code removed for confidentiality
# - example: work[work$HHCODE==1001 & work$PERCODE==1, c("WORK_APPROX", "WORK")] <- list(T, "city hall, Logan, UT")
# general
# - NOTE: code removed for confidentiality
# - example: work$WORK[work$HHCODE==1001 & work$PERCODE==1] <- "Main St & Center St, Logan, UT"
# usu
usuloc <- "USU, Old Main Hill, Logan"
# - NOTE: code removed for confidentiality
# - example: work$WORK[work$HHCODE==1001 & work$PERCODE==1] <- usuloc
rm(usuloc)
# work from home
# - NOTE: code removed for confidentiality
# - example: work$WORK[work$HHCODE==1001 & work$PERCODE==1] <- home$HLOC[home$HHCODE==1001]

# # Check
# i <- which(work$HHCODE==1001 & work$PERCODE==1)
# tloc <- work$WORK[i]
# temp <- get_geo(tloc)
# tloc; temp[[1]]
# work[i,c("WORK_LAT", "WORK_LNG", "WORK_ADD", "WORK_PID")] <- temp
# rm(tloc, temp, i)

# Process
for (i in 1:nrow(work)) {
  # get location
  tloc <- work$WORK[i]
  # get geocode
  temp <- get_geo(tloc)
  # assign geocode
  work[i,c("WORK_LAT", "WORK_LNG", "WORK_ADD", "WORK_PID")] <- temp
  # remove
  rm(tloc, temp)
}; rm(i)

# Check
# View(work[,c("HHCODE", "PERCODE", "WORK", "WORK_ADD")])
for (i in 1:nrow(work)) {
  print(paste(work$HHCODE[i], work$PERCODE[i], sep=" "))
  print(paste(work$WORK_NAME[i], work$WORK_STREET[i], work$WORK_CITY[i], sep=", "))
  print(work$WORK_ADD[i])
  invisible(readline(prompt="Press [enter] to continue"))
}; rm(i)

# Inspect
# work <- readRDS(file.path("Data", "Survey 2019 Winter", "geo_work.rds"))
summary(work)

# Save
saveRDS(work, file.path("Data", "geo_work.rds"))
# geocoded on 2020-10-23
# edited on 2021-05-18

########################################
# Geocode school locations

# Initialize
names(PER)
school <- PER[,c("ResponseId", "HHCODE", "PERCODE", "STUDENT2", "SCHOOL_NAME", "SCHOOL_STREET", "SCHOOL_CITY")]
school$SCHOOL <- paste(school$SCHOOL_NAME, school$SCHOOL_STREET, school$SCHOOL_CITY, sep=", ")
school <- school[!is.na(school$STUDENT2) & school$STUDENT2=="Yes",]
school[,c("SCHOOL_LAT", "SCHOOL_LNG", "SCHOOL_ADD", "SCHOOL_PID")] <- NA
school$SCHOOL_APPROX <- F
str(school)

# # Process
# for (i in 1:nrow(school)) {
#   # get location
#   tloc <- school$SCHOOL[i]
#   # get geocode
#   temp <- get_geo(tloc)
#   # assign geocode
#   school[i,c("SCHOOL_LAT", "SCHOOL_LNG", "SCHOOL_ADD", "SCHOOL_PID")] <- temp
#   # remove
#   rm(tloc, temp)
# }; rm(i)
# 
# # Inspect
# View(school[,c("HHCODE", "PERCODE", "SCHOOL", "SCHOOL_ADD")])

# Missing/unknown
# - NOTE: code removed for confidentiality
# - example: local school based on age
# PER[PER$HHCODE==1001 & PER$PERCODE==1,]
# PER[PER$SMODE_HOME==T,c("HHCODE", "PERCODE", "SCHOOL_NAME")]

# Fix errors
# others
# - NOTE: code removed for confidentiality
# - example: school$SCHOOL[school$HHCODE==1001 & school$PERCODE==1] <- "Main St & Center St, Logan, UT 84321"
# usu
usuloc <- "USU, Old Main Hill, Logan"
# - NOTE: code removed for confidentiality
# - example: school$SCHOOL[school$HHCODE==1001 & school$PERCODE==1] <- usuloc
rm(usuloc)
# online --> school from home
# - NOTE: code removed for confidentiality
# - example: school$SCHOOL[school$HHCODE==1001 & school$PERCODE==1] <- home$HLOC[home$HHCODE==1001]

# # Check
# i <- which(school$HHCODE==1001 & school$PERCODE==1)
# tloc <- school$SCHOOL[i]
# temp <- get_geo(tloc)
# tloc; temp[[1]]
# school[i,c("SCHOOL_LAT", "SCHOOL_LNG", "SCHOOL_ADD", "SCHOOL_PID")] <- temp
# rm(tloc, temp, i)

# Process
for (i in 1:nrow(school)) {
  # get location
  tloc <- school$SCHOOL[i]
  # get geocode
  temp <- get_geo(tloc)
  # assign geocode
  school[i,c("SCHOOL_LAT", "SCHOOL_LNG", "SCHOOL_ADD", "SCHOOL_PID")] <- temp
  # remove
  rm(tloc, temp)
}; rm(i)

# Check
# View(school[,c("HHCODE", "PERCODE", "SCHOOL", "SCHOOL_ADD")])
for (i in 1:nrow(school)) {
  print(paste(school$HHCODE[i], school$PERCODE[i], sep=" "))
  print(paste(school$SCHOOL_NAME[i], school$SCHOOL_STREET[i], school$SCHOOL_CITY[i], sep=", "))
  print(school$SCHOOL_ADD[i])
  invisible(readline(prompt="Press [enter] to continue"))
}; rm(i)

# Inspect
# school <- readRDS(file.path("Data", "Survey 2019 Winter", "geo_school.rds"))
summary(school)

# Save
saveRDS(school, file.path("Data", "geo_school.rds"))
# geocoded on 2020-10-21

########################################
# Cleanup

# Remove
rm(initial, HH, PER)
rm(home, work, school)
rm(get_geo, apikey.geocoding)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################