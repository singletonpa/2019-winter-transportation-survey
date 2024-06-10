########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     geo_diary.R
# About:    Script to geocode diary surveys
########################################

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
# library("")

########################################
# Load data

# Load data
# Load diary, place, trip information
DIARY <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "DIARY.rds"))
PLACE <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PLACE.rds"))
TRIP <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "TRIP.rds"))
# load home, work, school locations
home <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "geo_home.rds"))
work <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "geo_work.rds"))
school <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "geo_school.rds"))
# load initial/final survey data
HH <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "HH.rds"))
PER <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PER.rds"))
PER_final <- readRDS(file=file.path("Data", "Survey 2019 Winter", "Data 3a Formatted", "PER_final.rds"))

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
# Geocode place locations

# Initialize
names(PLACE)
place <- PLACE[,c("ResponseId", "HHResponseId", "HHCODE", "PERCODE", "DIARY", "DATE", "PLANO", "PLACE", "PLACE_NAME", "PLACE_STREET", "PLACE_CITY")]
# place$PLOC0 <- paste(place$PLACE_STREET, place$PLACE_CITY, sep=", ")
# place$PLOC1 <- paste(place$PLACE_NAME, place$PLACE_STREET, place$PLACE_CITY, sep=", ")
place$PLOC0 <- paste(place$PLACE_NAME, place$PLACE_STREET, place$PLACE_CITY, sep=", ")
place$PLOC <- paste(place$PLACE_NAME, place$PLACE_STREET, place$PLACE_CITY, sep=", ")
place[,c("PLOC_LAT", "PLOC_LNG", "PLOC_ADD", "PLOC_PID")] <- NA
place$PLOC_APPROX <- F
str(place)

# Add PLOC information for Home, Work, School places
table(place$PLACE)
for (i in 1:nrow(place)) {
  if (!is.na(place$PLACE[i])) {
    if (place$PLACE[i]=="Home") {
      temp <- home[(home$HHCODE==place$HHCODE[i]), 
                   c("HLOC", "HLOC_LAT", "HLOC_LNG", "HLOC_ADD", "HLOC_PID", "HLOC_APPROX")]
      if (nrow(temp)==1) { place[i,c("PLOC", "PLOC_LAT", "PLOC_LNG", "PLOC_ADD", "PLOC_PID", "PLOC_APPROX")] <- temp }
      rm(temp)
    } else if (place$PLACE[i]=="Primary job") {
      temp <- work[(work$HHCODE==place$HHCODE[i] & work$PERCODE==place$PERCODE[i]), 
                   c("WORK", "WORK_LAT", "WORK_LNG", "WORK_ADD", "WORK_PID", "WORK_APPROX")]
      if (nrow(temp)==1) { place[i,c("PLOC", "PLOC_LAT", "PLOC_LNG", "PLOC_ADD", "PLOC_PID", "PLOC_APPROX")] <- temp }
      rm(temp)
    } else if (place$PLACE[i]=="School") {
      temp <- school[(school$HHCODE==place$HHCODE[i] & school$PERCODE==place$PERCODE[i]), 
                     c("SCHOOL", "SCHOOL_LAT", "SCHOOL_LNG", "SCHOOL_ADD", "SCHOOL_PID", "SCHOOL_APPROX")]
      if (nrow(temp)==1) { place[i,c("PLOC", "PLOC_LAT", "PLOC_LNG", "PLOC_ADD", "PLOC_PID", "PLOC_APPROX")] <- temp }
      rm(temp)
    }
  }
}; rm(i)

# Check matches (ignore missing for now)
# View(place[!is.na(place$PLACE) & place$PLACE=="Home", c("HHCODE", "PERCODE", "PLOC", "PLOC_ADD")])
# View(place[!is.na(place$PLACE) & place$PLACE=="Primary job", c("HHCODE", "PERCODE", "PLOC", "PLOC_ADD")])
# View(place[!is.na(place$PLACE) & place$PLACE=="School", c("HHCODE", "PERCODE", "PLOC", "PLOC_ADD")])
# looks okay

# Fix some home, work, school locations
# home
# - NOTE: code removed for confidentiality
# work
# - none
# school
# - none

# Geocode missing for Home
table(place$PLACE, is.na(place$PLOC_PID))
tw <- which(place$PLACE=="Home" & is.na(place$PLOC_PID))
length(tw)
# - none
# check
# View(place[tw,])
# View(place[tw,c("HHCODE", "PERCODE", "DATE", "PLANO", "PLOC", "PLOC_ADD")])
# couldn't match 0
rm(tw)

# Geocode missing for School
table(place$PLACE, is.na(place$PLOC_PID))
tw <- which(place$PLACE=="School" & is.na(place$PLOC_PID))
length(tw)
# fix missing/errors
# - some workers at USU or other schools put school for primary job
# - some people put school for other place dropoff/pickup children, guessed school
# - some people of college age didn't report school in initial survey, assume USU
usuloc <- "USU, Old Main Hill, Logan"
# - NOTE: code removed for confidentiality
# - example: place[place$HHCODE==1001 & place$PERCODE==1 & place$PLACE=="School", c("PLOC_APPROX","PLOC")] <- list(F,usuloc)
# - couldn't fix
# - NOTE: code removed for confidentiality
rm(usuloc)
# process
for (i in tw) {
  # get location
  tloc <- place$PLOC[i]
  # get geocode
  temp <- get_geo(tloc)
  # assign geocode
  place[i,c("PLOC_LAT", "PLOC_LNG", "PLOC_ADD", "PLOC_PID")] <- temp
  # remove
  rm(tloc, temp)
}; rm(i)
# check
# View(place[tw,])
# View(place[tw,c("HHCODE", "PERCODE", "DATE", "PLANO", "PLOC", "PLOC_ADD")])
# couldn't match 2
rm(tw)

# Geocode missing for Primary job
table(place$PLACE, is.na(place$PLOC_PID))
tw <- which(place$PLACE=="Primary job" & is.na(place$PLOC_PID))
length(tw)
# fix missing/errors
# - assume city hall / downtown for home city, unless other clues (e.g., travel time)
logloc <- "Logan city hall, Logan, UT"
# - NOTE: code removed for confidentiality
# - example: place[place$HHCODE==1001 & place$PERCODE==1 & place$PLACE=="Primary job", c("PLOC_APPROX","PLOC")] <- list(T,logloc)
# - couldn't fix
# - NOTE: code removed for confidentiality
rm(logloc)
# process
for (i in tw) {
  # get location
  tloc <- place$PLOC[i]
  # get geocode
  temp <- get_geo(tloc)
  # assign geocode
  place[i,c("PLOC_LAT", "PLOC_LNG", "PLOC_ADD", "PLOC_PID")] <- temp
  # remove
  rm(tloc, temp)
}; rm(i)
# check
# View(place[tw,])
# View(place[tw,c("HHCODE", "PERCODE", "DATE", "PLANO", "PLOC", "PLOC_ADD")])
# couldn't match 6
rm(tw)

# Geocode for Secondary job
table(place$PLACE, is.na(place$PLOC_PID))
tw <- which(place$PLACE=="Secondary job" & is.na(place$PLOC_PID))
length(tw)
# fix missing/errors
# - NOTE: code removed for confidentiality
# - example: place[place$HHCODE==1001 & place$PERCODE==1 & place$DATE=="2019-01-30" & place$PLANO==1, c("PLOC")] <- list("Main St & Center St, Logan, UT")
# - couldn't fix
# - NOTE: code removed for confidentiality
# process
for (i in tw) {
  # get location
  tloc <- place$PLOC[i]
  # get geocode
  temp <- get_geo(tloc)
  # assign geocode
  place[i,c("PLOC_LAT", "PLOC_LNG", "PLOC_ADD", "PLOC_PID")] <- temp
  # remove
  rm(tloc, temp)
}; rm(i)
# check
# View(place[tw,])
# View(place[tw,c("HHCODE", "PERCODE", "DATE", "PLANO", "PLOC", "PLOC_ADD")])
# couldn't match 1
rm(tw)

# Geocode for Bus stop or parking location
table(place$PLACE, is.na(place$PLOC_PID))
tw <- which(place$PLACE=="Bus stop or parking location (not at your destination)" & is.na(place$PLOC_PID))
length(tw)
# fix missing/errors
cvtdtc <- "CVTD, 150 E 500 N, Logan"
# - NOTE: code removed for confidentiality
# - example: place[place$HHCODE==1001 & place$PERCODE==1 & place$DATE=="2019-01-30" & place$PLANO==1, c("PLOC")] <- list(cvtdtc)
rm(cvtdtc)
# - couldn't fix
# - none
# process
for (i in tw) {
  # get location
  tloc <- place$PLOC[i]
  # get geocode
  temp <- get_geo(tloc)
  # assign geocode
  place[i,c("PLOC_LAT", "PLOC_LNG", "PLOC_ADD", "PLOC_PID")] <- temp
  # remove
  rm(tloc, temp)
}; rm(i)
# check
# View(place[tw,])
# View(place[tw,c("HHCODE", "PERCODE", "DATE", "PLANO", "PLOC", "PLOC_ADD")])
# couldn't match 0
rm(tw)

# Geocode for Other place
table(place$PLACE, is.na(place$PLOC_PID))
tw <- which(place$PLACE=="Other place" & is.na(place$PLOC_PID))
length(tw)
# fix missing/errors/schools
source(file.path("Analysis", "Scripts", "Geocoding", "geo_diary.addl.R"))
# process
for (i in tw) {
  # get location
  tloc <- place$PLOC[i]
  # get geocode
  temp <- get_geo(tloc)
  # assign geocode
  place[i,c("PLOC_LAT", "PLOC_LNG", "PLOC_ADD", "PLOC_PID")] <- temp
  # remove
  rm(tloc, temp)
}; rm(i)
# check
# View(place[tw,])
# View(place[tw,c("HHCODE", "PERCODE", "DATE", "PLANO", "PLOC", "PLOC_ADD")])
# couldn't match 5
rm(tw)

# Inspect
table(place$PLACE, is.na(place$PLOC_PID))
summary(place)

# Save
# write.csv(place, file="tplace.csv", row.names=F)
# saveRDS(place, file="tplace.rds")
saveRDS(place, file.path("Data", "geo_place.rds"))
# geocoded on 2023-02-17

########################################
# Cleanup

# Remove
rm(HH, PER, PER_final)
rm(home, work, school)
rm(DIARY, PLACE, TRIP)
rm(place)
rm(get_geo, apikey.geocoding)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################