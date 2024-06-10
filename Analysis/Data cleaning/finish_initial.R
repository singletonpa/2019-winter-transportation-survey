########################################
# Project:  2019-winter-transportation-survey
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     finish_initial.R
# About:    Script to finish initial survey
########################################

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
library("sf")
library("mapview")

########################################
# Load data

# Read rds data files
t_folder <- "Data 3a Formatted"
HH <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "HH.rds"))
PER <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "PER.rds"))
home <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "geo_home.rds"))
work <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "geo_work.rds"))
school <- readRDS(file=file.path("Data", "Survey 2019 Winter", t_folder, "geo_school.rds"))
rm(t_folder)

########################################
# Add info on census block group

# Load shapefiles
cbgut <- st_read(dsn=file.path("Data", "GIS", "Census Block Groups", "tl_2019_49_bg"), layer="tl_2019_49_bg", stringsAsFactors=F)
cbgid <- st_read(dsn=file.path("Data", "GIS", "Census Block Groups", "tl_2019_16_bg"), layer="tl_2019_16_bg", stringsAsFactors=F)

# Inspect
# mapview(cbgut) + mapview(cbgid)
# mapview(cbgut[cbgut$COUNTYFP=="005",]) + mapview(cbgid[cbgid$COUNTYFP=="041",])
str(cbgut); str(cbgid)
st_crs(cbgut); st_crs(cbgid)

# Merge, simplify
cbg <- rbind(cbgut, cbgid)
cbg <- cbg[,c("GEOID")]

# Create spatial: home
home[,c("LAT", "LNG")] <- home[,c("HLOC_LAT", "HLOC_LNG")]
home <- st_as_sf(home, crs=st_crs(cbgut), coords=c("LNG", "LAT"))
mapview(cbgut[cbgut$COUNTYFP=="005",]) + mapview(home, col.regions="orange")
names(cbg)[1] <- "HLOC_GEOID"
home <- st_join(home, cbg)
table(home$HLOC_GEOID); table(is.na(home$HLOC_GEOID))
home$geometry <- NULL

# Create spatial: work
work[,c("LAT", "LNG")] <- work[,c("WORK_LAT", "WORK_LNG")]
work <- st_as_sf(work, crs=st_crs(cbgut), coords=c("LNG", "LAT"))
mapview(cbgut[cbgut$COUNTYFP=="005",]) + mapview(work, col.regions="orange")
names(cbg)[1] <- "WORK_GEOID"
work <- st_join(work, cbg)
table(work$WORK_GEOID); table(is.na(work$WORK_GEOID))
work$geometry <- NULL

# Create spatial: school
school[,c("LAT", "LNG")] <- school[,c("SCHOOL_LAT", "SCHOOL_LNG")]
school <- st_as_sf(school, crs=st_crs(cbgut), coords=c("LNG", "LAT"))
mapview(cbgut[cbgut$COUNTYFP=="005",]) + mapview(school, col.regions="orange")
names(cbg)[1] <- "SCHOOL_GEOID"
school <- st_join(school, cbg)
table(school$SCHOOL_GEOID); table(is.na(school$SCHOOL_GEOID))
school$geometry <- NULL

# Cleanup
rm(cbg, cbgut, cbgid)

########################################
# Append geocoded location information

# HH
str(HH)
str(home)
home <- home[,c("ResponseId", "HHCODE", "HLOC_LAT", "HLOC_LNG", "HLOC_ADD", "HLOC_PID", "HLOC_GEOID", "HLOC_APPROX")]
HH <- merge(HH, home, by=c("ResponseId", "HHCODE"), all=T)
HH <- HH[order(HH$HHCODE),]
row.names(HH) <- NULL
str(HH); summary(HH)

# PER
str(PER)
str(work)
str(school)
work <- work[,c("ResponseId", "HHCODE", "PERCODE", "WORK_LAT", "WORK_LNG", "WORK_ADD", "WORK_PID", "WORK_GEOID", "WORK_APPROX")]
school <- school[,c("ResponseId", "HHCODE", "PERCODE", "SCHOOL_LAT", "SCHOOL_LNG", "SCHOOL_ADD", "SCHOOL_PID", "SCHOOL_GEOID", "SCHOOL_APPROX")]
PER <- merge(PER, work, by=c("ResponseId", "HHCODE", "PERCODE"), all=T)
PER <- merge(PER, school, by=c("ResponseId", "HHCODE", "PERCODE"), all=T)
PER <- PER[order(PER$HHCODE, PER$PERCODE),]
row.names(PER) <- NULL
str(PER); summary(PER)

########################################
# Save

# Save
t_folder <- "Data 3b Finished"
saveRDS(HH, file=file.path("Data", "Survey 2019 Winter", t_folder, "HH.rds"))
write.csv(HH, file=file.path("Data", "Survey 2019 Winter", t_folder, "HH.csv"), row.names=F)
saveRDS(PER, file=file.path("Data", "Survey 2019 Winter", t_folder, "PER.rds"))
write.csv(PER, file=file.path("Data", "Survey 2019 Winter", t_folder, "PER.csv"), row.names=F)
rm(t_folder)

########################################
# Cleanup

# Remove
rm(HH, PER)
rm(home, work, school)

# Cleanup
gc()
# cat("\014") #clear console in RStudio

########################################
# END
########################################